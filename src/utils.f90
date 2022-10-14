submodule (h5fortran) utils_smod

use hdf5, only: h5get_libversion_f, &
h5eset_auto_f, &
h5iis_valid_f, h5iget_name_f, H5Iget_type_f, &
h5open_f, h5close_f, &
H5Dget_create_plist_f, &
h5fopen_f, h5fcreate_f, h5fclose_f, h5fis_hdf5_f, h5fget_filesize_f, &
h5fget_obj_count_f, h5fget_obj_ids_f, h5fget_name_f, &
H5Pall_filters_avail_f, H5Pclose_f, &
h5sselect_hyperslab_f, h5screate_simple_f, &
H5Sget_simple_extent_ndims_f, H5Sget_simple_extent_dims_f, H5Sget_simple_extent_npoints_f, &
H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
H5F_OBJ_FILE_F, H5F_OBJ_GROUP_F, H5F_OBJ_DATASET_F, H5F_OBJ_DATATYPE_F, H5F_OBJ_ALL_F, &
H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_COMPACT_F, &
H5I_FILE_F, &
H5S_SELECT_SET_F

implicit none (type, external)

contains

module procedure id2name

integer(SIZE_T) :: L
integer :: ierr

character(2048) :: name

call h5iget_name_f(id, name, len(name, SIZE_T), L, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:id2name:h5iget_name"

id2name = name(:L)

end procedure id2name


module procedure h5open

character(2) :: laction
integer :: ier
integer(HID_T) :: fapl !< file access property list
integer :: file_mode

if(self%is_open()) then
  write(stderr,*) 'h5fortran:open: file handle already open: '//self%filename
  return
endif

laction = 'r'
if (present(action)) laction = action

self%filename = filename

if(present(mpi)) self%use_mpi = mpi

if(present(debug)) self%debug = debug

call get_hdf5_config(self%parallel_compression)
if(self%use_mpi .and. .not. self%parallel_compression .and. self%comp_lvl > 0) then
  write(stderr, '(a)') "WARNING:h5fortran:open: parallel compression is NOT available"
  !! don't set to 0 because non-MPI writes can compress.
  !! We warn again and disable compression for each attempted MPI compress write.
endif

!> compression parameter
if(present(comp_lvl) .and. laction /= "r") self%comp_lvl = comp_lvl
if(self%comp_lvl > 0) then
  self%shuffle = .true.
  self%fletcher32 = .true.
endif

if(present(shuffle)) self%shuffle = shuffle
if(present(fletcher32)) self%fletcher32 = fletcher32

if(self%comp_lvl < 0) then
  write(stderr, '(a)') "NOTICE:h5fortran:open: compression level must be >= 0, setting comp_lvl = 0"
  self%comp_lvl = 0
elseif(self%comp_lvl > 9) then
  write(stderr, '(a)') "NOTICE:h5fortran:open: compression level must be <= 9, setting comp_lvl = 9"
  self%comp_lvl = 9
endif

!> Initialize FORTRAN interface.
call h5open_f(ier)
if (ier /= 0) error stop 'ERROR:h5fortran:open: HDF5 library initialize'

if(self%debug) then
  call h5eset_auto_f(1, ier)
else
  call h5eset_auto_f(0, ier)
endif
if (ier /= 0) error stop 'ERROR:h5fortran:open: HDF5 library set traceback'

select case(laction)
case('r')
  file_mode = H5F_ACC_RDONLY_F
case('r+')
  file_mode = H5F_ACC_RDWR_F
case('rw', 'a')
  if(is_hdf5(filename)) then
    file_mode = H5F_ACC_RDWR_F
  else
    file_mode = H5F_ACC_TRUNC_F
  endif
case ('w')
  file_mode = H5F_ACC_TRUNC_F
case default
  error stop 'ERROR:h5fortran:open Unsupported action ' // laction // ' for ' // filename
end select

call mpi_opener(filename, self%use_mpi, self%mpi_id, fapl)

if (file_mode == H5F_ACC_RDONLY_F .or. file_mode == H5F_ACC_RDWR_F) then
  if(.not. is_hdf5(filename)) error stop "ERROR:h5fortran:open: not an HDF5 file: "//filename
  call H5Fopen_f(filename, file_mode, self%file_id, ier, access_prp=fapl)
  if (ier /= 0) error stop "ERROR:h5fortran:open:H5Fopen: " // filename
elseif(file_mode == H5F_ACC_TRUNC_F) then
  call H5Fcreate_f(filename, file_mode, self%file_id, ier, access_prp=fapl)
  if (ier /= 0) error stop "ERROR:h5fortran:open:H5Fcreate: " // filename
else
  error stop "ERROR:h5fortran:open: Unsupported file mode: " // filename
endif

if(fapl /= H5P_DEFAULT_F) then
  call h5pclose_f(fapl, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:open:h5pclose: " // filename
endif

end procedure h5open


module procedure h5close

integer :: ierr, i
integer(SIZE_T) :: Ngroup, Ndset, Ndtype, Nfile, Lf_name, Lds_name
integer(HID_T), allocatable :: obj_ids(:)
integer(SIZE_T), parameter :: L = 2048 !< arbitrary length
character(L) :: file_name, dset_name

if (.not. self%is_open()) then
  write(stderr,*) 'WARNING:h5fortran:close: file handle is already closed: '// self%filename
  return
endif

!> ref count for better error messages; this is more of a problem with HDF5-MPI programs
call h5fget_obj_count_f(self%file_id, H5F_OBJ_GROUP_F, Ngroup, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open groups: " // self%filename
if(Ngroup > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ngroup, " groups open: " // self%filename


call h5fget_obj_count_f(self%file_id, H5F_OBJ_DATASET_F, Ndset, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datasets: " // self%filename
if(Ndset > 0) then
  write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndset, " datasets open: " // self%filename

  allocate(obj_ids(Ndset))
  call h5fget_obj_ids_f(self%file_id, H5F_OBJ_DATASET_F, Ndset, obj_ids, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_ids: could not get open dataset ids: " // self%filename

  do i = 1, int(Ndset)
    call h5fget_name_f(obj_ids(i), file_name, Lf_name, ierr)
    if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_name: could not get filename of open dataset: " // self%filename

    call h5iget_name_f(obj_ids(i), dset_name, L, Lds_name, ierr)
    if(ierr /= 0) error stop "ERROR:h5fortran:close:h5iget_name could not get dataset name: " // self%filename

    write(stderr,*) "h5fortran:close: open dataset: " // dset_name(:Lds_name) // " in file: " // file_name(:Lf_name)
  end do
endif

call h5fget_obj_count_f(self%file_id, H5F_OBJ_DATATYPE_F, Ndtype, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datatypes: " // self%filename
if(Ndtype > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndtype, " datatypes open: " // self%filename

call h5fget_obj_count_f(self%file_id, H5F_OBJ_FILE_F, Nfile, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open files: " // self%filename
if(Nfile < 1) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Nfile, " files open: " // self%filename

if(Ngroup > 0 .or. Ndset > 0 .or. Ndtype > 0) error stop "ERROR:h5fortran:close: hanging HID handles open: " // self%filename


!> close hdf5 file
call H5Fclose_f(self%file_id, ierr)
if (ierr /= 0) then
  write(stderr,'(a,i0)') 'ERROR:h5fortran:H5Fclose: HDF5 file close: ' // self%filename // ' mpi_id: ', self%mpi_id
  error stop
endif

deallocate(self%filename)

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ierr)
    if (ierr /= 0) error stop 'ERROR:h5fortran:h5close: HDF5 library close'
  endif
endif

end procedure h5close


module procedure is_open

integer :: obj_type, ier

call H5Iis_valid_f(self%file_id, is_open, ier)
if(ier /= 0) error stop "ERROR:h5fortran:is_open:h5iis_valid: " // self%filename

call H5Iget_type_f(self%file_id, obj_type, ier)
if(ier /= 0 .or. obj_type /= H5I_FILE_F) is_open = .false.

end procedure is_open


module procedure has_parallel_compression
call get_hdf5_config(has_parallel_compression)
end procedure has_parallel_compression


module procedure destructor

if (.not. self%is_open()) return

print '(a)', "auto-closing " // self%filename
call self%close()

end procedure destructor


module procedure hdf5version

integer :: ierr

!> get library version
call h5get_libversion_f(v(1), v(2), v(3), ierr)
if (ierr/=0) error stop 'ERROR:h5fortran: HDF5 library get version'

if ((v(2) == 10 .and. v(3) < 2) .or. v(2) < 10) then
  write(stderr,'(a,I0,a1,I0,a1,I0,/,a)') "WARNING: HDF5 >= 1.10.2 required for MPI-HDF5. Your HDF5 version: ", &
  v(1), ".", v(2), ".", v(3), &
  "https://www.hdfgroup.org/2018/03/release-of-hdf5-1-10-2-newsletter-160/"
end if

end procedure hdf5version


module procedure hdf5_close

integer :: ier

call h5close_f(ier)
if (ier /= 0) error stop 'ERROR: h5fortran:h5close: HDF5 library close'

end procedure hdf5_close


module procedure hdf_is_contig
hdf_is_contig = self%layout(dname) == H5D_CONTIGUOUS_F
end procedure hdf_is_contig

module procedure hdf_is_compact
hdf_is_compact = self%layout(dname) == H5D_COMPACT_F
end procedure hdf_is_compact

module procedure hdf_is_chunked
hdf_is_chunked = self%layout(dname) == H5D_CHUNKED_F
end procedure hdf_is_chunked


module procedure is_hdf5
integer :: ierr

inquire(file=filename, exist=is_hdf5)
!! avoid warning/error messages
if (.not. is_hdf5) return

call h5fis_hdf5_f(filename, is_hdf5, ierr)

if (ierr/=0) is_hdf5 = .false.
!! sometimes h5fis_hdf5_f is .true. for missing file

end procedure is_hdf5


subroutine check_filters(dset_name, dset_id)
!! check that all necessary filters to access dataset are available on the system.

character(*), intent(in) :: dset_name
integer(HID_T), intent(in) :: dset_id

integer(HID_T) :: pid
integer :: ier
logical :: ok

call H5Dget_create_plist_f(dset_id, pid, ier)
if (ier /= 0) error stop "ERROR:h5fortran:check_filters:H5Dget_create_plist: " // dset_name

call H5Pall_filters_avail_f(pid, ok, ier)
if (ier /= 0) error stop "ERROR:h5fortran:check_filters:H5Pall_filters_avail: " // dset_name

if (.not. ok) then
  error stop "ERROR:h5fortran: filter(s) missing necessary for dataset " // dset_name // " in parallel with MPI. This is " // &
    "typically caused by missing DEFLATE compression with HDF5-MPI."
endif

call H5Pclose_f(pid, ier)
if(ier /= 0) error stop "ERROR:h5fortran:check_filters:H5Pclose: " // dset_name

end subroutine check_filters


module procedure hdf_get_slice

integer(HSIZE_T), dimension(size(istart)) :: c_mem_dims, i0, istride
integer(HSIZE_T), dimension(:), allocatable :: ddims, maxdims
integer :: ier, drank
character(:), allocatable :: dset_name

dset_name = id2name(dset_id)

call check_filters(dset_name, dset_id)

if(present(dset_dims)) then
  ddims = dset_dims
else
  call H5Sget_simple_extent_ndims_f(filespace_id, drank, ier)
  if(ier /= 0) error stop "ERROR:h5fortran:get_slice: H5Sget_simple_extent_ndims: " // dset_name

  allocate(ddims(drank), maxdims(drank))

  call H5Sget_simple_extent_dims_f(filespace_id, ddims, maxdims, ier)
  if (ier /= drank) error stop 'ERROR:h5fortran:get_slice:H5Sget_simple_extent_dims: ' // dset_name
endif

istride = 1
if(present(stride)) istride = int(stride, HSIZE_T)

!! NOTE: 0-based hyperslab vs. 1-based Fortran
i0 = istart - 1
c_mem_dims = iend - i0

if(size(mem_dims) == 0) then
  !! rank(dims(0)) == 1, but size(dims(0)) == 0
  if (sum(c_mem_dims) /= 1) error stop "ERROR:h5fortran:hdf_get_slice: scalar index of array failed " // dset_name
elseif(any(c_mem_dims /= mem_dims)) then
  write(stderr,*) "ERROR:h5fortran:get_slice: memory size /= dataset size: check variable slice (index). " // &
    " Dset_dims:", ddims, "C Mem_dims:", c_mem_dims, "mem_dims:", mem_dims, "rank(mem_dims):", rank(mem_dims)
  error stop "ERROR:h5fortran:get_slice " // dset_name
elseif(any(iend-1 > ddims)) then
  write(stderr,*) "ERROR:h5fortran:get_slice: iend: ", iend, ' > dset_dims: ', ddims
  error stop "ERROR:h5fortran:get_slice " // dset_name
endif

! print *, 'TRACE:hdf_get_slice: ' // dset_name //': istart', i0, 'C mem_dims: ', c_mem_dims, 'mem_dims', mem_dims

if(any(c_mem_dims < 1)) error stop "ERROR:h5fortran:get_slice:non-positive hyperslab: " // dset_name

call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, &
start=i0, &
stride=istride, &
count=c_mem_dims, &
hdferr=ier)
! block=blk  !< would this help performance?
if (ier /= 0) error stop "ERROR:h5fortran:get_slice:h5sselect_hyperslab: " // dset_name

!> create memory dataspace
!! H5Dread needs this for non-scalar
call h5screate_simple_f(rank=size(c_mem_dims), dims=c_mem_dims, space_id=memspace_id, hdferr=ier)
if (ier /= 0) error stop "ERROR:h5fortran:get_slice:h5screate_simple:memspace " // dset_name

end procedure hdf_get_slice


module procedure hdf_rank_check

integer(HSIZE_T) :: N
integer :: ierr, drank

if(present(is_scalar)) is_scalar = .false.

call H5Sget_simple_extent_ndims_f(file_space_id, drank, ierr)
if (ierr/=0) error stop 'ERROR:h5fortran:rank_check:H5Sget_simple_extent_ndims: ' // obj_name // ' in ' // self%filename

if (drank == mrank) return

if (present(is_scalar)) then
  !! check if single element
  call H5Sget_simple_extent_npoints_f(file_space_id, N, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:rank_check:H5Sget_simple_extent_npoints: ' // obj_name // ' in ' // self%filename

  is_scalar = N == 1
  if(is_scalar) return
endif

write(stderr,'(A,I0,A,I0)') 'ERROR:h5fortran:rank_check: rank mismatch ' // obj_name // ' = ', drank,'  variable rank =', mrank
error stop

end procedure hdf_rank_check


module procedure hdf_shape_check

integer :: ierr
integer(HSIZE_T), dimension(size(dims)):: ddims, maxdims

call hdf_rank_check(self, dname, file_space_id, size(dims))

call H5Sget_simple_extent_dims_f(file_space_id, ddims, maxdims, ierr)
if (ierr /= size(dims)) error stop 'ERROR:h5fortran:rank_check:H5Sget_simple_extent_dims: ' // dname // ' in ' // self%filename

if(self%use_mpi) return

if(any(int(dims, int64) /= ddims)) then
  write(stderr,*) 'ERROR:h5fortran:shape_check: shape mismatch ' // dname // ' = ',ddims,'  variable shape =', dims
  error stop
endif

end procedure hdf_shape_check


module procedure hdf_filesize

integer :: ierr

if(.not. self%is_open()) error stop 'ERROR:h5fortran:filesize: file handle is not open: ' // self%filename

call h5fget_filesize_f(self%file_id, hdf_filesize, ierr)
if(ierr/=0) error stop "ERROR:h5fortran: could not get file size " // self%filename

end procedure hdf_filesize


end submodule utils_smod
