submodule (h5mpi) utils_smod

use hdf5, only: h5get_libversion_f, &
h5eset_auto_f, &
h5iis_valid_f, h5iget_type_f, h5iget_name_f, &
h5open_f, h5close_f, &
h5fopen_f, h5fcreate_f, h5fclose_f, h5fis_hdf5_f, h5fget_filesize_f, &
h5fget_obj_count_f, h5fget_obj_ids_f, h5fget_name_f, &
h5sselect_hyperslab_f, h5screate_simple_f, &
h5dopen_f, h5dclose_f, h5dget_space_f, &
h5dget_create_plist_f, &
h5pcreate_f, h5pset_fapl_mpio_f, h5pall_filters_avail_f, h5pset_dxpl_mpio_f, h5pclose_f, &
H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
H5F_OBJ_FILE_F, H5F_OBJ_GROUP_F, H5F_OBJ_DATASET_F, H5F_OBJ_DATATYPE_F, H5F_OBJ_ALL_F, &
H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_COMPACT_F, &
H5FD_MPIO_COLLECTIVE_F, &
H5P_DATASET_XFER_F, H5P_FILE_ACCESS_F

use h5lt, only : h5ltget_dataset_ndims_f, h5ltget_dataset_info_f

implicit none (type, external)

contains

module procedure h5open

character(len=2) :: laction
integer :: ierr
integer(HID_T) :: fapl !< file access property list

if(self%is_open()) then
  write(stderr,*) 'h5fortran:open: file handle already open: '//self%filename
  return
endif

laction = "rw"
if (present(action)) laction = action

self%use_mpi = mpi

if(self%use_mpi) then
  call mpi_comm_rank(mpi_h5comm, self%mpi_id, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:open: could not get MPI ID"
endif

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
  write(stderr, '(a)') "ERROR:h5fortran:open: compression level must be >= 0, setting comp_lvl = 0"
  self%comp_lvl = 0
elseif(self%comp_lvl > 9) then
  write(stderr, '(a)') "ERROR:h5fortran:open: compression level must be <= 9, setting comp_lvl = 9"
  self%comp_lvl = 9
endif

call h5open_f(ierr)
if(ierr/=0) error stop "ERROR:h5fortran:h5open: could not open HDF5 library"
!! OK to call repeatedly
!! https://support.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Open

if(self%use_mpi) then
  !! collective: setup for MPI access
  call h5pcreate_f(H5P_FILE_ACCESS_F, fapl, ierr)
  if(ierr/=0) error stop "ERROR:h5fortran:open:h5pcreate could not collective open property for " // filename
  call h5pset_fapl_mpio_f(fapl, mpi_h5comm, mpi_h5info, ierr)
  if(ierr/=0) error stop "ERROR:h5fortran:open:h5pset_fapl_mpio could not collective open file for " // filename
else
  fapl = H5P_DEFAULT_F
endif

select case(laction)
case('r')
  if(.not. is_hdf5(filename)) error stop "ERROR:h5fortran:open: file does not exist: "//filename
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, self%file_id, ierr, access_prp=fapl)
case('r+')
  if(.not. is_hdf5(filename)) error stop "ERROR:h5fortran:open: file does not exist: "//filename
  call h5fopen_f(filename, H5F_ACC_RDWR_F, self%file_id, ierr, access_prp=fapl)
case('rw', 'a')
  if(is_hdf5(filename)) then
    call h5fopen_f(filename, H5F_ACC_RDWR_F, self%file_id, ierr, access_prp=fapl)
  else
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%file_id, ierr, access_prp=fapl)
  endif
case ('w')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%file_id, ierr, access_prp=fapl)
case default
  error stop 'ERROR:h5fortran:open Unsupported action ' // laction // ' for ' // filename
end select

if(ierr/=0) error stop "ERROR:h5open/create: could not initialize HDF5 file: " // filename // " action: " // laction

if(fapl /= H5P_DEFAULT_F) then
  call h5pclose_f(fapl, ierr)
  if(ierr/=0) error stop "ERROR:h5fortran:open:h5pclose: " // filename
endif

self%filename = filename

end procedure h5open


module procedure h5close

integer :: ierr, i
integer(SIZE_T) :: Ngroup, Ndset, Ndtype, Nfile, Lf_name, Lds_name
integer(HID_T), allocatable :: obj_ids(:)
integer(SIZE_T), parameter :: L = 2048 !< arbitrary length
character(L) :: file_name, dset_name

if (.not. self%is_open()) then
  write(stderr,*) 'WARNING:h5fortran:file_close: file handle is already closed: '// self%filename
  return
endif

!> ref count for better error messages, as this is more of a problem with HDF5-MPI programs
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
call h5fclose_f(self%file_id, ierr)
if (ierr /= 0) then
  write(stderr,'(a,i0)') 'ERROR:h5fortran:h5fclose: HDF5 file close: ' // self%filename // ' mpi_id: ', self%mpi_id
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ierr)
    if (ierr /= 0) error stop 'ERROR:h5fortran:h5close: HDF5 library close'
  endif
endif

end procedure h5close


module procedure is_open

! integer :: hid_type
integer :: ierr

call h5iis_valid_f(self%file_id, is_open, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:is_open:h5iis_valid: " // self%filename

! call h5iget_type_f(self%file_id, hid_type, ierr)
! if(ierr /= 0 .or. hid_type /= H5I_FILE_F) is_open = .false.

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


function id2name(id)
!! get name of object with given id

integer(HID_T) :: id
character(:), allocatable :: id2name

integer(SIZE_T) :: L
integer :: ierr

character(2048) :: name

call h5iget_name_f(id, name, len(name, SIZE_T), L, ierr)
if(ierr /= 0) error stop "h5fortran:id2name:h5iget_name"

id2name = name(:L)

end function id2name


module procedure mpi_hyperslab

integer(HSIZE_T), dimension(size(mem_dims)) :: c_mem_dims, i0
integer(HID_T) :: dcpl
integer :: ierr
logical :: filters_OK
character(:), allocatable :: dset_name

dset_name = id2name(dset_id)

!> check that all necessary filters to access dataset are available on the system.
call h5dget_create_plist_f(dset_id, dcpl, ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5dget_create_plist: " // dset_name

call h5pall_filters_avail_f(dcpl, filters_OK, ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5pall_filters_avail: " // dset_name
if (.not. filters_OK) then
  error stop "h5fortran: filter(s) missing necessary for dataset " // dset_name // " in parallel with MPI. This is " // &
    "typically caused by missing DEFLATE compression with HDF5-MPI."
endif

call h5pclose_f(dcpl, ierr)
if(ierr/=0) error stop "h5fortran:mpi_hyperslab:h5pclose: " // dset_name

if(filespace == H5S_ALL_F) then
  !> create dataspace
  call h5screate_simple_f(rank=size(dset_dims), dims=dset_dims, space_id=filespace, hdferr=ierr)
  if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5screate_simple:filespace " // dset_name
endif

!> Select hyperslab in the file.
call h5dget_space_f(dset_id, filespace, ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5dget_space: " // dset_name


! blk(1) = 1
! blk(2:) = dset_dims(2:)

!! NOTE: 0-based hyperslab vs. 1-based Fortran
i0 = istart - 1
c_mem_dims = iend - i0

if(any(c_mem_dims /= mem_dims)) then
  write(stderr,*) "ERROR:h5fortran:mpi_hyperslab: memory size /= dataset size: check variable slice (index). " // &
    " Dset_dims:", dset_dims, "C Mem_dims", c_mem_dims
  error stop "ERROR:h5fortran:mpi_hyperslab"
endif

! print *, 'TRACE:mpi_hyperslab: ' // dset_name //': istart', i0, 'C mem_dims: ', c_mem_dims, 'mem_dims', mem_dims

if(any(c_mem_dims < 1)) error stop "h5mpi:hyperslab:non-positive hyperslab: " // dset_name

call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
start=i0, &
count=c_mem_dims, &
hdferr=ierr)
! stride=1, &  !< for now we don't stride data
! block=blk  !< would this help performance?

if (ierr/=0) error stop "g5fortran:mpi_hyperslab:h5sselect_hyperslab: " // dset_name

!> create memory dataspace
call h5screate_simple_f(rank=size(c_mem_dims), dims=c_mem_dims, space_id=memspace, hdferr=ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5screate_simple:memspace " // dset_name

end procedure mpi_hyperslab


module procedure mpi_collective

integer :: ierr

!! Create property list for collective dataset operations
call h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, ierr)
if (ierr/=0) error stop "h5pcreate dataset xfer: " // dname

call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ierr)
if (ierr/=0) error stop "h5pset_dxpl_mpio collective: " // dname

! For independent dataset operations
! call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_INDEPENDENT_F, ierr)

end procedure mpi_collective


module procedure hdf_rank_check

integer(HSIZE_T) :: ddims(1)
integer(SIZE_T) :: type_size
integer :: ierr, drank, type_class

if(present(vector_scalar)) vector_scalar = .false.

if(.not.self%is_open()) error stop 'ERROR:h5fortran:rank_check: file handle is not open: ' // self%filename

if (.not.self%exist(dname)) error stop 'ERROR::h5fortran:rank_check: ' // dname // ' does not exist in ' // self%filename

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ierr)
if (ierr/=0) error stop 'h5fortran:rank_check: get_dataset_ndim ' // dname // ' read ' // self%filename

if (drank == mrank) return

if (present(vector_scalar) .and. drank == 1 .and. mrank == 0) then
  !! check if vector of length 1
  call h5ltget_dataset_info_f(self%file_id, dname, dims=ddims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
  if (ierr/=0) error stop 'h5fortran:rank_check: get_dataset_info ' // dname // ' read ' // self%filename
  if (ddims(1) == 1) then
    vector_scalar = .true.
    return
  endif
endif

write(stderr,'(A,I0,A,I0)') 'h5fortran:rank_check: rank mismatch ' // dname // ' = ',drank,'  variable rank =', mrank
error stop

end procedure hdf_rank_check


module procedure hdf_shape_check

integer :: ierr, type_class
integer(SIZE_T) :: type_size
integer(HSIZE_T) :: dsdims(size(dims))

call hdf_rank_check(self, dname, size(dims))

!> check for matching size, else bad reads can occur.

call h5ltget_dataset_info_f(self%file_id, dname, dims=dsdims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
if (ierr/=0) error stop 'ERROR:h5fortran:shape_check: get_dataset_info ' // dname // ' read ' // self%filename

if(present(dset_dims)) dset_dims = dsdims

if(self%use_mpi) return

if(any(int(dims, int64) /= dsdims)) then
  write(stderr,*) 'ERROR:h5fortran:shape_check: shape mismatch ' // dname // ' = ', dsdims, '  variable shape =', dims
  error stop
endif

end procedure hdf_shape_check


module procedure hdf_filesize

integer :: ierr

if(.not. self%is_open()) error stop 'h5fortran:filesize: file handle is not open: ' // self%filename

call h5fget_filesize_f(self%file_id, hdf_filesize, ierr)
if(ierr/=0) error stop "ERROR:h5fortran: could not get file size " // self%filename

end procedure hdf_filesize


end submodule utils_smod
