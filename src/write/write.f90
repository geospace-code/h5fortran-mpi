submodule (h5mpi) write

use hdf5, only : h5pset_deflate_f, h5pset_fletcher32_f, h5pset_shuffle_f, &
h5dwrite_f

implicit none (type, external)

contains


module procedure hdf_create

logical :: exists
integer :: ierr
integer(HID_T) :: plist_id, ds_id

plist_id = H5P_DEFAULT_F
memspace = H5S_ALL_F

if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open: ' // self%filename

call h5ltpath_valid_f(self%file_id, dname, .true., exists, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid
!! stricter than self%exists() since we're creating and/or writing variable
if (ierr /= 0) error stop 'ERROR:h5fortran:create: variable path invalid: ' // dname // ' in ' // self%filename

if(self%debug) print *,'h5fortran:TRACE:create:exists: ' // dname, exists

if(exists) then
  if (.not.present(istart)) then
    if (size(mem_dims) == 0) then
      !! scalar
      call hdf_rank_check(self, dname, size(mem_dims))
    else
      call hdf_shape_check(self, dname, mem_dims)
    endif
  endif
  !! FIXME: read and write slice shape not checked; but should check in future versions

  !> open dataset
  call h5dopen_f(self%file_id, dname, ds_id, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:create: could not open ' // dname // ' in ' // self%filename

  if(present(dset_id)) dset_id = ds_id
  if(present(filespace)) then
    call h5dget_space_f(ds_id, filespace, ierr)
    if(ierr /= 0) error stop 'h5fortran:create could not get dataset ' // dname // ' in ' // self%filename
  end if
  return
endif

!> compression
if(size(mem_dims) >= 2) then
  call set_deflate(self, mem_dims, plist_id, ierr, chunk_size)
  if (ierr/=0) error stop 'ERROR:h5fortran:create: problem setting deflate on ' // dname // ' in ' // self%filename
endif

!> create dataset dataspace
call h5screate_simple_f(rank=size(dset_dims), dims=dset_dims, space_id=filespace, hdferr=ierr)
if (ierr/=0) error stop "h5screate_simple:filespace " // dname // " " // self%filename

!> create dataset
call h5dcreate_f(self%file_id, dname, dtype, space_id=filespace, dset_id=dset_id, hdferr=ierr, dcpl_id=plist_id)
if (ierr/=0) error stop "h5fortran:h5dcreate: " // dname // " " // self%filename

call h5pclose_f(plist_id, ierr)
if (ierr/=0) error stop "h5pclose: " // dname // ' in ' // self%filename

end procedure hdf_create


subroutine set_deflate(self, dims, plist_id, ierr, chunk_size)
class(hdf5_file), intent(inout) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: plist_id
integer, intent(out) :: ierr
integer, intent(in), optional :: chunk_size(:)

integer(HSIZE_T) :: cs(size(dims))

ierr = 0
plist_id = H5P_DEFAULT_F

if (present(chunk_size)) then
  cs = chunk_size
  where (cs > dims) cs = dims
  if(self%debug) print *,'TRACE: user request chunk_size ',cs
elseif (self%comp_lvl < 1 .or. self%comp_lvl > 9) then
  ! didn't request chunk_size and didn't request compression
  return
else
  !! guess chunk size, keeping in mind 1 Megabyte recommended maximum chunk size
  call guess_chunk_size(dims, cs)
endif

if(any(cs < 1)) return

if(self%debug) print *,'DEBUG:set_deflate: dims: ',dims,'chunk size: ', cs

call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, ierr)
if (check(ierr, "h5pcreate: " // self%filename)) return

call h5pset_chunk_f(plist_id, size(dims), cs, ierr)
if (check(ierr, "h5pset_chunk: " // self%filename)) return

if (self%use_mpi .and. .not. self%parallel_compression) then
  write(stderr, '(a)') 'h5fortran:set_deflate: parallel filters (compression) not supported'
  return
endif

if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return

!> enable filters

call h5pset_shuffle_f(plist_id, ierr)
if (check(ierr, "h5pset_shuffle: " // self%filename)) return

call h5pset_fletcher32_f(plist_id, ierr)
if (check(ierr, "h5pset_fletcher32: " // self%filename)) return

call h5pset_deflate_f(plist_id, self%comp_lvl, ierr)
if (check(ierr, "h5pset_deflate: " // self%filename)) return

if(self%debug) print '(a,i0)','TRACE:set_deflate done, comp_lvl: ', self%comp_lvl

end subroutine set_deflate


subroutine guess_chunk_size(dims, chunk_size)
!! based on https://github.com/h5py/h5py/blob/master/h5py/_hl/filters.py
!! refer to https://support.hdfgroup.org/HDF5/Tutor/layout.html
integer(HSIZE_T), intent(in) :: dims(:)
integer(HSIZE_T), intent(out) :: chunk_size(:)

integer(hsize_t), parameter :: &
CHUNK_BASE = 16000, &    !< Multiplier by which chunks are adjusted
CHUNK_MIN = 8000, &      !< lower limit: 8 kbyte
CHUNK_MAX = 1000000, &   !< upper limit: 1 Mbyte
TYPESIZE = 8             !< bytes, assume real64 for simplicity

integer(hsize_t) :: dset_size, target_size, chunk_bytes, i, j, ndims

if (product(dims) * TYPESIZE < CHUNK_MIN) then
  chunk_size = 0
  return
endif

ndims = size(chunk_size)
chunk_size = dims

dset_size = product(chunk_size) * TYPESIZE
target_size = int(CHUNK_BASE * (2**log10(real(dset_size) / 1e6)), hsize_t)
if (target_size > CHUNK_MAX) target_size = CHUNK_MAX

! print *,'target_size [bytes]: ',target_size

i = 0
do
  !! Repeatedly loop over the axes, dividing them by 2.
  !! Stop when:
  !!   1a. We're smaller than the target chunk size, OR
  !!   1b. We're within 50% of the target chunk size, AND
  !!    2. The chunk is smaller than the maximum chunk size

  chunk_bytes = product(chunk_size) * TYPESIZE

  if ((chunk_bytes < target_size .or. 2*(abs(chunk_bytes-target_size) / target_size) < 1) .and. &
     chunk_bytes < CHUNK_MAX) exit

  if (product(chunk_size) == 1) exit
  !! Element size larger than CHUNK_MAX
  j = int(modulo(i, ndims), hsize_t) + 1
  if (j < 1 .or. j > ndims) error stop 'h5fortran: auto index bounds error'
  chunk_size(j) = ceiling(real(chunk_size(j)) / 2.0)
  i = i+1
end do

end subroutine guess_chunk_size

end submodule write
