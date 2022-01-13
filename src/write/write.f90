submodule (h5mpi) write

use hdf5, only : h5pset_deflate_f, h5pset_fletcher32_f, h5pset_shuffle_f, h5pset_layout_f, &
h5dwrite_f, &
h5lcreate_soft_f, &
h5screate_f, &
H5S_SCALAR_F

implicit none (type, external)

contains


module procedure hdf_create

logical :: exists, is_scalar
integer :: ierr
integer(HID_T) :: dcpl

dcpl = H5P_DEFAULT_F
memspace = H5S_ALL_F

is_scalar = size(mem_dims) == 0

!> sanity check: file is open
if(.not. self%is_open()) error stop 'ERROR:h5fortran:write: file handle is not open: ' // self%filename

!> sanity check: dataset path is valid
call h5ltpath_valid_f(self%file_id, dname, .true., exists, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:create: variable path invalid: ' // dname // ' in ' // self%filename
!! h5lexists_f can false error with groups--just use h5ltpath_valid
!! stricter than self%exists() since we're creating and/or writing variable

if(exists) then
  if (.not.present(istart)) then
    if (is_scalar) then
      !! scalar
      call hdf_rank_check(self, dname, size(mem_dims))
    else
      call hdf_shape_check(self, dname, mem_dims)
    endif
  endif
  !! FIXME: read and write slice shape not checked; but should check in future versions

  !> open dataset
  call h5dopen_f(self%file_id, dname, dset_id, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:create: could not open ' // dname // ' in ' // self%filename

  !> get dataset filespace
  call h5dget_space_f(dset_id, filespace, ierr)
  if(ierr /= 0) error stop 'ERROR:h5fortran:create could not get dataset ' // dname // ' in ' // self%filename

  if(self%use_mpi .and. is_scalar .and. self%mpi_id > 0) call h5sselect_none_f(filespace, ierr)
  !! for MPI collective scalar writes, only root worker can write.
  !! otherwise race condition would result

  return
endif

!> Only new datasets go past this point

!> compression
if(size(mem_dims) >= 2) then
  call set_deflate(self, mem_dims, dcpl, chunk_size)
endif

!> compact dataset (for very small datasets to increase I/O speed)
if(present(compact)) then
!! datasets are EITHER compact or chunked.
if(compact .and. dcpl == H5P_DEFAULT_F .and. product(dset_dims) * 8 < 60000)  then
!! 64000 byte limit, here we assumed 8 bytes / element
  call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ierr)
  if (check(ierr, self%filename)) error stop "ERROR:h5fortran:hdf_create:h5pcreate: " // dname

  call h5pset_layout_f(dcpl, H5D_COMPACT_F, ierr)
  if (check(ierr, self%filename)) error stop "ERROR:h5fortran:hdf_create:h5pset_layout: " // dname
endif
endif

!> create dataset dataspace
if(size(dset_dims) == 0) then
  call h5screate_f(H5S_SCALAR_F, filespace, ierr)
else
  call h5screate_simple_f(size(dset_dims), dset_dims, filespace, ierr)
endif
if (ierr/=0) error stop "ERROR:h5fortran:hdf_create:h5screate:filespace " // dname // " " // self%filename

if(self%use_mpi .and. is_scalar .and. self%mpi_id > 0) call h5sselect_none_f(filespace, ierr)
!! for MPI collective scalar writes, only root worker can write.
!! otherwise race condition would result

!> create dataset
call h5dcreate_f(self%file_id, dname, dtype, space_id=filespace, dset_id=dset_id, hdferr=ierr, dcpl_id=dcpl)
if (ierr/=0) error stop "ERROR:h5fortran:hdf_create:h5dcreate: " // dname // " " // self%filename

call h5pclose_f(dcpl, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:h5pclose: " // dname // ' in ' // self%filename

end procedure hdf_create


module procedure create_softlink
!! HDF5 soft link -- to variables in same file
!! target need not exist (dangling link)
!! linking to external files requires an external link (different function required)

integer :: ierr

call H5Lcreate_soft_f(tgt, self%file_id, link, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:create_softlink: ' // link // ' in ' // self%filename

end procedure create_softlink


subroutine set_deflate(self, dims, dcpl, chunk_size)
class(hdf5_file), intent(in) :: self
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out) :: dcpl
integer, intent(in), optional :: chunk_size(:)

integer(HSIZE_T) :: cs(size(dims))
integer :: ierr


dcpl = H5P_DEFAULT_F

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

if(self%debug) print *,'DEBUG:set_deflate: dims: ',dims,'chunk size: ', cs

if(any(cs == 0)) return  !< array too small to chunk
if(any(cs < 0)) error stop "ERROR:h5fortran:set_deflate: chunk_size must be strictly positive"

call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:set_deflate:h5pcreate: " // self%filename

call h5pset_chunk_f(dcpl, size(dims), cs, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:set_deflate:h5pset_chunk: " // self%filename

if (self%fletcher32) then
  !! fletcher32 filter adds a checksum to the data
  if (self%use_mpi .and. .not. self%parallel_compression) then
    write(stderr, '(a)') 'WARNING: h5fortran:set_deflate: fletcher32 parallel filter not supported ' // self%filename
  else
    call h5pset_fletcher32_f(dcpl, ierr)
    if (ierr/=0) error stop "ERROR:h5fortran:set_deflate:h5pset_fletcher32: " // self%filename
  endif
endif

if (self%use_mpi .and. .not. self%parallel_compression) then
  write(stderr, '(a)') 'WARNING: h5fortran:set_deflate: deflate parallel filter not supported ' // self%filename
  return
endif

if (self%comp_lvl < 1 .or. self%comp_lvl > 9) return

if(self%shuffle) then
  !! shuffle filter improves compression
  if (self%use_mpi .and. .not. self%parallel_compression) then
    write(stderr, '(a)') 'WARNING: h5fortran:set_deflate: shuffle parallel filter not supported ' // self%filename
  else
    call h5pset_shuffle_f(dcpl, ierr)
    if (ierr/=0) error stop "ERROR:h5fortran:set_deflate:h5pset_shuffle: " // self%filename
  endif
endif

call h5pset_deflate_f(dcpl, self%comp_lvl, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:set_deflate:h5pset_deflate: " // self%filename

if(self%debug) print '(a,i0)','TRACE:set_deflate done, comp_lvl: ', self%comp_lvl

end subroutine set_deflate


pure subroutine guess_chunk_size(dims, chunk_size)
!! if array is too small to chunk, returns 0.

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


chunk_size = 0

if (product(dims) * TYPESIZE < CHUNK_MIN) return

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
  if (j < 1 .or. j > ndims) error stop 'ERROR:h5fortran:guess_chunk_size: auto index bounds error'
  chunk_size(j) = ceiling(real(chunk_size(j)) / 2.0)
  i = i+1
end do

end subroutine guess_chunk_size

end submodule write
