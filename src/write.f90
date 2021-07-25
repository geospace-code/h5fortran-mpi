submodule (h5mpi) write

use mpi, only : mpi_comm_rank

implicit none (type, external)

contains


module procedure hdf_create

integer(HSIZE_T), dimension(size(dims)) :: cnt, stride, blk, offset

logical :: exists
integer :: ierr, mpi_id

plist_id = H5P_DEFAULT_F

if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open: ' // self%filename

call h5ltpath_valid_f(self%file_id, dname, .true., exists, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid
!! stricter than self%exists() since we're creating and/or writing variable
if (ierr /= 0) error stop 'ERROR:h5fortran:create: variable path invalid: ' // dname // ' in ' // self%filename
if(self%debug) print *,'h5fortran:TRACE:create:exists: ' // dname, exists

if (self%use_mpi) then
  !! Each process defines dataset in memory and writes it to the hyperslab in the file.
  call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)
  if(ierr /= 0) error stop "hdf_create: could not get MPI rank"

  !> chunk choices are arbitrary, but must be the same on all processes
  !> only chunking along first dim
  cnt(1) = dims(1)
  cnt(2:) = 1
  offset(1) = mpi_id*cnt(1)
  offset(2:) = 0
  stride = 1
  blk(1) = 1
  blk(2:) = dims_file(2:)
endif

!> compression
if (size(dims) >= 2) then
  call set_deflate(self, dims, plist_id, ierr, chunk_size)
  if (ierr/=0) error stop 'h5fortran:create: problem setting deflate on ' // dname
endif

!> create dataspace
call h5screate_simple_f(size(dims_file), dims_file, filespace, ierr)
if (ierr/=0) error stop "h5screate_simple:filespace " // dname // " " // self%filename

if(self%use_mpi) then
  call h5screate_simple_f(size(dims), dims, memspace, ierr)
  if (ierr/=0) error stop "h5screate_simple:memspace " // dname // " " // self%filename
else
  memspace = H5S_ALL_F
endif

!> create dataset
call h5dcreate_f(self%file_id, dname, dtype, space_id=filespace, dset_id=dset_id, hdferr=ierr, dcpl_id=plist_id)
if (ierr/=0) error stop "h5dcreate: " // dname // " " // self%filename

if(self%use_mpi) then
  !> Select hyperslab in the file.
  call h5dget_space_f(dset_id, filespace, ierr)
  call h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, &
    start=offset, &
    count=cnt, hdferr=ierr, &
    stride=stride, &
    block=blk)
  if (ierr/=0) error stop "h5sselect_hyperslab: " // dname // " " // self%filename

  !! Create property list for collective dataset write
  call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
  call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)

  ! For independent write use
  ! call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)
endif

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

if (.not. self%parallel_compression .or. self%comp_lvl < 1 .or. self%comp_lvl > 9) return

if (present(chunk_size)) then
  cs = chunk_size
  where (cs > dims) cs = dims
  if(self%debug) print *,'TRACE: user request chunk_size ',cs
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
