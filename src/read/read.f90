submodule (h5mpi) hdf5_read

use mpi, only : mpi_comm_rank

implicit none (type, external)

contains


subroutine h5open_read(self, dname, dims, filespace, dset_id, xfer_id)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer(HID_T), intent(out), optional :: filespace, dset_id, xfer_id

integer(HSIZE_T), dimension(size(dims)) :: cnt, stride, blk, offset, dset_dims

integer :: ierr, mpi_id
integer(HID_T) :: plist_id

filespace = H5S_ALL_F
plist_id = H5P_DEFAULT_F
xfer_id = H5P_DEFAULT_F

call hdf_shape_check(self, dname, dims, dset_dims)

call h5dopen_f(self%file_id, dname, dset_id, ierr)
if(ierr /= 0) error stop 'h5open_read: open ' // dname // ' from ' // self%filename

if (self%use_mpi) then
  !! Each process defines dataset in memory and reads from hyperslab in the file.
  call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)
  if(ierr /= 0) error stop "h5open_read: could not get MPI rank"

  !> chunk choices are arbitrary, but must be the same on all processes
  !> TODO: only chunking along first dim
  cnt(1) = dims(1)
  cnt(2:) = 1
  offset(1) = mpi_id*cnt(1)
  offset(2:) = 0
  stride = 1
  blk(1) = 1
  blk(2:) = dset_dims(2:)

  !> Select hyperslab in the file.
  call h5dget_space_f(dset_id, filespace, ierr)
  call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
    start=offset, &
    count=cnt, hdferr=ierr, &
    stride=stride, &
    block=blk)
  if (ierr/=0) error stop "h5sselect_hyperslab: " // dname // " " // self%filename

  !! Create property list for collective dataset operations
  call h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, ierr)
  call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ierr)

  ! For independent dataset operations
  ! call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_INDEPENDENT_F, ierr)
endif

end subroutine h5open_read


integer(hid_t) function get_native_dtype(ds_id, dname, filename) result(native_dtype)

integer(hid_t), intent(in) :: ds_id
character(*), intent(in) :: dname, filename

integer(hid_t) :: dtype_id, native_dtype_id
integer :: class
integer :: ierr
! integer :: order, machine_order
integer(size_t) :: size_bytes

!> get the dataset variable type
!! the "type" and "native_type" are just IDs, the final native type is composed from:
!! * enddianness
!! * generic type
call h5dget_type_f(ds_id, dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get internal dtype ' // dname // ' from ' // filename

call h5tget_native_type_f(dtype_id, H5T_DIR_ASCEND_F, native_dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get native dtype id ' // dname // ' from ' // filename

!> we think endianness is handled by HDF5 ... ?
! call h5tget_order_f(native_dtype_id, order, ierr)
! if(ierr/=0) error stop 'h5fortran:reader: get endianness ' // dname // ' from ' // filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ierr)
! if(order /= machine_order) error stop 'h5fortran:reader: endianness does not match machine native ' // dname // ' from ' // filename

!> compose datatype inferred
call h5tget_class_f(native_dtype_id, class, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get class ' // dname // ' from ' // filename

call h5tget_size_f(native_dtype_id, size_bytes, ierr)
if(ierr/=0) error stop 'h5fortran:reader: get byte size ' // dname // ' from ' // filename

call h5tclose_f(dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:reader: closing dtype ' // dname // ' from ' // filename


if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    native_dtype = H5T_STD_I64LE
  else
    error stop "h5fortran:reader: expected 32-bit or 64-bit integer:" // dname // ' from ' // filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "h5fortran:reader: expected 32-bit or 64-bit real:" // dname // ' from ' // filename
  endif
elseif(class == H5T_STRING_F) then
  native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "h5fortran:reader: non-handled datatype: " // dname // " from " // filename
endif

end function get_native_dtype

end submodule hdf5_read
