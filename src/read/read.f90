submodule (h5mpi) hdf5_read

use hdf5, only : h5tget_native_type_f, h5tget_class_f, h5dget_type_f, h5tget_size_f, h5tclose_f, h5topen_f, &
h5dopen_f, h5dread_f,  &
H5T_DIR_ASCEND_F
use h5lt, only : h5ltread_dataset_string_f

implicit none (type, external)

contains


module procedure h5open_read

integer(HSIZE_T), dimension(size(dims)) :: dset_dims

integer :: ierr
integer(HID_T) :: plist_id

filespace = H5S_ALL_F
memspace = H5S_ALL_F
plist_id = H5P_DEFAULT_F
xfer_id = H5P_DEFAULT_F

call hdf_shape_check(self, dname, dims, dset_dims)

call h5dopen_f(self%file_id, dname, dset_id, ierr)
if(ierr /= 0) error stop 'h5open_read: open ' // dname // ' from ' // self%filename

if(.not. self%use_mpi) return

!> create dataspace
call h5screate_simple_f(rank=size(dset_dims), dims=dset_dims, space_id=filespace, hdferr=ierr)
if (ierr/=0) error stop "h5screate_simple:filespace " // dname // " " // self%filename

call h5screate_simple_f(rank=size(dims), dims=dims, space_id=memspace, hdferr=ierr)
if (ierr/=0) error stop "h5screate_simple:memspace " // dname // " " // self%filename

call mpi_hyperslab(dims, dset_dims, dset_id, filespace, dname)

xfer_id = mpi_collective(dname)

end procedure h5open_read


module procedure get_class

call get_dset_class(self, dname, get_class)

end procedure get_class


subroutine get_dset_class(self, dname, class, ds_id, size_bytes)
!! get the dataset class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(out) :: class
integer(hid_t), intent(in), optional :: ds_id
integer(size_t), intent(out), optional :: size_bytes

integer :: ierr
integer(hid_t) :: dtype_id, native_dtype_id, dset_id

if(present(ds_id)) then
  dset_id = ds_id
else
  call h5dopen_f(self%file_id, dname, dset_id, ierr)
  if(ierr/=0) error stop 'h5fortran:get_class: ' // dname // ' from ' // self%filename
endif

call h5dget_type_f(dset_id, dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: dtype_id ' // dname // ' from ' // self%filename

call h5tget_native_type_f(dtype_id, H5T_DIR_ASCEND_F, native_dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: native_dtype_id ' // dname // ' from ' // self%filename

!> compose datatype inferred
call h5tget_class_f(native_dtype_id, class, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: class ' // dname // ' from ' // self%filename

if(present(size_bytes)) then
  call h5tget_size_f(native_dtype_id, size_bytes, ierr)
  if(ierr/=0) error stop 'h5fortran:get_class: byte size ' // dname // ' from ' // self%filename
endif

!> close to avoid memory leaks
call h5tclose_f(native_dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: closing native dtype ' // dname // ' from ' // self%filename

call h5tclose_f(dtype_id, ierr)
if(ierr/=0) error stop 'h5fortran:get_class: closing dtype ' // dname // ' from ' // self%filename

if(.not.present(ds_id)) then
  call h5dclose_f(dset_id, ierr)
  if(ierr/=0) error stop 'h5fortran:get_class: close dataset ' // dname // ' from ' // self%filename
endif

end subroutine get_dset_class


module procedure get_native_dtype
!! get the dataset variable type:
!! {H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE}

integer :: class
! integer :: order, machine_order
integer(size_t) :: size_bytes

call get_dset_class(self, dname, class, ds_id, size_bytes)

!> endianness and within type casting is handled by HDF5
! call h5tget_order_f(native_dtype_id, order, ierr)
! if(ierr/=0) error stop 'h5fortran:reader: get endianness ' // dname // ' from ' // self%filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ierr)
! if(order /= machine_order) error stop 'h5fortran:read: endianness /= machine native: ' &
! // dname // ' from ' // self%filename

if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_STD_I64LE
  else
    error stop "h5fortran:get_native_dtype: expected 32-bit or 64-bit integer:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "h5fortran:get_native_dtype: expected 32-bit or 64-bit real:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_STRING_F) then
  get_native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "h5fortran:get_native_dtype: non-handled datatype: " // dname // " from " // self%filename
endif

end procedure get_native_dtype

end submodule hdf5_read
