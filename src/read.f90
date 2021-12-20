submodule (h5mpi) hdf5_read

implicit none (type, external)

contains


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
