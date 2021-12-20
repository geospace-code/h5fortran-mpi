integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: ds_id, space_id, mem_space_id, native_dtype
integer :: ier

space_id = H5S_ALL_F
mem_space_id = H5S_ALL_F
dims = shape(value, HSIZE_T)

call hdf_shape_check(self, dname, dims)
call h5dopen_f(self%file_id, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: open ' // dname // ' from ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> casting is handled by HDF5 library internally
!! select case doesn't allow H5T_*
if(native_dtype == H5T_NATIVE_DOUBLE .or. native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, space_id)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER .or. native_dtype == H5T_STD_I64LE) then
  select type(value)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, space_id)
  type is (integer(int64))
    call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier, mem_space_id, space_id)
  class default
    error stop 'h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call h5dclose_f(ds_id, ier)
if (ier/=0) error stop 'h5fortran:reader: error closing dataset ' // dname // ' in ' // self%filename
