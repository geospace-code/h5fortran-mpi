integer(HSIZE_T) :: dims(rank(value))
integer(HID_T) :: dset_id, file_space_id, mem_space_id, xfer_id
integer :: dclass, ier

mem_space_id = H5S_ALL_F

dims = shape(value, HSIZE_T)

call h5open_read(self, dname, dims, file_space_id, dset_id, xfer_id)

call get_dset_class(self, dname, dclass, dset_id)

!> casting is handled by HDF5 library internally
!! select case doesn't allow H5T_*
if(dclass == H5T_FLOAT_F) then
  select type(value)
  type is (real(real64))
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (real(real32))
    call h5dread_f(dset_id, H5T_NATIVE_REAL, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(dclass == H5T_INTEGER_F) then
  select type(value)
  type is (integer(int32))
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int64))
    call h5dread_f(dset_id, H5T_STD_I64LE, value, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call hdf_wrapup(file_space_id, mem_space_id, dset_id, xfer_id)
