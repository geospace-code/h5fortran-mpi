integer(HID_T) :: filespace, memspace, dset_id, xfer_id, dtype
integer(HSIZE_T) :: dims(rank(value))
integer :: ier

dims = shape(value, HSIZE_T)

select type (value)
type is (real(real32))
  dtype = H5T_NATIVE_REAL
type is (real(real64))
  dtype = H5T_NATIVE_DOUBLE
type is (integer(int32))
  dtype = H5T_NATIVE_INTEGER
type is (integer(int64))
  dtype = H5T_STD_I64LE
class default
  error stop "unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, dims, dims_file=dims_file, &
  filespace=filespace, memspace=memspace, dset_id=dset_id, xfer_id=xfer_id)

select type (value)
type is (real(real32))
  call h5dwrite_f(dset_id, dtype, value, dims_file, ier, file_space_id=filespace, mem_space_id=memspace, xfer_prp=xfer_id)
type is (real(real64))
  call h5dwrite_f(dset_id, dtype, value, dims_file, ier, file_space_id=filespace, mem_space_id=memspace, xfer_prp=xfer_id)
type is (integer(int32))
  call h5dwrite_f(dset_id, dtype, value, dims_file, ier, file_space_id=filespace, mem_space_id=memspace, xfer_prp=xfer_id)
type is (integer(int64))
  call h5dwrite_f(dset_id, dtype, value, dims_file, ier, file_space_id=filespace, mem_space_id=memspace, xfer_prp=xfer_id)
class default
  error stop "unknown variable type for " // dname
end select
if (ier/=0) error stop "h5dwrite: " // dname // " " // self%filename

call hdf_wrapup(filespace, memspace, dset_id, xfer_id)
