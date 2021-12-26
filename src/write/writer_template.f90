integer(HID_T) :: file_space_id, mem_space_id, dset_id, xfer_id, dtype
integer(HSIZE_T), dimension(rank(value)) :: dims, dims_dset
integer :: ier

dims = shape(value, HSIZE_T)
if(present(dset_dims)) then
  select type (dset_dims)
  type is (integer(HSIZE_T))
    dims_dset = dset_dims
  type is (integer(int32))
    dims_dset = dset_dims
  class default
    error stop "write: expecting dset_dimes to be integer"
  end select
else
  if(self%use_mpi) error stop "h5mpi:write: must specify dset_dims if using MPI"
  !! FIXME: there may be a way to do indepedent write rather than simply fail
  dims_dset = dims
endif

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

call hdf_create(self, dname, dtype, dims, dset_dims=dims_dset, &
  filespace=file_space_id, memspace=mem_space_id, dset_id=dset_id, xfer_id=xfer_id)

select type (value)
type is (real(real32))
  call h5dwrite_f(dset_id, dtype, value, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (real(real64))
  call h5dwrite_f(dset_id, dtype, value, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int32))
  call h5dwrite_f(dset_id, dtype, value, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int64))
  call h5dwrite_f(dset_id, dtype, value, dims_dset, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
class default
  error stop "unknown variable type for " // dname
end select
if (ier/=0) error stop 'h5fortran:ERROR: could not write ' // dname // ' to ' // self%filename

call hdf_wrapup(file_space_id, mem_space_id, dset_id, xfer_id)
