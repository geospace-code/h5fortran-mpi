integer(HID_T) :: file_space_id, mem_space_id, dset_id, xfer_id, dtype
integer(HSIZE_T), dimension(rank(value)) :: mem_dims, dims_dset
integer :: ier

xfer_id = H5P_DEFAULT_F

mem_dims = shape(value, HSIZE_T)

if(present(dset_dims)) then
  select type (dset_dims)
  type is (integer(HSIZE_T))
    dims_dset = dset_dims
  type is (integer(int32))
    dims_dset = dset_dims
  class default
    error stop "write: expecting dset_dims to be integer"
  end select
else
  dims_dset = mem_dims
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

!> create or open dataset
call hdf_create(self, dname, dtype, mem_dims=mem_dims, dset_dims=dims_dset, &
filespace=file_space_id, memspace=mem_space_id, dset_id=dset_id, &
istart=istart, iend=iend, chunk_size=chunk_size, &
compact=compact)

if(self%use_mpi) then
  if(present(dset_dims)) then
    call mpi_hyperslab(mem_dims, dims_dset, dset_id, file_space_id, mem_space_id, istart=istart, iend=iend)
  elseif(self%mpi_id > 0) then
    call h5sselect_none_f(file_space_id, ier)
    if(ier /= 0) error stop "ERROR:h5fortran:writer:h5sselect_none: selecting no write failed for worker. " // dname
    !! for MPI collective scalar or whole array writes, only root worker can write.
    !! otherwise race condition would result
  endif

  xfer_id = mpi_collective(dname)
endif

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
  error stop "ERROR:h5fortran:writer: unknown variable type for " // dname
end select
if (ier /= 0) error stop 'ERROR:h5fortran:h5dwrite: could not write ' // dname // ' to ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing file dataspace: " // dname // " in " // self%filename

if(self%use_mpi) call h5pclose_f(xfer_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing property: " // dname // " in " // self%filename
