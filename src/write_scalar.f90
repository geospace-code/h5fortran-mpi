submodule (h5mpi:write) write_scalar

use hdf5, only: h5dwrite_f, h5tclose_f, h5sselect_none_f

implicit none (type, external)

contains


module procedure h5write_scalar

integer :: ier, charlen
integer(HSIZE_T) :: dset_dims(0), mem_dims(0)
integer(HID_T) :: file_space_id, mem_space_id, dset_id, dtype_id, xfer_id, dtype

xfer_id = H5P_DEFAULT_F

if(.not. self%is_open()) error stop 'ERROR:h5fortran:write: file handle is not open'

charlen = 0

select type (A)
type is (character(*))
  dtype = H5T_NATIVE_CHARACTER
  charlen = len(A)
type is (real(real32))
  dtype = H5T_NATIVE_REAL
type is (real(real64))
  dtype = H5T_NATIVE_DOUBLE
type is (integer(int32))
  dtype = H5T_NATIVE_INTEGER
type is (integer(int64))
  dtype = H5T_STD_I64LE
class default
  error stop "ERROR:h5fortran:write: unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, mem_dims=mem_dims, dset_dims=dset_dims, &
    filespace_id=file_space_id, memspace=mem_space_id, dset_id=dset_id, dtype_id=dtype_id, compact=compact, &
    charlen=charlen)

if(self%use_mpi) then
  if(self%mpi_id > 0) then
    call h5sselect_none_f(file_space_id, ier)
    if(ier /= 0) error stop "ERROR:h5fortran:write:h5sselect_none: selecting no write failed for worker. " // dname
    !! for MPI collective scalar writes, only root worker can write.
    !! otherwise race condition would result
  endif

  xfer_id = mpi_collective(dname)
endif

select type (A)
type is (real(real32))
  call h5dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (real(real64))
  call h5dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int32))
  call h5dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (integer(int64))
  call h5dwrite_f(dset_id, dtype, A, dset_dims, ier, file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
type is (character(*))
  call h5dwrite_f(dset_id, dtype_id, A, dset_dims, ier, &
  file_space_id=file_space_id, mem_space_id=mem_space_id, xfer_prp=xfer_id)
  if (ier /= 0) error stop 'h5fortran:write:string: could not write ' // dname // ' to ' // self%filename
  call h5tclose_f(dtype_id, ier)
class default
  error stop "ERROR:h5fortran:write: unsupported type for " // dname
end select
if (ier /= 0) error stop 'ERROR:h5fortran:write: could not write ' // dname // ' to ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing file dataspace: " // dname // " in " // self%filename

if(self%use_mpi) call h5pclose_f(xfer_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing property: " // dname // " in " // self%filename

end procedure h5write_scalar

end submodule write_scalar
