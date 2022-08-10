submodule (h5fortran:write) write_scalar

use hdf5, only: H5Dwrite_f, h5sselect_none_f

implicit none (type, external)

contains


module procedure h5write_scalar

integer(HSIZE_T) :: dset_dims(0), mem_dims(0)
integer(HID_T) :: file_space_id, dset_id, dtype_id, xfer_id, dtype
integer :: ier, charlen
type(C_PTR) :: cA

select type (A)
type is (real(real32))
  dtype = H5T_NATIVE_REAL
  cA = C_LOC(A)
type is (real(real64))
  dtype = H5T_NATIVE_DOUBLE
  cA = C_LOC(A)
type is (integer(int32))
  dtype = H5T_NATIVE_INTEGER
  cA = C_LOC(A)
type is (integer(int64))
  dtype = H5T_STD_I64LE
  cA = C_LOC(A)
type is (character(*))
  dtype = H5T_NATIVE_CHARACTER
  charlen = len(A)  !< workaround for GCC 8.3.0 bug
  cA = C_LOC(A)
class default
  error stop "ERROR:h5fortran:writer:unknown variable type for " // dname
end select

call hdf_create(self, dname, dtype, mem_dims=mem_dims, dset_dims=dset_dims, &
  filespace_id=file_space_id, dset_id=dset_id, compact=compact, dtype_id=dtype_id, &
  charlen=charlen)

xfer_id = mpi_collective(dname, self%use_mpi)

if(self%use_mpi .and. self%mpi_id > 0) then
    call h5sselect_none_f(file_space_id, ier)
    if(ier /= 0) error stop "ERROR:h5fortran:write:h5sselect_none: selecting no write failed for worker. " // dname
    !! for MPI collective scalar writes, only root worker can write.
    !! otherwise race condition would result
endif

call H5Dwrite_f(dset_id, dtype_id, cA, ier, file_space_id=file_space_id, xfer_prp=xfer_id)
if (ier /= 0) error stop 'ERROR:h5fortran:write: could not write ' // dname // ' ' // self%filename

call H5Tclose_f(dtype_id, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:write:H5Tclose ' // dname // ' ' // self%filename

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer:H5Dclose: " // dname // " " // self%filename

call H5Sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer:H5Sclose file: " // dname // " " // self%filename

if(self%use_mpi) call h5pclose_f(xfer_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing property: " // dname // " in " // self%filename

end procedure h5write_scalar

end submodule write_scalar
