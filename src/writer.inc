integer(HID_T) :: file_space_id, mem_space_id, dset_id, xfer_id, dtype, dtype_id
integer(HSIZE_T), dimension(rank(A)) :: mem_dims, dims_dset
integer :: ier, charlen
type(C_PTR) :: cA

mem_dims = shape(A, HSIZE_T)

if(present(dset_dims)) then
  dims_dset = dset_dims
else
  if(present(istart) .or. present(iend)) then
    error stop "ERROR:h5fortran:write: dset_dims required if istart or iend are specified: " // dname // " in " // self%filename
  endif
  dims_dset = mem_dims
endif

charlen = 0

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

!> create or open dataset
call hdf_create(self, dname, dtype, mem_dims=mem_dims, dset_dims=dims_dset, &
filespace_id=file_space_id, dset_id=dset_id, &
istart=istart, iend=iend, stride=stride, chunk_size=chunk_size, compact=compact, &
dtype_id=dtype_id, charlen=charlen)

if((self%use_mpi .and. present(dset_dims)) .or. (present(istart) .and. present(iend))) then
  call hdf_get_slice(mem_dims, dset_id, file_space_id, mem_space_id, istart=istart, iend=iend, stride=stride, dset_dims=dims_dset)
else
  mem_space_id = H5S_ALL_F
endif

if(self%use_mpi) then
  if(.not. present(dset_dims) .and. self%mpi_id > 0) then
    call h5sselect_none_f(file_space_id, ier)
    if(ier /= 0) error stop "ERROR:h5fortran:writer:h5sselect_none: selecting no write failed for worker. " // dname
    !! for MPI collective scalar or whole array writes, only root worker can write.
    !! otherwise race condition would result
  endif
endif

xfer_id = mpi_collective(dname, self%use_mpi)

call H5Dwrite_f(dset_id, dtype_id, cA, ier, mem_space_id=mem_space_id, file_space_id=file_space_id, xfer_prp=xfer_id)
if (ier /= 0) error stop 'ERROR:h5fortran:write: could not write ' // dname // ' ' // self%filename

call H5Tclose_f(dtype_id, ier)
if (ier /= 0) error stop 'ERROR:h5fortran:write:H5Tclose ' // dname // ' ' // self%filename

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer:H5Dclose: " // dname // " " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer:H5Sclose memory: " // dname // " " // self%filename

call H5Sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer:H5Sclose file: " // dname // " " // self%filename

if(self%use_mpi) call h5pclose_f(xfer_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:writer closing property: " // dname // " in " // self%filename
