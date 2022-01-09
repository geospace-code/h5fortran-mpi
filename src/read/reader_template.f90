integer(HSIZE_T), dimension(rank(value)) :: dims, dset_dims
integer(HID_T) :: dset_id, file_space_id, mem_space_id, xfer_id
integer :: dclass, ier

xfer_id = H5P_DEFAULT_F

dims = shape(value, HSIZE_T)

call h5open_read(self, dname, dims, dset_dims, file_space_id, mem_space_id, dset_id)

if(self%use_mpi) then
  if(present(istart) .and. present(iend)) then
    call mpi_hyperslab(dims, dset_dims, dset_id, file_space_id, mem_space_id, dname, istart=istart, iend=iend)
  endif
  xfer_id = mpi_collective(dname)
endif

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
if(ier/=0) error stop 'h5fortran:reader: h5dread_f ' // dname // ' from ' // self%filename

call hdf_wrapup(file_space_id, mem_space_id, dset_id, xfer_id)
