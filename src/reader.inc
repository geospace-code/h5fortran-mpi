integer(HSIZE_T), dimension(rank(A)) :: dims, dset_dims
integer(HID_T) :: dset_id, file_space_id, mem_space_id, xfer_id
integer :: dclass, ier

xfer_id = H5P_DEFAULT_F
dims = shape(A, HSIZE_T)

call h5open_read(self, dname, dims, dset_dims, file_space_id, mem_space_id, dset_id)

if(self%use_mpi) then
  if(present(istart) .and. present(iend)) then
    call mpi_hyperslab(dims, dset_dims, dset_id, file_space_id, mem_space_id, istart=istart, iend=iend, stride=stride)
  endif
  xfer_id = mpi_collective(dname)
endif

call get_dset_class(self, dname, dclass, dset_id)

!> casting is handled by HDF5 library internally
!! select case doesn't allow H5T_*
if(dclass == H5T_FLOAT_F) then
  select type(A)
  type is (real(real64))
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (real(real32))
    call h5dread_f(dset_id, H5T_NATIVE_REAL, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'ERROR:h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(dclass == H5T_INTEGER_F) then
  select type(A)
  type is (integer(int32))
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int64))
    call h5dread_f(dset_id, H5T_STD_I64LE, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'ERROR:h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
elseif(dclass == H5T_STRING_F) then
  error stop "ERROR:h5fortran:reader: character arrays (non-singleton) not yet supported by h5fortran."
else
  error stop 'ERROR:h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier /= 0) error stop 'ERROR:h5fortran:reader:h5dread: ' // dname // ' from ' // self%filename

call h5dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader: closing dataset: " // dname // " in " // self%filename

if(mem_space_id /= H5S_ALL_F) call h5sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader closing memory dataspace: " // dname // " in " // self%filename

if(file_space_id /= H5S_ALL_F) call h5sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader closing file dataspace: " // dname // " in " // self%filename

if(self%use_mpi) call h5pclose_f(xfer_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader closing property: " // dname // " in " // self%filename
