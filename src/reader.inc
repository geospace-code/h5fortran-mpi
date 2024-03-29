integer(HSIZE_T), dimension(rank(A)) :: dims
integer(HID_T) :: dset_id, file_space_id, mem_space_id, xfer_id
integer :: dclass, ier


dims = shape(A, HSIZE_T)

call h5dopen_f(self%file_id, dname, dset_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:reader:H5Dopen ' // dname // ' from ' // self%filename
call H5Dget_space_f(dset_id, file_space_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:reader:H5Dget_space ' // dname // ' from ' // self%filename

if(present(istart) .and. present(iend)) then
  call hdf_get_slice(dims, dset_id, file_space_id, mem_space_id, istart=istart, iend=iend, stride=stride)
else
  call hdf_shape_check(self, dname, file_space_id, dims)

  call H5Dget_space_f(dset_id, mem_space_id, ier)
  if(ier/=0) error stop "ERROR:h5fortran:reader:H5Dget_space " // dname
endif

xfer_id = mpi_collective(dname, self%use_mpi)

call get_obj_class(self, dname, dset_id, dclass)

!> casting is handled by HDF5 library internally
!! select case doesn't allow H5T_*
if(dclass == H5T_FLOAT_F .OR. dclass == H5T_INTEGER_F) then
  select type(A)
  type is (real(real64))
    call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (real(real32))
    call h5dread_f(dset_id, H5T_NATIVE_REAL, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int32))
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  type is (integer(int64))
    call h5dread_f(dset_id, H5T_STD_I64LE, A, dims, ier, mem_space_id, file_space_id, xfer_id)
  class default
    error stop 'ERROR:h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
elseif(dclass == H5T_STRING_F) then
  select type(A)
  type is (character(*)) !< kind=c_char too
    call read_char(self, dname, A, dset_id, mem_space_id, file_space_id)
  class default
    error stop 'ERROR:h5fortran:read: string dataset ' // dname // ' needs character memory variable'
  end select
else
  error stop 'ERROR:h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier /= 0) error stop 'ERROR:h5fortran:reader:h5dread: ' // dname // ' from ' // self%filename

call H5Dclose_f(dset_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader:H5Dclose " // dname // " in " // self%filename

call H5Sclose_f(mem_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader:H5Sclose: memory: " // dname // " in " // self%filename

call H5Sclose_f(file_space_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader:H5Sclose: file " // dname // " in " // self%filename

if(self%use_mpi) call h5pclose_f(xfer_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:reader closing property: " // dname // " in " // self%filename
