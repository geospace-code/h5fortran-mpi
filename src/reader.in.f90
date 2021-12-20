submodule (h5mpi:hdf5_read) reader

use, intrinsic :: iso_fortran_env, only : int32, int64

implicit none (type, external)

contains

module procedure h5read_scalar

integer(HSIZE_T) :: dims(rank(value))
integer(hid_t) :: ds_id, native_dtype
integer :: ier

logical :: vector_scalar

real(real32) :: buf_r32(1)
real(real64) :: buf_r64(1)
integer(int32) :: buf_i32(1)
integer(int64) :: buf_i64(1)

call hdf_rank_check(self, dname, rank(value), vector_scalar)
if(vector_scalar) then
  select type(value)
  type is (real(real32))
    call ph5read_1d(self, dname, buf_r32)
    value = buf_r32(1)
  type is (real(real64))
    call ph5read_1d(self, dname, buf_r64)
    value = buf_r64(1)
  type is (integer(int32))
    call ph5read_1d(self, dname, buf_i32)
    value = buf_i32(1)
  type is (integer(int64))
    call ph5read_1d(self, dname, buf_i64)
    value = buf_i64(1)
  class default
    error stop "h5fortran:read:vector_scalar: unknown memory variable type" // dname
  end select
  return
endif

call h5dopen_f(self%file_id, dname, ds_id, ier)
if(ier/=0) error stop 'h5fortran:reader: ' // dname // ' could not be opened in ' // self%filename

native_dtype = get_native_dtype(ds_id, dname, self%filename)

!> cast the dataset read from disk to the variable type presented by user h5f%read("/my_dataset", x)
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
!! https://support.hdfgroup.org/HDF5/doc/UG/HDF5_Users_Guide-Responsive%20HTML5/index.html#t=HDF5_Users_Guide%2FDatatypes%2FHDF5_Datatypes.htm%23TOC_6_10_Data_Transferbc-26&rhtocid=6.5_2
if(native_dtype == H5T_NATIVE_DOUBLE .or. native_dtype == H5T_NATIVE_REAL) then
  select type(value)
  type is (real(real64))
    call h5dread_f(ds_id, H5T_NATIVE_DOUBLE, value, dims, ier)
  type is (real(real32))
    call h5dread_f(ds_id, H5T_NATIVE_REAL, value, dims, ier)
  class default
    error stop 'h5fortran:read: real disk dataset ' // dname // ' needs real memory variable'
  end select
elseif(native_dtype == H5T_NATIVE_INTEGER .or. native_dtype == H5T_STD_I64LE) then
  select type(value)
  type is (integer(int32))
    call h5dread_f(ds_id, H5T_NATIVE_INTEGER, value, dims, ier)
  type is (integer(int64))
    call h5dread_f(ds_id, H5T_STD_I64LE, value, dims, ier)
  class default
    error stop 'h5fortran:read: integer disk dataset ' // dname // ' needs integer memory variable'
  end select
else
  error stop 'h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'h5fortran:reader: reading ' // dname // ' from ' // self%filename

call h5dclose_f(ds_id, ier)
if (ier/=0) error stop 'h5fortran:reader: error closing dataset ' // dname // ' in ' // self%filename

end procedure h5read_scalar



module procedure ph5read_1d
@reader_template@
end procedure ph5read_1d

module procedure ph5read_2d
@reader_template@
end procedure ph5read_2d

module procedure ph5read_3d
@reader_template@
end procedure ph5read_3d


end submodule reader