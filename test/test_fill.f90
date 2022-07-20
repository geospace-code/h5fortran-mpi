program fill

use, intrinsic:: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_finite

use h5mpi, only : hdf5_file
use hdf5, only : HSIZE_T, H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER
use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

implicit none (type, external)

external :: mpi_finalize

type(hdf5_file) :: h5

character(*), parameter :: fn = "test_fill.h5"

real :: NaN, r(3)
integer :: i(2), ierr, mpi_id
character(10) :: c, fill_value

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"

NaN = ieee_value(0., ieee_quiet_nan)

call h5%open(fn, action="w", mpi=.true.)

call h5%create("/r32", H5T_NATIVE_REAL, dset_dims=[3], fill_value=NaN)
call h5%create("/r64", H5T_NATIVE_DOUBLE, dset_dims=[3], fill_value=NaN)
call h5%create("/i32", H5T_NATIVE_INTEGER, dset_dims=[2], fill_value=-1)

!> Note that character fill value must have same length as dataset, hence we use a character(10)
fill_value = "NaN"
call h5%create("/char", H5T_NATIVE_CHARACTER, dset_dims=[1], fill_value=fill_value, charlen=10)
fill_value = ""
call h5%create("/char_blank", H5T_NATIVE_CHARACTER, dset_dims=[1], fill_value=fill_value, charlen=10)
fill_value = " "
call h5%create("/char_space", H5T_NATIVE_CHARACTER, dset_dims=[1], fill_value=fill_value, charlen=10)

call h5%close()


call h5%open(fn, action="r", mpi=.true.)

call h5%read("/r32", r)
if(any(ieee_is_finite(r))) error stop "real32: expected all NaN"

call h5%read("/r64", r)
if(any(ieee_is_finite(r))) error stop "real64: expected all NaN"

call h5%read("/i32", i)
if(any(i /= -1)) error stop "int32: expected all -1"

call h5%read("/char", c)
if(c /= "NaN") error stop "char: expected 'NaN', got: " // c

call h5%read("/char_blank", c)
if(c /= "") error stop "char: expected '', got: " // c

call h5%read("/char_space", c)
if(c /= " ") error stop "char: expected ' ', got: " // c

call h5%close()

if(mpi_id == 0) print *, "OK: fill value"

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

end program
