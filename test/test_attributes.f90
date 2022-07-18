program test_attributes

use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit

use h5mpi, only: hdf5_file
use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: filename = 'test_attr.h5'

integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"


call test_write_attributes(filename)
if(mpi_id == 0) print *,'PASSED: HDF5 write attributes'

call test_read_attributes(filename)
if(mpi_id == 0) print *, 'PASSED: HDF5 read attributes'


call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"


contains

subroutine test_write_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path

integer :: i2(1,1), i3(1,1,1), i4(1,1,1,1), i5(1,1,1,1,1), i6(1,1,1,1,1,1), i7(1,1,1,1,1,1,1)

call h%open(path, action='w', mpi=.true.)

call h%write('/x', 1)

call h%writeattr('/x', 'int32-scalar', 42)
call h%writeattr('/x', 'char','this is just a little number')
call h%writeattr('/x', 'hello', 'hi')
call h%writeattr('/x', 'real32_1d', [real(real32) :: 42, 84])
call h%writeattr('/x', 'real64_1d0', [42._real64])

call h%writeattr('/x', 'i2', i2)
call h%writeattr('/x', 'i3', i3)
call h%writeattr('/x', 'i4', i4)
call h%writeattr('/x', 'i5', i5)
call h%writeattr('/x', 'i6', i6)
call h%writeattr('/x', 'i7', i7)

call h%close()

call h%open(path, action='a', mpi=.true.)
call h%writeattr('/x', 'int32-scalar', 142)
call h%writeattr('/x', 'real32_1d', [real(real32) :: 142, 84])
call h%writeattr('/x', 'char', 'overwrite attrs')
call h%delete_attr('/x', 'hello')
call h%close()

end subroutine test_write_attributes


subroutine test_read_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path
character(1024) :: attr_str
integer :: int32_0
real(real32) :: attr32(2)
real(real64) :: attr64

integer :: x

integer :: i2(1,1), i3(1,1,1), i4(1,1,1,1), i5(1,1,1,1,1), i6(1,1,1,1,1,1), i7(1,1,1,1,1,1,1)

call h%open(path, action='r', mpi=.true.)

call h%read('/x', x)
if (x/=1) error stop 'readattr: unexpected value'

call h%readattr('/x', 'char', attr_str)
if (attr_str /= 'overwrite attrs') error stop 'overwrite attrs failed: ' // attr_str

call h%readattr('/x', 'int32-scalar', int32_0)
if (int32_0 /= 142) error stop 'readattr: int32-scalar'

call h%readattr('/x', 'real32_1d', attr32)
if (any(attr32 /= [real(real32) :: 142, 84])) error stop 'readattr: real32'

call h%readattr('/x', 'real64_1d0', attr64)
if (attr64 /= 42._real64) error stop 'readattr: real64'

if (h%exist_attr('/x', 'hello')) error stop "delete attr failed"

call h%readattr('/x', 'i2', i2)
call h%readattr('/x', 'i3', i3)
call h%readattr('/x', 'i4', i4)
call h%readattr('/x', 'i5', i5)
call h%readattr('/x', 'i6', i6)
call h%readattr('/x', 'i7', i7)

call h%close()

end subroutine test_read_attributes

end program
