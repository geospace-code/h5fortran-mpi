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

call h%open(path, action='w', mpi=.true.)

call h%write('/x', 1)

call h%writeattr('/x', 'note','this is just a little number')
call h%writeattr('/x', 'hello', 'hi')
call h%writeattr('/x', 'life', [42])
call h%writeattr('/x', 'life_float', [42._real32, 84._real32])
call h%writeattr('/x', 'life_double', [42._real64])

call h%close()

end subroutine test_write_attributes


subroutine test_read_attributes(path)

type(hdf5_file) :: h
character(*), intent(in) :: path
character(1024) :: attr_str
integer :: attr_int(1)
real(real32) :: attr32(2)
real(real64) :: attr64(1)

integer :: x

call h%open(path, action='r', mpi=.true.)

call h%read('/x', x)
if (x/=1) error stop 'readattr: unexpected value'

call h%readattr('/x', 'note', attr_str)
if (attr_str /= 'this is just a little number') error stop 'readattr value note'

call h%readattr('/x', 'life', attr_int)
if (attr_int(1) /= 42) error stop 'readattr: int'

call h%readattr('/x', 'life_float', attr32)
if (any(attr32 /= [42._real32, 84._real32])) error stop 'readattr: real32'

call h%readattr('/x', 'life_double', attr64)
if (attr64(1) /= 42._real64) error stop 'readattr: real64'

call h%close()

end subroutine test_read_attributes

end program
