program test_string

use, intrinsic:: iso_fortran_env, only:  stderr=>error_unit

use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

use h5mpi, only : hdf5_file

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn='test_string.h5'

integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"

if(mpi_id == 0) then
  call test_write(fn)
  print *, "OK: HDF5 string write"
endif

call test_read(fn)
if(mpi_id == 0) print *,'OK: HDF5 string read'

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

contains


subroutine test_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h%open(fn, action='w', mpi=.false.)

call h%write('/little', '42')
call h%write('/MySentence', 'this is a little sentence.')

call h%close()

end subroutine test_write


subroutine test_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
character(2) :: value
character(1024) :: val1k

call h%open(fn, action='r', mpi=.true.)
call h%read('/little', value)

if (value /= '42') error stop 'test_string:  read/write verification failure. Value: '// value

!> longer character than data
call h%read('/little', val1k)

if (len_trim(val1k) /= 2) then
  write(stderr, *) 'expected character len_trim 2 but got len_trim() = ', len_trim(val1k)
  error stop
endif

call h%close()

end subroutine test_read

end program
