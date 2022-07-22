program test_destruct
!! test hdf5_file destructor, that should auto-flush and close file
!! if user forgets to %close() file

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

use h5fortran, only: hdf5_file

implicit none (type, external)

external :: mpi_finalize

integer :: ierr, mpi_id
character(*), parameter :: fn = "test_destruct.h5"


call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr /= 0) error stop "mpi_comm_rank"

call test_destructor_write(fn)
if(mpi_id == 0) print *, 'OK: destructor write'

call test_destructor_read(fn)
if(mpi_id == 0) print *, 'OK: destructor read'

call mpi_finalize(ierr)

contains


subroutine test_destructor_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h%open(fn, action="w", mpi=.true.)

call h%write('/x', 42)

!! deliberately omitted %close() to test destructor

end subroutine test_destructor_write


subroutine test_destructor_read(fn)

character(*), intent(in) :: fn

integer :: i
type(hdf5_file) :: h

call h%open(fn, action="r", mpi=.true.)

call h%read("/x", i)
if(i/=42) error stop "destructor did not flush " // fn

!! deliberately omitted %close() to test destructor

end subroutine test_destructor_read

end program
