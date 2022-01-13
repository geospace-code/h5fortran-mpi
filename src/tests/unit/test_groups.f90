program test_groups
!! groups test
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit

use h5mpi, only: hdf5_file
use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

implicit none (type, external)

external :: mpi_finalize

integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"


call test_group('test_groups.h5')
if(mpi_id == 0) print *,'OK: group variable'

call test_write_existing('overwrite.h5')
if(mpi_id == 0) print *,'OK: write existing variable'


call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"


contains

subroutine test_group(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h5f

call h5f%open(fn, action='w', mpi=.true.)

call h5f%write('/test/group3/scalar', 1_int32)

call h5f%write('/test/group3/scalar_real', 1._real32)

if(.not. h5f%exist('/test/group3/scalar')) error stop "/test/group3/scalar does not exist: create group failed"

call h5f%close()

end subroutine test_group


subroutine test_write_existing(fn)
type(hdf5_file) :: h5f
character(*), intent(in) :: fn

call h5f%open(fn, action='w', mpi=.true.)
call h5f%write('/scalar_int', 42_int32)
call h5f%write('/int1d', [42_int32, 1_int32])
call h5f%close()

call h5f%open(fn, action='r+', mpi=.true.)
call h5f%write('/scalar_int', 100_int32)
call h5f%write('/int1d', [100_int32, 10_int32])
call h5f%close()

end subroutine test_write_existing

end program
