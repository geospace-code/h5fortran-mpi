program test_scalar

use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank
use h5mpi, only : hdf5_file

implicit none (type, external)

external :: mpi_finalize

integer :: ierr, mpi_id


call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call test_scalar_collective(mpi_id)

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

contains

subroutine test_scalar_collective(mpi_id)
!! this would be unusual use case to write the same dataset scalar from each worker
!! but it is a test of the collective write
!! in general, the written dataset value is unpredicable in this write / read race condition

integer, intent(in) :: mpi_id

type(hdf5_file) :: h5
integer :: i

call h5%open("scalar_coll.h5", action="w", mpi=.true.)

call h5%write("/mpi_id", mpi_id)

call h5%read("/mpi_id", i)

print '(a,i0,a,i0)', "MPI worker: ", mpi_id, "read value: ", i

call h5%close()



end subroutine test_scalar_collective


end program
