program example2

use, intrinsic :: iso_fortran_env, only : real32

use mpi, only : mpi_init, mpi_finalize

use h5mpi, only : hdf5_file, HSIZE_T

implicit none (type, external)

character(*), parameter :: filename = 'example1.h5'
integer :: ierr, mpi_id, Nmpi

type(hdf5_file) :: h5f

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init failed"

call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank failed"

if(mpi_id == 0) then

call h5f%open(filename, action='w', mpi=.false.)
call h5f%write('/x', 123)
call h5f%close()

call h5f%open(filename, action='r', mpi=.false.)
call h5f%read('/x', i32)
if (i32 /= 123) error stop 'incorrect value read'

print *, 'OK: non-MPI write / read'

endif

call mpi_finalize(ierr)

end program
