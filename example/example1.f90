program example2

use, intrinsic :: iso_fortran_env, only : real32, int32

use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank, mpi_comm_size

use h5fortran, only : hdf5_file, HSIZE_T

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: filename = 'example1.h5'
integer(int32) :: ierr, mpi_id, Nmpi, i32

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
