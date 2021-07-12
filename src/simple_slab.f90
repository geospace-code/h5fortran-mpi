program simple
!! a more optimal case is to use hyperslabs with each worker
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use mpi
use hdf5
use mpi_h5write, only : mpi_h5comm, hdf5_file

implicit none

type(hdf5_file) :: h5

real, allocatable :: A2(:,:), A3(:,:,:)
character(:), allocatable :: outfn
character(1000) :: argv

integer :: ierr, lx1, lx2, lx3, dx1
integer :: Nmpi, mpi_id, mpi_req
integer, parameter :: mpi_root_id = 0

integer(HSIZE_T) :: dims_full(rank(A3))

call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"
call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

lx1 = 0
lx2 = 0
lx3 = 0
if(mpi_id == 0) call get_simsize("simsize.txt", Nmpi, lx1, lx2, lx3)

call mpi_ibcast(lx1, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, mpi_req, ierr)
call mpi_ibcast(lx2, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, mpi_req, ierr)
call mpi_ibcast(lx3, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, mpi_req, ierr)
print *, 'MPI worker: ', mpi_id, 'request:' , mpi_req
call mpi_wait(mpi_req, MPI_STATUS_IGNORE, ierr)
if(ierr/=0) error stop "failed to send lx1, lx2, lx3"

print *, 'MPI worker: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

dims_full = [lx1, lx2, lx3]

!> output HDF5 file to write
outfn = "out.h5"
call get_command_argument(1, argv, status=ierr)
if(ierr == 0) outfn = trim(argv)

!! 1-D decompose in rows (neglect ghost cells)
dx1 = lx1 / Nmpi

allocate(A2(dx1, lx2), A3(dx1, lx2, lx3))
!> dummy data
A2 = mpi_id
A3 = mpi_id

call h5%open("out.h5", action="w")

call h5%write("/A2", A2, dims_full(:2))
call h5%write("/A3", A3, dims_full)

call h5%close(close_hdf5_interface=.true.)

call mpi_finalize(ierr)


contains


subroutine get_simsize(sizefn, Nmpi, lx1, lx2, lx3)

character(*), intent(in) :: sizefn
integer, intent(in) :: Nmpi
integer, intent(out) :: lx1, lx2, lx3

logical :: exists
integer :: u, ierr

!! dummy problem
inquire(file=sizefn, exist=exists)
if(.not.exists) error stop sizefn // ' not found'
open(newunit=u, file=sizefn, action="read", status='old', iostat=ierr)
read(u, *) lx1, lx2, lx3
close(u)

!> MPI sanity check
if (Nmpi > lx1) error stop "too many MPI workers"
if (modulo(lx1, Nmpi) /= 0) error stop "number of MPI workers must evenly divide problem size."

end subroutine get_simsize


end program
