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
integer :: Nmpi, mpi_id

integer(HSIZE_T) :: dims_full(rank(A3))

call mpi_init(ierr)
call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

outfn = "out.h5"
call get_command_argument(1, argv, status=ierr)
if(ierr == 0) outfn = trim(argv)

!! dummy problem
lx1 = 16
lx2 = 4
lx3 = 8

dims_full = [lx1, lx2, lx3]

if (Nmpi > lx1) error stop "too many MPI workers"
if (modulo(lx1, Nmpi) /= 0) error stop "number of MPI workers must evenly divide problem size."

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

end program
