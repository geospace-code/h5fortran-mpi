program test_deflate_read

use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64, stderr=>error_unit
use mpi, only : mpi_init, mpi_comm_rank, mpi_comm_size, MPI_COMM_WORLD

use h5fortran, only: hdf5_file

implicit none (type, external)

external :: mpi_finalize

integer :: ierr, mpi_id, Nmpi
character(*), parameter :: fn1='deflate1.h5'
integer, parameter :: N(2) = [50, 1000]


call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
if(ierr/=0) error stop "mpi_comm_size"
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call test_read_deflate(fn1, N, mpi_id, Nmpi)
if(mpi_id==0) print *,'OK: HDF5 read deflate'

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

contains

subroutine test_read_deflate(fn, N, mpi_id, Nmpi)

character(*), intent(in) :: fn
integer, intent(in) :: N(2), Nmpi, mpi_id

type(hdf5_file) :: h5f
integer :: i0(2), i1(2), dx2
real(real32), allocatable :: A(:,:)
logical :: debug = .false.

!> MPI partition
if(mpi_id == 0) then
  if (Nmpi > 1 .and. (modulo(N(2), Nmpi) /= 0 .or. Nmpi > N(2))) then
    write(stderr, '(a,1x,i0,1x,i0)') "test_deflate_props: MPI worker count must be multiple of N", N(2), Nmpi
    error stop fn
  endif
end if

dx2 = N(2) / Nmpi

allocate(A(N(1), dx2))

i0(1) = 1
i0(2) = mpi_id * dx2 + 1
i1(1) = size(A, 1)
i1(2) = i0(2) + dx2 - 1

!> read with MPI
if(debug) print '(a,i0,1x,2i5,2x,2i5)', "#1 partition: mpi_id, i0, i1 ", mpi_id, i0, i1

call h5f%open(fn, action='r', mpi=.true.)
call h5f%read('/A', A, istart=i0, iend=i1)
call h5f%read('/noMPI', A, istart=i0, iend=i1)  !< compressed with MPI
call h5f%close()

end subroutine test_read_deflate

end program
