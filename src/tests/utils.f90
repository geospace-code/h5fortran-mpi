module test_utils

use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
use, intrinsic :: iso_fortran_env, only : real32

use mpi, only : MPI_STATUS_IGNORE, MPI_REAL

use kernel, only : gaussian2d

implicit none (type, external)

external :: mpi_send, mpi_recv

private
public :: generate_and_send

contains

subroutine generate_and_send(Nmpi, mpi_id, mpi_root_id, dx1, lx1, lx2, lx3, tagA2, tagA3, mpi_h5comm, noise, A2, A3)

integer, intent(in) :: Nmpi, mpi_id, mpi_root_id, dx1, lx1, lx2, lx3, tagA2, tagA3, mpi_h5comm
real(real32), intent(in) :: noise
real(real32), intent(inout), allocatable :: A2(:,:), A3(:,:,:)

integer :: i, ierr
logical :: test2d
real(real32), allocatable :: t2(:,:), t3(:,:,:)
!! this allows distinct sizes for different use cases (mpi_root vs mpi_hdf5)

real(real32) :: NaN
NaN = ieee_value(0._real32, ieee_quiet_nan)

ierr = 0
test2d = allocated(A2)

if(test2d) A2 = NaN
A3 = NaN

if(mpi_id == mpi_root_id) then
  !> root creates synthetic data for this benchmark
  allocate(t3(lx1,lx2,lx3))
  if(test2d) then
    allocate(t2(lx1,lx2))
    call random_number(t2)
    t2 = noise*t2 + gaussian2d(lx1, lx2, 1.)
  endif
  call random_number(A3)
  t3 = noise*t3 + spread(gaussian2d(lx1, lx2, 1.), 3, lx3)
endif

!> dummy data from root to workers
if(mpi_id == mpi_root_id) then
  do i = 1, Nmpi-1
    if(test2d) call mpi_send(t2(i*dx1+1:(i+1)*dx1, :), dx1*lx2, MPI_REAL, i, tagA2, mpi_h5comm, ierr)
    if (ierr /= 0) error stop "generate: root => worker: mpi_send 2D"
    call mpi_send(t3(i*dx1+1:(i+1)*dx1, :, :), dx1*lx2*lx3, MPI_REAL, i, tagA3, mpi_h5comm, ierr)
    if (ierr /= 0) error stop "generate: root => worker: mpi_send 3D"
  end do

  !> root's subarray
  if(test2d) A2(1:dx1, :) = t2(1:dx1, :)
  A3(1:dx1, :, :) = t3(1:dx1, :, :)
else
  if(test2d) call mpi_recv(A2, dx1*lx2, MPI_REAL, mpi_root_id, tagA2, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
  if (ierr /= 0) error stop "generate: root => worker: mpi_recv 2D"
  call mpi_recv(A3, dx1*lx2*lx3, MPI_REAL, mpi_root_id, tagA3, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
  if (ierr /= 0) error stop "generate: root => worker: mpi_recv 3D"
endif

end subroutine generate_and_send


end module test_utils
