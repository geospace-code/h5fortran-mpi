module test_utils

use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_finite
use, intrinsic :: iso_fortran_env, only : real32

use mpi, only : MPI_STATUS_IGNORE, MPI_REAL, MPI_COMM_WORLD

use kernel, only : phantom

implicit none (type, external)

external :: mpi_send, mpi_recv

private
public :: generate_and_send

contains

subroutine generate_and_send(Nmpi, mpi_id, mpi_root_id, dx2, lx1, lx2, lx3, tagA3, noise, gensig, A3)

integer, intent(in) :: Nmpi, mpi_id, mpi_root_id, dx2, lx1, lx2, lx3, tagA3
real(real32), intent(in) :: noise, gensig
real(real32), intent(inout), allocatable :: A3(:,:,:)

integer :: i, ierr, i0, i1
real(real32), allocatable :: t3(:,:,:)
!! this allows distinct sizes for different use cases (mpi_root vs mpi_hdf5)

real(real32) :: NaN
NaN = ieee_value(0._real32, ieee_quiet_nan)

if (.not. (ieee_is_finite(gensig) .and. ieee_is_finite(noise)) ) error stop "unset noise or gensig"

A3 = NaN

if(mpi_id == mpi_root_id) then
  !> root creates synthetic data for this benchmark
  allocate(t3(lx1, lx2, lx3))
  t3 = 0.

  if(gensig >= 0) then
    call random_number(t3)
    t3 = noise*t3
  endif

  t3 = t3 + spread(phantom(lx1, lx2, gensig), 3, lx3)
endif

!> dummy data from root to workers
if(mpi_id == mpi_root_id) then
  do i = 1, Nmpi-1
    i0 = i*dx2 + 1
    i1 = (i + 1)*dx2
    ! print '(a,i0,1x,i0)', "TRACE: generate istart, iend: ", i0, i1
    call mpi_send(t3(:, i0:i1, :), lx1*dx2*lx3, MPI_REAL, i, tagA3, MPI_COMM_WORLD, ierr)
    if (ierr /= 0) error stop "generate: root => worker: mpi_send 3D"
  end do

  !> root's subarray
  A3(:, 1:dx2, :) = t3(:, 1:dx2, :)
else
  call mpi_recv(A3, lx1*dx2*lx3, MPI_REAL, mpi_root_id, tagA3, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
  if (ierr /= 0) error stop "generate: root => worker: mpi_recv 3D"
endif

end subroutine generate_and_send


end module test_utils
