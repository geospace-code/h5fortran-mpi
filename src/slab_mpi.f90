program simple
!! use hyperslabs with each worker
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real64, stderr=>error_unit
use mpi
use hdf5
use h5mpi, only : mpi_h5comm, hdf5_file
use partition, only : get_simsize
use cli, only : get_cli
use perf, only : print_timing

implicit none

type(hdf5_file) :: h5

real, allocatable :: A2(:,:), A3(:,:,:)
character(1000) :: argv, outfn

integer :: ierr, lx1, lx2, lx3, dx1, i, j
integer(HSIZE_T), allocatable, dimension(:) :: d2, d3
integer :: Nmpi, mpi_id, mpi_req, Nrun
integer, parameter :: mpi_root_id = 0
real, parameter :: d0 = 10.  !< dummy value to start from

logical :: debug = .false.

integer(int64) :: tic, toc, tmin

integer(HSIZE_T) :: dims_full(rank(A3))

call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

Nrun = 1
outfn = ""

do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, outfn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  case("-d")
    debug = .true.
  end select
end do

if(len_trim(outfn) == 0) error stop "please specify -o filename to write"

lx1 = -1
lx2 = -1
lx3 = -1
if(mpi_id == mpi_root_id) call get_simsize(lx1, lx2, lx3, Nmpi)

if(mpi_id == mpi_root_id) print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI-HDF5 parallel write. ", Nmpi, " total MPI processes. shape: ", &
  lx1,lx2,lx3

! call mpi_ibcast(lx1, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, mpi_req, ierr)
! call mpi_ibcast(lx2, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, mpi_req, ierr)
! call mpi_ibcast(lx3, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, mpi_req, ierr)
! call mpi_wait(mpi_req, MPI_STATUS_IGNORE, ierr)
call mpi_bcast(lx1, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, ierr)
call mpi_bcast(lx2, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, ierr)
call mpi_bcast(lx3, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, ierr)
if(ierr/=0 .or. lx3 < 1 .or. lx2 < 1 .or. lx1 < 1) then
  write(stderr,"(A,I5,A)") "ERROR: MPI ID:", mpi_id, " - failed to send lx1, lx2, lx3"
  error stop
endif
!! we init workers with sentinel values to catch broken MPI library or mpiexec.

if (debug) print '(a,i0,a,i0,1x,i0,1x,i0)', 'MPI worker: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

dims_full = [lx1, lx2, lx3]

!! 1-D decompose in rows (neglect ghost cells)
dx1 = lx1 / Nmpi

allocate(A2(dx1, lx2), A3(dx1, lx2, lx3))
!> dummy data
A2 = mpi_id
A3 = mpi_id

!> benchmark loop
tmin = huge(0_int64)

main : do j = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)
  call h5%open(trim(outfn), action="w", mpi=.true., comp_lvl=3, debug=debug)

  call h5%write("/A2", A2, dims_full(:2))
  call h5%write("/A3", A3, dims_full)

  call h5%close()
  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    tmin = min(tmin, toc-tic)
  endif

end do main

!> sanity check file shape

if(mpi_id == mpi_root_id) then
  call h5%open(trim(outfn), action="r", mpi=.false.)
  call h5%shape("/A2", d2)
  call h5%shape("/A3", d3)
  call h5%close()

  if(any(d2 /= dims_full(:2)) .or. any(d3 /= dims_full)) error stop "slab_mpi: file shape mismatch"
endif

!> RESULTS

if(mpi_id == mpi_root_id) call print_timing(storage_size(A3), int(dims_full), tmin)

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)

end program
