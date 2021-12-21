program write_slab_mpi
!! use hyperslabs with each worker
!! use HDF5-MPI layer for best efficiency
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real64, stderr=>error_unit
use mpi, only : mpi_comm_size, mpi_comm_rank, mpi_integer
use hdf5, only : HSIZE_T
use h5mpi, only : mpi_h5comm, hdf5_file
use cli, only : get_cli, get_simsize
use perf, only : print_timing, sysclock2ms
use kernel, only : gaussian2d

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize

type(hdf5_file) :: h5

real, allocatable :: A2(:,:), A3(:,:,:)
character(1000) :: argv, outfn

integer :: ierr, lx1, lx2, lx3, dx1, i, comp_lvl, real_bits
integer(HSIZE_T), allocatable, dimension(:) :: d2, d3
integer :: Nmpi, mpi_id, Nrun
integer, parameter :: mpi_root_id = 0

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)

integer(HSIZE_T) :: dims_full(rank(A3))

call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

Nrun = 1
outfn = ""
comp_lvl = 0

do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, outfn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  case("-realbits")
    call get_cli(i, argv, real_bits)
  case ("-comp")
    call get_cli(i, argv, comp_lvl)
  case("-d", "-debug")
    debug = .true.
  end select
end do

if(len_trim(outfn) == 0) error stop "please specify -o filename to write"

allocate(t_elapsed(Nrun))

lx1 = -1
lx2 = -1
lx3 = -1
if(mpi_id == mpi_root_id) call get_simsize(lx1, lx2, lx3, Nmpi)

if(mpi_id == mpi_root_id) then
  print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI-HDF5 parallel write. ", Nmpi, " total MPI processes. shape: ", &
  lx1, lx2, lx3
endif

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
A2 = gaussian2d(dx1, lx2, 1.)
call random_number(A3)
A3 = 0.01*A3 + spread(gaussian2d(dx1, lx2, 1.), 3, lx3)

!> benchmark loop

main : do i = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)
  call h5%open(trim(outfn), action="w", mpi=.true., comp_lvl=comp_lvl, debug=debug)

  call h5%write("/A2", A2, dims_full(:2))
  call h5%write("/A3", A3, dims_full)

  call h5%close()
  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    t_elapsed(i) = toc-tic
  endif

end do main

if (debug) print '(a,i0)', "mpi write:done: worker: ", mpi_id

!> sanity check file shape

if(mpi_id == mpi_root_id) then
  call h5%open(trim(outfn), action="r", mpi=.false.)
  call h5%shape("/A2", d2)
  call h5%shape("/A3", d3)
  call h5%close()

  if(any(d2 /= dims_full(:2)) .or. any(d3 /= dims_full)) error stop "slab_mpi: file shape mismatch"
endif

!> RESULTS

if(mpi_id == mpi_root_id) then
  call print_timing(storage_size(A3), int(dims_full), t_elapsed, real(h5%filesize()), outfn=trim(outfn) // ".stat.h5")
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)

end program
