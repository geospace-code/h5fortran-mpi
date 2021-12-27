program write_slab_mpi
!! use hyperslabs with each worker
!! use HDF5-MPI layer for best efficiency
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real64, stderr=>error_unit
use mpi, only : mpi_comm_size, mpi_comm_rank, mpi_integer
use h5mpi, only : mpi_h5comm, hdf5_file, mpi_tags, HSIZE_T
use cli, only : get_cli, get_simsize
use perf, only : print_timing, sysclock2ms
use test_utils, only : generate_and_send

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize

type(hdf5_file) :: h5
type(mpi_tags) :: mt

real, allocatable :: A3(:,:,:)
real :: noise, gensig
character(1000) :: argv, h5fn

integer :: ierr, lx1, lx2, lx3, dx1, i, comp_lvl, real_bits
integer(HSIZE_T), allocatable, dimension(:) ::  d3
integer :: Nmpi, mpi_id, Nrun
integer, parameter :: mpi_root_id = 0

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)

call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

Nrun = 1
h5fn = ""
comp_lvl = 0
gensig = 1.

do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, h5fn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  case("-realbits")
    call get_cli(i, argv, real_bits)
  case ("-comp")
    call get_cli(i, argv, comp_lvl)
  case ("-noise")
    call get_cli(i, argv, noise)
  case ("-gen")
    call get_cli(i, argv, gensig)
  case("-d", "-debug")
    debug = .true.
  end select
end do

if(len_trim(h5fn) == 0) error stop "please specify -o filename to write"

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
if(ierr/=0) error stop "failed to broadcast lx1"
call mpi_bcast(lx2, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, ierr)
if(ierr/=0) error stop "failed to broadcast lx2"
call mpi_bcast(lx3, 1, MPI_INTEGER, mpi_root_id, mpi_h5comm, ierr)
if(ierr/=0) error stop "failed to broadcast lx3"
if(lx2 < 1 .or. lx1 < 1) then
  write(stderr,"(A,i0,A,i0,1x,i0,1x,i0)") "ERROR: MPI ID: ", mpi_id, " failed to receive lx1, lx2, lx3: ", lx1, lx2, lx3
  error stop
endif
!! init workers with sentinel values to catch broken MPI library or mpiexec.

if (debug) print '(a,i0,a,i0,1x,i0,1x,i0)', 'MPI worker: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

!! 1-D decompose in rows (neglect ghost cells)
dx1 = lx1 / Nmpi

allocate(A3(dx1, lx2, lx3))
!> dummy data
!! root has only a subarray like workers.
!! Here we generate synthetic data on root; real programs wouldn't do this

if (mpi_id == mpi_root_id) call system_clock(count=tic)

call generate_and_send(Nmpi, mpi_id, mpi_root_id, dx1, lx1, lx2, lx3, mt%a3, mpi_h5comm, noise, gensig, A3)

if (mpi_id == mpi_root_id) then
  call system_clock(count=toc)
  if (debug) print '(a,i0,a,f10.3)', "MPI worker: ", mpi_id, " time to initialize (milliseconds) ", sysclock2ms(toc-tic)
endif
!> benchmark loop

main : do i = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)

  call h5%open(trim(h5fn), action="w", mpi=.true., comp_lvl=comp_lvl, debug=debug)
  call h5%write("/A3", A3, [lx1, lx2, lx3])
  call h5%close()

  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    t_elapsed(i) = toc-tic
  endif

end do main

if (debug) print '(a,i0)', "mpi write:done: worker: ", mpi_id

!> sanity check file shape

if(mpi_id == mpi_root_id) then

  call h5%open(trim(h5fn), action="r", mpi=.false.)

  call h5%shape("/A3", d3)
  if(any(d3 /= [lx1, lx2, lx3])) error stop "slab_mpi: file shape mismatch"

  call h5%close()

endif

!> RESULTS

if(mpi_id == mpi_root_id) then
  call print_timing(Nmpi, h5%comp_lvl, storage_size(A3), [lx1, lx2, lx3], t_elapsed, h5%filesize(), debug, &
  trim(h5fn) // ".write_stat.h5")
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)
if (ierr/=0) error stop "mpi_finalize failed"

end program
