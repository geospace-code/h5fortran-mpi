program write_slab_mpi_root
!! use hyperslabs with each worker, but have only root do serial HDF5
!! this is noticably less efficient than using HDF5 MPI built-in interface.
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real32, real64, stderr=>error_unit
use mpi, only : mpi_comm_rank, mpi_comm_size, mpi_integer, mpi_real, mpi_status_ignore
use h5mpi, only : mpi_h5comm, hdf5_file, mpi_tags
use cli, only : get_cli, get_simsize
use perf, only : print_timing, sysclock2ms
use test_utils, only : generate_and_send

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize, mpi_send, mpi_recv

type(mpi_tags) :: mt

type(hdf5_file) :: h5

real(real32), allocatable :: A3(:,:,:), t3(:,:,:)
real(real32) :: noise, gensig
character(1000) :: argv, h5fn

integer :: ierr, lx1, lx2, lx3, dx1, i, j, i0, i1, comp_lvl, real_bits
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
noise = 0.
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

lx1 = -1
lx2 = -1
lx3 = -1
if(mpi_id == mpi_root_id) call get_simsize(lx1, lx2, lx3, Nmpi)

if(mpi_id == mpi_root_id) then
  print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI-root write. ", Nmpi, " total MPI processes. shape: ", &
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

!! root has the full array, and all other processes have a subarray
if(mpi_id == mpi_root_id) then
  allocate(A3(lx1, lx2, lx3))
else
  allocate(A3(dx1, lx2, lx3))
endif

tic = 0
if (mpi_id == mpi_root_id) call system_clock(count=tic)

call generate_and_send(Nmpi, mpi_id, mpi_root_id, dx1, lx1, lx2, lx3, mt%a3, mpi_h5comm, noise, gensig, A3)

if (mpi_id == mpi_root_id) then
  call system_clock(count=toc)
  if (debug) print '(a,i0,a,f10.3)', "MPI worker: ", mpi_id, " time to initialize (milliseconds) ", sysclock2ms(toc-tic)
endif

!> benchmark loop
allocate(t_elapsed(Nrun))

main : do j = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)
  !! root receives data from workers
  if(mpi_id == mpi_root_id) then
    !! root's own subarray i=0 is already initialized
    !! worker i=1..Nmpi subarrays
    do i = 1, Nmpi-1
      i0 = i*dx1+1
      i1 = (i+1)*dx1
      call mpi_recv(A3(i0:i1,:,:), dx1*lx2*lx3, MPI_REAL, i, mt%a3, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
      if(ierr/=0) error stop "worker => root: mpi_recv 3D"
    end do
  else
    !! workers send data to root
    call mpi_send(A3, dx1*lx2*lx3, MPI_REAL, mpi_root_id, mt%a3, mpi_h5comm, ierr)
    if(ierr/=0) error stop "worker => root: mpi_send 3D"
  endif

  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    print '(a,f9.1)', "worker => root transfer time (ms)", sysclock2ms(toc-tic)
  endif

  if(mpi_id == mpi_root_id) then
    !! Root: serial write HDF5 file
    call h5%open(trim(h5fn), action="w", mpi=.false., comp_lvl=comp_lvl, debug=debug)
    call h5%write("/A3", A3)
    call h5%close()

    call system_clock(count=toc)
    t_elapsed(j) = toc-tic
  endif

end do main

!> sanity check file contents vs memory
if(mpi_id == mpi_root_id) then
  allocate(t3(lx1, lx2, lx3))

  call h5%open(trim(h5fn), action="r", mpi=.false.)
  call h5%read("/A3", t3)
  call h5%close()

  if (any(abs(t3 - A3) > 0.01)) then
    write(stderr,'(a,i0,1x,i0)') "ERROR: 3D disk vs. memory mismatch."
    write(stderr,'(a,25f4.0)') "disk: ", t3
    write(stderr,'(a,25f4.0)') "memory: ", A3
    error stop trim(h5fn)
  endif
endif

!> RESULTS

if(mpi_id == mpi_root_id) then
  call print_timing(Nmpi, h5%comp_lvl, storage_size(A3), int([lx1, lx2, lx3]), t_elapsed, h5%filesize(), debug, &
    trim(h5fn) // ".write_stat.h5")
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)
if(ierr/=0) error stop "mpi_finalize failed"

end program
