program write_slab_mpi
!! use hyperslabs with each worker
!! use HDF5-MPI layer for best efficiency
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
use, intrinsic :: iso_fortran_env, only : int64, real64, real32, stderr=>error_unit

use mpi, only : mpi_comm_size, mpi_comm_rank, mpi_integer, MPI_COMM_WORLD

use h5fortran, only : hdf5_file

use cli, only : get_cli, get_simsize
use perf, only : print_timing, sysclock2ms
use test_utils, only : generate_and_send

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize

type(hdf5_file) :: h5

integer, parameter :: ta3 = 100

real(real32), allocatable :: S3(:,:,:), ts3(:,:,:), V3(:), dv3(:)

!> default parameters
real(real32) :: noise = 0.
real(real32) :: gensig = -1.
integer :: comp_lvl = 0, real_bits = 32, Nrun = 1
character(1000) :: h5fn = ""
integer :: lx1 = -1, lx2 = -1, lx3 = -1

character(1000) :: argv

integer :: ierr, dx2, i, i0, i1

integer, dimension(rank(S3)) :: istart, iend

integer :: Nmpi, mpi_id
integer, parameter :: mpi_root_id = 0

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)

call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
if(ierr/=0) error stop "mpi_comm_size"
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

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

if(mpi_id == mpi_root_id) then
  call get_simsize(lx1, lx2, lx3, Nmpi)
  print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI-HDF5 parallel write. ", Nmpi, " total MPI processes. shape: ", lx1, lx2, lx3
endif

! call mpi_ibcast(lx1, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, mpi_req, ierr)
! call mpi_ibcast(lx2, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, mpi_req, ierr)
! call mpi_ibcast(lx3, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, mpi_req, ierr)
! call mpi_wait(mpi_req, MPI_STATUS_IGNORE, ierr)
call mpi_bcast(lx1, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, ierr)
if(ierr/=0) error stop "failed to broadcast lx1"
call mpi_bcast(lx2, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, ierr)
if(ierr/=0) error stop "failed to broadcast lx2"
call mpi_bcast(lx3, 1, MPI_INTEGER, mpi_root_id, MPI_COMM_WORLD, ierr)
if(ierr/=0) error stop "failed to broadcast lx3"
if(lx3 < 1 .or. lx2 < 1 .or. lx1 < 1) then
  write(stderr,"(A,i0,A,i0,1x,i0,1x,i0)") "ERROR: MPI ID: ", mpi_id, " failed to receive lx1, lx2, lx3: ", lx1, lx2, lx3
  error stop
endif
!! init workers with sentinel values to catch broken MPI library or mpiexec.

if (debug) print '(a,i0,a,i0,1x,i0,1x,i0)', 'mpi_writer: mpi_id: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

!! 1-D decompose in rows (neglect ghost cells)
dx2 = lx2 / Nmpi

allocate(S3(lx1, dx2, lx3))
!> dummy data
!! root has only a subarray like workers.
!! Here we generate synthetic data on root; real programs wouldn't do this
tic = 0
if (mpi_id == mpi_root_id) call system_clock(count=tic)

call generate_and_send(Nmpi, mpi_id, mpi_root_id, dx2, lx1, lx2, lx3, tA3, noise, gensig, S3)

if (mpi_id == mpi_root_id) then
  call system_clock(count=toc)
  if (debug) print '(a,f10.3)', "mpi root time to initialize workers (milliseconds) ", sysclock2ms(toc-tic)
endif

!> sanity check generated data on the worker
if(gensig < 0) then
  allocate(V3(lx1*dx2), dv3(lx1*dx2))
  V3 = pack(S3, .true.)
  dv3 = V3 - eoshift(V3, -1, V3(1) - 1)
  if (any(dV3 > 1.01)) then
    !! not formatted in case of weird data
    write(stderr, *) "ERROR: MPI ID: ", mpi_id, " failed generate (sequence): ", S3
    write(stderr, *) "diff mpi_id: ", mpi_id, dV3
    error stop
  endif
endif
if(.not.all(ieee_is_finite(S3))) then
  write(stderr, '(a,i0,a,100f5.1)') "ERROR: MPI ID: ", mpi_id, " failed generate (NaN) : ", S3
  error stop
endif

!> assign each worker hyperslab
!! Each process defines dataset in memory and writes it to the hyperslab in the file.

!! can make test chunk on any/all dimension(s)
istart(1) = 1
istart(2) = mpi_id * dx2 + 1
istart(3) = 1
iend(1) = lx1
iend(2) = istart(2) + dx2 - 1
iend(3) = lx3

if(debug) print '(a,i0,a,i0,a,i0)', "mpi_writer: mpi_id: ", mpi_id, " istart: ", istart(1), " iend: ", iend(1)

!> benchmark loop

main : do i = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)

  call h5%open(trim(h5fn), action="w", mpi=.true., comp_lvl=comp_lvl, debug=debug)
  call h5%write("/A3", S3, [lx1, lx2, lx3], istart=istart, iend=iend)
  call h5%close()

  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    t_elapsed(i) = toc-tic
  endif

end do main

if (debug) print '(a,i0)', "mpi write:done: worker: ", mpi_id

!> sanity check file contents vs memory
allocate(ts3(lx1, lx2, lx3))

call h5%open(trim(h5fn), action="r", mpi=.false.)
call h5%read("/A3", ts3)
call h5%close()
i0 = mpi_id*dx2 + 1
i1 = (mpi_id + 1)*dx2
if (any(abs(ts3(:, i0:i1, :) - S3) > 0.01)) then
  write(stderr, '(a,i0,a,i0,1x,i0)') "ERROR: mpi_writer: mpi_id: ", mpi_id, " failed to write to file between i0,i1: ", i0, i1
  write(stderr,'(a,i0,1x,i0)') "ERROR: 3D disk vs. memory mismatch."
  write(stderr,'(a,i0,1x,100f5.1)') "disk worker ",mpi_id, ts3(:, i0:i1, :)
  write(stderr,'(a,i0,1x,100f5.1)') "memory worker: ", mpi_id, S3
  error stop trim(h5fn)
endif

!> RESULTS

if(mpi_id == mpi_root_id) then
  call h5%open(trim(h5fn), action="r", mpi=.false.)
  call print_timing(Nmpi, h5%comp_lvl, storage_size(S3), [lx1, lx2, lx3], t_elapsed, h5%filesize(), debug, &
  trim(h5fn) // ".write_stat.h5")
  call h5%close()
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)
if (ierr/=0) error stop "mpi_finalize failed"

end program
