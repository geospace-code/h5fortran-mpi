program read_slab_mpi_root
!! use hyperslabs with each worker, but have only root do serial HDF5
!! this is noticably less efficient than using HDF5 MPI built-in interface.
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int32, int64, real64, stderr=>error_unit
use mpi, only : mpi_comm_rank, mpi_comm_size, mpi_integer, mpi_real, MPI_STATUS_IGNORE, MPI_COMM_WORLD

use h5fortran, only : hdf5_file, HSIZE_T

use cli, only : get_cli
use perf, only : print_timing, sysclock2ms

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize, mpi_send, mpi_recv

type(hdf5_file) :: h5

integer, parameter :: tA3 = 100
real, allocatable ::  A3(:,:,:), t3(:,:,:)
character(1000) :: argv

!> default parameters
integer :: real_bits = 32, Nrun = 1
character(1000) :: h5fn = "", refh5fn = ""
integer :: lx1 = -1, lx2 = -1, lx3 = -1

integer :: ierr, dx2, i, j, i0, i1
integer(HSIZE_T), allocatable, dimension(:) :: dims_full
integer :: Nmpi, mpi_id
integer, parameter :: mpi_root_id = 0

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)


call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)

do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, h5fn)
  case("-ref")
    call get_cli(i, argv, refh5fn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  case("-realbits")
    call get_cli(i, argv, real_bits)
  case("-d", "-debug")
    debug = .true.
  end select
end do

if(len_trim(h5fn) == 0) error stop "please specify -o HDF5 filename"
if(len_trim(refh5fn) == 0) error stop "please specify -ref reference HDF5 filename"

if(mpi_id == mpi_root_id) then
  !> get simsize
  call h5%open(trim(h5fn), action="r", mpi=.false., debug=debug)
  call h5%shape("/A3", dims_full)
  call h5%close()
  lx1 = int(dims_full(1), int32)
  lx2 = int(dims_full(2), int32)
  lx3 = int(dims_full(3), int32)
  print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI-root: ", Nmpi, " total MPI processes. shape: ", lx1, lx2, lx3
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
if(lx3 < 0 .or. lx2 < 1 .or. lx1 < 1) then
  write(stderr,"(A,i0,A,i0,1x,i0,1x,i0)") "ERROR: MPI ID: ", mpi_id, " failed to receive lx1, lx2, lx3: ", lx1, lx2, lx3
  error stop
endif
!! init workers with sentinel values to catch broken MPI library or mpiexec.

if (debug) print '(a,i0,a,i0,1x,i0,1x,i0)', 'MPI worker: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

!! 1-D decompose in rows (neglect ghost cells)
dx2 = lx2 / Nmpi

!! root has the full array, and all other processes have a subarray
if(mpi_id == mpi_root_id) then
  allocate(A3(lx1, lx2, lx3), t3(lx1, dx2, lx3))  !< root-only working subarray
else
  allocate(A3(lx1, dx2, lx3))
endif

allocate(t_elapsed(Nrun))

!> benchmark loop
tic = 0
main : do j = 1, Nrun
  !! Root: serial read HDF5 file
  if(mpi_id == mpi_root_id) then
    call system_clock(count=tic)
    call h5%open(trim(h5fn), action="r", mpi=.false., debug=debug)
    call h5%read("/A3", A3)
    call h5%close()
  end if

  !! workers receive data from root
  if(mpi_id == mpi_root_id) then
    !! root's own subarray
    t3 = A3(:, 1:dx2, :)
    !! worker subarrays
    do i = 1, Nmpi-1
      i0 = mpi_id * dx2 + 1
      i1 = (mpi_id + 1) * dx2
      call mpi_send(A3(:, i0:i1, :), lx1*dx2*lx3, MPI_REAL, i, tA3, MPI_COMM_WORLD, ierr)
      if(ierr/=0) error stop "root => worker: mpi_send 3D"
    end do
  else
    !! workers receive data from root
    call mpi_recv(A3, lx1*dx2*lx3, MPI_REAL, mpi_root_id, tA3, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
    if(ierr/=0) error stop "root => worker: mpi_recv 3D"
  endif

  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    t_elapsed(j) = toc-tic
  endif

end do main

!> Check writtenfile vs. serial read
if(mpi_id == mpi_root_id) then
  deallocate(t3)
  allocate(t3(lx1, lx2, lx3))
  call h5%open(trim(refh5fn), action="r", mpi=.false.)
  call h5%read("/A3", t3)

  if (any(abs(t3 - A3) > 0.01)) error stop "3D ref mismatch " // trim(refh5fn) // " /= " // trim(h5fn)

  call print_timing(Nmpi, h5%comp_lvl, storage_size(A3), int(dims_full), t_elapsed, h5%filesize(), debug, &
    trim(h5fn) // ".read_stat.h5")
  call h5%close()
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)
if(ierr/=0) error stop "mpi_finalize failed"

end program
