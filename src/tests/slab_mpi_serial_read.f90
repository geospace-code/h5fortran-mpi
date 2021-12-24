program read_slab_mpi_root
!! use hyperslabs with each worker, but have only root do serial HDF5
!! this is noticably less efficient than using HDF5 MPI built-in interface.
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int32, int64, real64, stderr=>error_unit
use mpi, only : mpi_comm_rank, mpi_comm_size, mpi_integer, mpi_real, mpi_status_ignore
use hdf5, only : HSIZE_T
use h5mpi, only : mpi_h5comm, hdf5_file, mpi_tags
use cli, only : get_cli
use perf, only : print_timing, sysclock2ms

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize, mpi_send, mpi_recv

type(mpi_tags) :: mt

type(hdf5_file) :: h5

real, allocatable :: A2(:,:), A3(:,:,:), t2(:,:), t3(:,:,:)
character(1000) :: argv, h5fn

integer :: ierr, dx1, i, j, comp_lvl, real_bits
integer :: lx1, lx2, lx3
integer(HSIZE_T), allocatable, dimension(:) :: dims_full
integer :: Nmpi, mpi_id, Nrun
integer, parameter :: mpi_root_id = 0

logical :: debug = .false.
logical :: test2d = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)


call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

Nrun = 1
h5fn = ""
comp_lvl = 0

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
  case("-d")
    debug = .true.
  end select
end do

if(len_trim(h5fn) == 0) error stop "please specify -o HDF5 filename"

lx1 = -1
lx2 = -1
lx3 = -1

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
if(lx3 < 1 .or. lx2 < 1 .or. lx1 < 1) then
  write(stderr,"(A,i0,A,i0,1x,i0,1x,i0)") "ERROR: MPI ID: ", mpi_id, " failed to receive lx1, lx2, lx3: ", lx1, lx2, lx3
  error stop
endif
!! init workers with sentinel values to catch broken MPI library or mpiexec.

if (debug) print '(a,i0,a,i0,1x,i0,1x,i0)', 'MPI worker: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

!! 1-D decompose in rows (neglect ghost cells)
dx1 = lx1 / Nmpi

!! root has the full array, and all other processes have a subarray
if(mpi_id == mpi_root_id) then
  if(test2d) allocate(A2(lx1, lx2), t2(dx1, lx2))
  allocate(A3(lx1, lx2, lx3), t3(dx1, lx2, lx3))  !< root-only working subarray
else
  if(test2d) allocate(A2(dx1, lx2))
  allocate(A3(dx1, lx2, lx3))
endif

allocate(t_elapsed(Nrun))

!> benchmark loop

main : do j = 1, Nrun
  !! Root: serial read HDF5 file
  if(mpi_id == mpi_root_id) then
    call system_clock(count=tic)
    call h5%open(trim(h5fn), action="r", mpi=.false., debug=debug)

    if(test2d) call h5%read("/A2", A2)
    call h5%read("/A3", A3)

    call h5%close()
  end if

  !! workers receive data from root
  if(mpi_id == mpi_root_id) then
    !! root's own subarray
    if(test2d) t2 = A2(1:dx1, :)
    t3 = A3(1:dx1, :, :)
    !! worker subarrays
    do i = 1, Nmpi-1
      if(test2d) then
        call mpi_send(A2(i*dx1+1:(i+1)*dx1,:), dx1*lx2, MPI_REAL, i, mt%a2, mpi_h5comm, ierr)
        if(ierr/=0) error stop "root => worker: mpi_send 2D"
      endif
      call mpi_send(A3(i*dx1+1:(i+1)*dx1,:,:), dx1*lx2*lx3, MPI_REAL, i, mt%a3, mpi_h5comm, ierr)
      if(ierr/=0) error stop "root => worker: mpi_send 3D"
    end do
  else
    !! workers receive data from root
    if(test2d) then
      call mpi_recv(A2, dx1*lx2, MPI_REAL, mpi_root_id, mt%a2, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
      if(ierr/=0) error stop "root => worker: mpi_recv 2D"
    endif
    call mpi_recv(A3, dx1*lx2*lx3, MPI_REAL, mpi_root_id, mt%a3, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
    if(ierr/=0) error stop "root => worker: mpi_recv 3D"
  endif

  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    t_elapsed(j) = toc-tic
  endif

end do main

!> RESULTS

if(mpi_id == mpi_root_id) then
  call print_timing(Nmpi, h5%comp_lvl, storage_size(A3), int(dims_full), t_elapsed, h5%filesize(), trim(h5fn) // ".read_stat.h5")
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)
if(ierr/=0) error stop "mpi_finalize failed"

end program
