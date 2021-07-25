program simple
!! use hyperslabs with each worker, but have only root write serial HDF5
!! this is noticably less efficient than using HDF5 MPI built-in interface.
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real64, stderr=>error_unit
use mpi
use hdf5
use h5mpi, only : mpi_h5comm, hdf5_file, mpi_tags
use partition, only : get_simsize
use cli, only : get_cli
use perf, only : print_timing, sysclock2ms

implicit none

type(mpi_tags) :: mt

type(hdf5_file) :: h5

real, allocatable :: A2(:,:), A3(:,:,:), t2(:,:), t3(:,:,:)
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

if(mpi_id == mpi_root_id) print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI serial write. ", Nmpi, " total MPI processes. shape: ", &
  lx1, lx2, lx3


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

!! root has the full array, and all other processes have a subarray
if(mpi_id == mpi_root_id) then
  allocate(A2(lx1, lx2), A3(lx1, lx2, lx3))
  allocate(t2(dx1, lx2), t3(dx1, lx2, lx3))  !< root-only working subarray
else
  allocate(A2(dx1, lx2), A3(dx1, lx2, lx3))
endif

!> dummy data from root to workers
call system_clock(count=tic)
if(mpi_id == mpi_root_id) then
  do i = 1, Nmpi-1
    t2 = d0 + i
    t3 = d0 + i
    call mpi_send(t2, dx1*lx2, MPI_REAL, i, mt%a2, mpi_h5comm, ierr)
    call mpi_send(t3, dx1*lx2*lx3, MPI_REAL, i, mt%a3, mpi_h5comm, ierr)
    if (ierr /= 0) error stop "initialize: root => worker: mpi_send"
  end do
else
  if (debug) print '(a,i0,a,i0)', "mpi_recv worker: ",mpi_id," tag: ", mt%a2
  call mpi_recv(A2, dx1*lx2, MPI_REAL, mpi_root_id, mt%a2, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
  call mpi_recv(A3, dx1*lx2*lx3, MPI_REAL, mpi_root_id, mt%a3, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
  if (ierr /= 0) error stop "initialize: root => worker: mpi_recv"
  if (debug) print '(a,i0,a,i0)', "DONE: mpi_recv worker: ",mpi_id," tag: ", mt%a2
endif
call system_clock(count=toc)

if (debug) print '(a,i0,a,f10.3)', "MPI worker: ", mpi_id, " time to initialize (milliseconds) ", sysclock2ms(toc-tic)

!> benchmark loop
tmin = huge(0_int64)

main : do j = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)
  !! root receives data from workers
  if(mpi_id == mpi_root_id) then
    !! root's own subarray
    A2(1:dx1, :) = d0
    A3(1:dx1, :, :) = d0
    !! worker subarrays
    do i = 1, Nmpi-1
      call mpi_recv(t2, dx1*lx2, MPI_REAL, i, mt%a2, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
      call mpi_recv(t3, dx1*lx2*lx3, MPI_REAL, i, mt%a3, mpi_h5comm, MPI_STATUS_IGNORE, ierr)
      if(ierr/=0) error stop "worker => root: mpi_recv"

      A2(i*dx1+1:(i+1)*dx1,:) = t2
      A3(i*dx1+1:(i+1)*dx1,:,:) = t3
    end do
  else
    !! workers send data to root
    call mpi_send(A2, dx1*lx2, MPI_REAL, mpi_root_id, mt%a2, mpi_h5comm, ierr)
    call mpi_send(A3, dx1*lx2*lx3, MPI_REAL, mpi_root_id, mt%a3, mpi_h5comm, ierr)

    if(ierr/=0) error stop "worker => root: mpi_send"
  endif

  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    print '(a,f9.1)', "worker => root transfer time (ms)", sysclock2ms(toc-tic)
  endif

  if(mpi_id == mpi_root_id) then
    !! Root: serial write HDF5 file
    call h5%open(trim(outfn), action="w", mpi=.false., comp_lvl=3, debug=debug)

    call h5%write("/A2", A2, dims_full(:2))
    call h5%write("/A3", A3, dims_full)

    call h5%close()

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

if(mpi_id == mpi_root_id) call print_timing(storage_size(A3), int(dims_full), tmin, real(h5%filesize()))

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)

end program
