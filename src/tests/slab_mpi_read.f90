program read_slab_mpi
!! use hyperslabs with each worker
!! use HDF5-MPI layer for best efficiency
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int32, int64, real64, stderr=>error_unit
use mpi, only : mpi_comm_size, mpi_comm_rank, mpi_integer

use h5mpi, only : mpi_h5comm, hdf5_file, HSIZE_T

use cli, only : get_cli
use perf, only : print_timing, sysclock2ms

implicit none

external :: mpi_bcast, mpi_init, mpi_finalize

type(hdf5_file) :: h5

real, allocatable :: A3(:,:,:), t3(:,:,:)

character(1000) :: argv, h5fn, refh5fn

integer :: ierr, lx1, lx2, lx3, dx1, i, i0, i1
integer :: comp_lvl, real_bits
integer :: Nmpi, mpi_id, Nrun
integer, parameter :: mpi_root_id = 0

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)

integer(HSIZE_T), allocatable :: dims_full(:)
integer(HSIZE_T), dimension(rank(A3)) :: istart, iend

call mpi_init(ierr)
if(ierr/=0) error stop "mpi_init"

call mpi_comm_size(mpi_h5comm, Nmpi, ierr)
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

tic = 0
Nrun = 1
h5fn = ""
refh5fn = ""
comp_lvl = 0

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

if(len_trim(h5fn) == 0) error stop "please specify -o HDF5 filename to write"
if(len_trim(refh5fn) == 0) error stop "please specify -ref reference HDF5 filename to read"

allocate(t_elapsed(Nrun))

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

if(mpi_id == mpi_root_id) then
  print '(a,i0,a,i0,1x,i0,1x,i0)', "MPI-HDF5 parallel. ", Nmpi, " total MPI processes. shape: ", &
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
if(lx3 < 1 .or. lx2 < 1 .or. lx1 < 1) then
  write(stderr,"(A,i0,A,i0,1x,i0,1x,i0)") "ERROR: MPI ID: ", mpi_id, " failed to receive lx1, lx2, lx3: ", lx1, lx2, lx3
  error stop
endif
!! init workers with sentinel values to catch broken MPI library or mpiexec.
if (debug) print '(a,i0,a,i0,1x,i0,1x,i0)', 'MPI worker: ', mpi_id, ' lx1, lx2, lx3 = ', lx1, lx2, lx3

!! 1-D decompose in rows (neglect ghost cells)
dx1 = lx1 / Nmpi

allocate(A3(dx1, lx2, lx3))

!> assign each worker hyperslab
!! Each process defines dataset in memory and writes it to the hyperslab in the file.

!! Only chunking along first dim, but can make test chunk on any/all dimension(s)
istart(1) = mpi_id * dx1 + 1
istart(2) = 1
istart(3) = 1
iend(1) = istart(1) + dx1 - 1
iend(2) = lx2
iend(3) = lx3

if(debug) print '(a,i0,a,i0,a,i0)', "mpi_writer: mpi_id: ", mpi_id, " istart: ", istart(1), " iend: ", iend(1)

!> benchmark loop

main : do i = 1, Nrun
  if(mpi_id == mpi_root_id) call system_clock(count=tic)
  call h5%open(trim(h5fn), action="r", mpi=.true., debug=debug)
  call h5%read("/A3", A3, istart=istart, iend=iend)
  call h5%close()
  if(mpi_id == mpi_root_id) then
    call system_clock(count=toc)
    t_elapsed(i) = toc-tic
  endif

end do main

!> Check vs. serial read (each worker tests their subarray vs the reference full array)

allocate(t3(lx1, lx2, lx3))

call h5%open(trim(refh5fn), action="r", mpi=.false.)
call h5%read("/A3", t3)
call h5%close()

i0 = mpi_id * dx1 + 1
i1 = (mpi_id + 1) * dx1
if(debug) then
  print '(a,i0,a,3i4,a,3i4,a,2I4)', "TRACE: mpi id ", mpi_id, " shape(A3) ", shape(A3), " shape(t3) ", shape(t3), "i0,i1: ", i0, i1
  print '(a,i0,a,50f4.0)', "TRACE: mpi_id ", mpi_id, " ref subarray: ", t3(i0:i1,:,:)
  print '(a,i0,a,50f4.0)', "TRACE: mpi_id ", mpi_id, " worker subarray: ", A3
endif

if (any(abs(t3(i0:i1,:,:) - A3) > 0.01)) error stop "3D ref mismatch " // trim(refh5fn) // " /= " // trim(h5fn)

!> RESULTS

if(mpi_id == mpi_root_id) then
  call print_timing(Nmpi, h5%comp_lvl, storage_size(A3), int(dims_full), t_elapsed, h5%filesize(), debug, &
    trim(h5fn) // ".read_stat.h5")
endif

if (debug) print '(a,i0)', "mpi finalize: worker: ", mpi_id
call mpi_finalize(ierr)

end program
