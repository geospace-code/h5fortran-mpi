program test_deflate
!! unit tests and registration tests of HDF5 deflate compression write
!! these tests are adapted from non-MPI h5fortran.
!! several of them don't actually need MPI, but demonstrate that properties
!! can be read by each MPI worker when the file is opened wiht h5%open(..., mpi=.true.)

use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64, stderr=>error_unit

use mpi, only : mpi_init, mpi_comm_rank, mpi_comm_size, MPI_COMM_WORLD

use h5mpi, only: hdf5_file, HSIZE_T

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn1='deflate1.h5', fn2='deflate2.h5', fn3='deflate3.h5'
integer, parameter :: N(2) = [50, 1000], &
MIN_COMP = 2  !< lots of CPUs, smaller arrays => poorer compression
integer :: ierr, mpi_id, Nmpi


call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
if(ierr/=0) error stop "mpi_comm_size"
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call test_write_deflate(fn1, N, mpi_id, Nmpi)
if(mpi_id==0) print *,'OK: HDF5 write deflate'

call test_deflate_whole(fn2, N)
if(mpi_id==0) print *,'OK: HDF5 compress whole'

call test_deflate_slice(fn3, N)
if(mpi_id==0) print *,'OK: HDF5 compress slice'

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

contains

subroutine test_write_deflate(fn, N, mpi_id, Nmpi)

character(*), intent(in) :: fn
integer, intent(in) :: N(2), mpi_id, Nmpi

type(hdf5_file) :: h5f
integer(HSIZE_T) :: i0(2), i1(2), dx2
real(real32), allocatable :: A(:,:)
logical :: debug = .false.

!> MPI partition
if(mpi_id == 0) then
  if (Nmpi > 1 .and. (modulo(N(2), Nmpi) /= 0 .or. Nmpi > N(2))) then
    write(stderr, '(a,1x,i0,1x,i0)') "test_deflate_props: MPI worker count must be multiple of N", N(2), Nmpi
    error stop fn
  endif
end if

dx2 = N(2) / Nmpi

allocate(A(N(1), dx2))

A = 0  !< simplest data

i0(1) = 1
i0(2) = mpi_id * dx2 + 1
i1(1) = size(A, 1)
i1(2) = i0(2) + dx2 - 1

!> write with MPI, compressing if available
if(debug) print '(a,i0,1x,2i5,2x,2i5)', "#1 partition: mpi_id, i0, i1 ", mpi_id, i0, i1

call h5f%open(fn, action='w', comp_lvl=1, mpi=.true.)
call h5f%write('/A', A, N, istart=i0, iend=i1, chunk_size=[5, 50])
call h5f%close()

deallocate(A)

if(mpi_id == 0) then
  allocate(A(N(1), N(2)))
  A = 1  !< simplest data
  !! write without MPI, with compression of noMPI dataset
  call h5f%open(fn, action='a', comp_lvl=1, mpi=.false.)

  call h5f%write('/small_contig', A(:4,:4))
  !! not compressed because too small

  call h5f%write('/noMPI', A)
  !! write without MPI, with compression

  call h5f%close()
endif

end subroutine test_write_deflate


subroutine test_deflate_whole(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f
real, allocatable :: A(:,:,:)
integer(hsize_t) :: crat, chunks(3)
integer :: fsize, ierr, mpi_id, Nmpi
integer(HSIZE_T) :: dx2, i0(3), i1(3)

!> MPI partition
call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
if(ierr/=0) error stop "mpi_comm_size"
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

dx2 = N(2) / Nmpi

allocate(A(N(1), dx2, 4))

i0(1) = 1
i0(2) = mpi_id * dx2 + 1
i0(3) = 1
i1(1) = size(A, 1)
i1(2) = i0(2) + dx2 - 1
i1(3) = size(A, 3)

call h5f%open(fn, action='w', comp_lvl=3, mpi=.true.)

call h5f%write('/A', A, [N(1), N(2), 4], istart=i0, iend=i1, chunk_size=[4, 20, 1])
call h5f%chunks('/A', chunks)
if(chunks(1) /= 4 .or. chunks(3) /= 1)  then
  write(stderr, '(a,3I5)') "expected chunks: 4,*,1 but got chunks ", chunks
  error stop '#2 manual chunk unexpected chunk size'
endif

call h5f%write('/A_autochunk', A, [N(1), N(2), 4], istart=i0, iend=i1)
call h5f%chunks('/A_autochunk', chunks)
if(any(chunks < 1)) error stop '#2 auto chunk unexpected chunk size'

call h5f%close()

if(mpi_id == 0) then
  inquire(file=fn, size=fsize)
  crat = (2 * N(1) * N(2) * 4 * storage_size(A) / 8) / fsize
  !! 2* since two datasets same size

  print '(A,F6.2,A,I6)','#2 filesize (Mbytes): ', fsize / 1e6, '   compression ratio:', crat

  if (h5f%parallel_compression) then
    if(crat < MIN_COMP) error stop fn // ' low compression'
  else
    print *, "test_deflate_whole: MPI commpression was disabled, so " // fn // " was not compressed."
  endif
endif

end subroutine test_deflate_whole


subroutine test_deflate_slice(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f
integer, allocatable :: A(:,:,:)
integer(hsize_t) :: crat, chunks(3), dx2, i0(3), i1(3)
integer :: fsize, mpi_id, Nmpi, M(3)

!> MPI partition
call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
if(ierr/=0) error stop "mpi_comm_size"
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

M = [N(1), N(2) - 20, 4]

dx2 = M(2) / Nmpi

allocate(A(N(1)+1, dx2, 4))  !< dim 1 is deliberately not the "right" size, we will index at %write()

i0(1) = 1
i0(2) = mpi_id * dx2 + 1
i0(3) = 1
i1(1) = M(1)
i1(2) = i0(2) + dx2 - 1
i1(3) = M(3)

A = 0

call h5f%open(fn, action='w', comp_lvl=1, mpi=.true.)

call h5f%write('/A', A(:M(1), :, :), dset_dims=M, istart=i0, iend=i1)
call h5f%chunks('/A', chunks)
if(any(chunks < 1)) error stop '#3 auto chunk unexpected chunk size'

call h5f%close()

if(mpi_id == 0) then
  inquire(file=fn, size=fsize)
  crat = (N(1) * N(2) * storage_size(A) / 8) / fsize

  print '(A,F6.2,A,I6)','#3 filesize (Mbytes): ',fsize / 1e6, '  compression ratio:', crat

  if (h5f%parallel_compression) then
    if(crat < MIN_COMP) error stop fn // ' low compression'
  else
    print *, "test_deflate_slice: MPI commpression was disabled, so " // fn // " was not compressed."
  endif
endif

end subroutine test_deflate_slice


end program
