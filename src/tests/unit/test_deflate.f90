program test_deflate
!! unit tests and registration tests of HDF5 deflate compression write
!! these tests are adapted from non-MPI h5fortran.
!! several of them don't actually need MPI, but demonstrate that properties
!! can be read by each MPI worker when the file is opened wiht h5%open(..., mpi=.true.)

use, intrinsic:: iso_fortran_env, only: int32, int64, real32, real64, stderr=>error_unit

use hdf5, only : H5D_CHUNKED_F, H5D_CONTIGUOUS_F, hsize_t
use mpi, only : mpi_init, mpi_comm_rank, mpi_comm_size, MPI_COMM_WORLD

use h5mpi, only: hdf5_file, HSIZE_T

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn1='deflate1.h5', fn2='deflate2.h5', fn3='deflate3.h5'
integer, parameter :: N(2) = [50, 1000]
integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call test_write_deflate(fn1, N)
if(mpi_id==0) print *,'OK: HDF5 write deflate'

call test_read_deflate_props(fn1, N)
if(mpi_id==0) print *,'OK: HDF5 read deflate properties'

call test_deflate_whole(fn2, N)
if(mpi_id==0) print *,'OK: HDF5 compress whole'

call test_deflate_slice(fn3, N)
if(mpi_id==0) print *,'OK: HDF5 compress slice'

if(mpi_id==0) call test_get_deflate(fn1)
!! quirk in HDF5? only works with mpi=.false.
!! else get file close error
print *, 'OK: HDF5 get deflate'

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

contains

subroutine test_write_deflate(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N(2)

type(hdf5_file) :: h5f

integer(HSIZE_T) :: i0(2), i1(2), dx2
integer :: Nmpi, mpi_id

real(real32), allocatable :: A(:,:)

logical :: debug = .false.

!> MPI partition
call mpi_comm_size(MPI_COMM_WORLD, Nmpi, ierr)
if(ierr/=0) error stop "mpi_comm_size"
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

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

if(mpi_id == 0) then
  !! write small dataset without MPI, with compression of noMPI dataset
  call h5f%open(fn, action='a', comp_lvl=1, mpi=.false.)

  call h5f%write('/small_contig', A(:4,:4))
  !! not compressed because too small

  call h5f%write('/noMPI', A(:,:))
  !! write without MPI, with compression

  call h5f%close()
endif

end subroutine test_write_deflate


subroutine test_read_deflate_props(fn, N)

character(*), intent(in) :: fn
integer, dimension(2), intent(in) :: N

type(hdf5_file) :: h5f

integer ::  fsize, layout, mpi_id
integer(int64) :: crat
integer(HSIZE_T) :: chunks(2)

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)

if(mpi_id == 0) then
  inquire(file=fn, size=fsize)
  crat = (N(1) * N(2) * 32 / 8) / fsize
  print '(A,F6.2,A,I6)','#1 filesize (Mbytes): ',fsize/1e6, '   2D compression ratio:',crat
  if (h5f%parallel_compression) then
    if(crat < 5) error stop '2D low compression'
  else
    print *, "MPI commpression was disabled, so " // fn // " was not compressed."
  endif
endif

call h5f%open(fn, action='r', mpi=.true.)

layout = h5f%layout('/A')
if(layout /= H5D_CHUNKED_F) error stop '#1 not chunked layout: ' // fn
if(.not.h5f%is_chunked('/A')) error stop '#1 not chunked layout: ' // fn
call h5f%chunks('/A', chunks)
if(any(chunks(:2) /= [5, 50])) then
  write(stderr, '(a,2I5)') "expected chunks: 5,50 but got chunks ", chunks
  error stop '#1 get_chunk mismatch'
endif
layout = h5f%layout('/small_contig')
if(layout /= H5D_CONTIGUOUS_F) error stop '#1 not contiguous layout'
if(.not.h5f%is_contig('/small_contig')) error stop '#1 not contig layout'
call h5f%chunks('/small_contig', chunks)
if(any(chunks(:2) /= -1)) error stop '#1 get_chunk mismatch'

call h5f%close()

end subroutine test_read_deflate_props


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

call h5f%write('/A', A, [N(1), N(2), 4], istart=i0, iend=i1, chunk_size=[4, 50, 1])
call h5f%chunks('/A', chunks)
if(any(chunks /= [4,50,1]))  then
  write(stderr, '(a,3I5)') "expected chunks: 4,40,1 but got chunks ", chunks
  error stop '#2 manual chunk unexpected chunk size'
endif

call h5f%write('/A_autochunk', A, [N(1), N(2), 4], istart=i0, iend=i1)
call h5f%chunks('/A_autochunk', chunks)
if(any(chunks /= [13,63,2]))  then
  write(stderr, '(a,3I5)') "expected chunks: 13,63,2 but got chunks ", chunks
  error stop '#2 auto chunk unexpected chunk size'
endif

call h5f%close()

if(mpi_id == 0) then
  inquire(file=fn, size=fsize)
  crat = (N(1) * N(2) * 4 * storage_size(A) / 8) / fsize

  print '(A,F6.2,A,I6)','#2 filesize (Mbytes): ', fsize / 1e6, '   3D compression ratio:',crat

  if (h5f%parallel_compression) then
    if(crat < 5) error stop fn // ' low compression'
  else
    print *, "MPI commpression was disabled, so " // fn // " was not compressed."
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

M = [N(1) - 10, N(2) - 20, 4]

dx2 = M(2) / Nmpi

allocate(A(N(1), dx2, 4))

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
if(any(chunks /= [10, 123, 2]))  then
  write(stderr, '(a,3I5)') "expected chunks: 10,123,2 but got chunks ", chunks
  error stop '#3 auto chunk unexpected chunk size'
endif

call h5f%close()

if(mpi_id == 0) then
  inquire(file=fn, size=fsize)
  crat = (N(1) * N(2) * storage_size(A) / 8) / fsize

  print '(A,F6.2,A,I6)','#3 filesize (Mbytes): ',fsize / 1e6, '   3D compression ratio:',crat

  if (h5f%parallel_compression) then
    if(crat < 5) error stop fn // ' low compression'
  else
    print *, "MPI commpression was disabled, so " // fn // " was not compressed."
  endif
endif

end subroutine test_deflate_slice


subroutine test_get_deflate(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h5f

call h5f%open(fn, action='r', mpi=.false.)
!! bug in HDF5? only works with MPI=.false.

if (h5f%parallel_compression) then
  if (.not. h5f%deflate("/A")) error stop "expected deflate MPI"
else
  write(stderr,'(a)') "MPI compression was disabled, so " // fn // " was not compressed."
endif

if (.not. h5f%deflate("/noMPI")) error stop "expected deflate as dataset was written without MPI"

call h5f%close()

end subroutine test_get_deflate


end program
