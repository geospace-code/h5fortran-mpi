program test_deflate_props

use, intrinsic :: iso_fortran_env, only : int64, stderr=>output_unit

use hdf5, only : H5D_CHUNKED_F, H5D_CONTIGUOUS_F
use mpi, only : mpi_init, mpi_comm_rank, mpi_comm_size, MPI_COMM_WORLD

use h5mpi, only: hdf5_file, HSIZE_T, has_parallel_compression

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn1='deflate1.h5'
integer, parameter :: N(2) = [50, 1000], &
MIN_COMP = 2  !< lots of CPUs, smaller arrays => poorer compression

integer :: ierr, mpi_id


call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call test_read_deflate_props(fn1, N, mpi_id)
if(mpi_id==0) print *,'OK: HDF5 read deflate properties'

if(mpi_id==0) then
  call test_get_deflate(fn1)
  !! only works with mpi=.false. else get h5fclose error
  print *, 'OK: HDF5 get deflate'
endif

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

contains


subroutine test_read_deflate_props(fn, N, mpi_id)

character(*), intent(in) :: fn
integer, intent(in) :: N(2), mpi_id

type(hdf5_file) :: h5f

integer ::  fsize, layout
integer(int64) :: crat
integer(HSIZE_T) :: chunks(2)

if(mpi_id == 0) then
  inquire(file=fn, size=fsize)
  crat = (N(1) * N(2) * 32 / 8) / fsize
  print '(A,F6.2,A,I6)','#1 filesize (Mbytes): ',fsize/1e6, '  compression ratio:',crat
  if (has_parallel_compression()) then
    if(crat < MIN_COMP) error stop '2D low compression'
  else
    print *, "test_read_deflate_props: MPI commpression was disabled, so " // fn // " was not compressed."
  endif
endif

call h5f%open(fn, action='r', mpi=.true.)

layout = h5f%layout('/A')
if(layout /= H5D_CHUNKED_F) error stop '#1 not chunked layout: ' // fn
if(.not.h5f%is_chunked('/A')) error stop '#1 not chunked layout: ' // fn
call h5f%chunks('/A', chunks)
if(chunks(1) /= 5) then
  write(stderr, '(a,2I5)') "expected chunks(1) = 5 but got chunks ", chunks
  error stop '#1 get_chunk mismatch'
endif
layout = h5f%layout('/small_contig')
if(layout /= H5D_CONTIGUOUS_F) error stop '#1 not contiguous layout'
if(.not.h5f%is_contig('/small_contig')) error stop '#1 not contig layout'
call h5f%chunks('/small_contig', chunks)
if(any(chunks(:2) /= -1)) error stop '#1 get_chunk mismatch'

call h5f%close()

end subroutine test_read_deflate_props


subroutine test_get_deflate(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h5f

call h5f%open(fn, action='r', mpi=.false.)

if (h5f%parallel_compression) then
  if (.not. h5f%deflate("/A")) error stop "test_get_deflate: expected deflate MPI"
else
  if (h5f%deflate("/A")) error stop "test_get_deflate: expected no deflate MPI"
endif

if (.not. h5f%deflate("/noMPI")) error stop "expected deflate as dataset was written without MPI"

call h5f%close()

end subroutine test_get_deflate

end program
