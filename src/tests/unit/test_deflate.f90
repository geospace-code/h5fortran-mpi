program test_deflate
!! unit tests and registration tests of HDF5 deflate compression write
use, intrinsic:: iso_fortran_env, only: int32, real32, real64, stderr=>error_unit
use h5mpi, only: hdf5_file
use hdf5, only: H5D_CHUNKED_F, H5D_CONTIGUOUS_F, hsize_t

implicit none (type, external)

character(*), parameter :: fn1='deflate1.h5', fn2='deflate2.h5', fn3='deflate3.h5', fn4='deflate4.h5'
integer, parameter :: N=1000

call test_deflate_props(fn1, N)
print *,'OK: HDF5 compression props'

call test_deflate_whole(fn2, N)

call test_deflate_slice(fn3, N)

call test_deflate_chunk_size(fn4)

call test_get_deflate(fn1)
print *, 'OK: HDF5 get deflate'

contains

subroutine test_deflate_props(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N

type(hdf5_file) :: h5f
integer(hsize_t) :: crat, chunks(3)
integer ::  fsize, layout

real(real32), allocatable :: big2(:,:)

allocate(big2(N,N))

big2 = 0

call h5f%open(fn, action='w', comp_lvl=1, debug=.true.)
call h5f%write('/big2', big2) !, chunk_size=[100,100])
call h5f%write('/small_contig', big2(:5,:5))
call h5f%close()

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(big2)/8) / fsize
print '(A,F6.2,A,I6)','filesize (Mbytes): ',fsize/1e6, '   2D compression ratio:',crat
if (h5f%comp_lvl > 0 .and. crat < 10) error stop '2D low compression'

call h5f%open(fn, action='r', debug=.false.)

layout = h5f%layout('/big2')
if(layout /= H5D_CHUNKED_F) error stop '#1 not chunked layout'
if(.not.h5f%is_chunked('/big2')) error stop '#1 not chunked layout'
call h5f%chunks('/big2', chunks(:2))
! if(any(chunks(:2) /= [100, 100])) then
if(any(chunks(:2) /= [63, 63])) then
  write(stderr, '(a,2I5)') "expected chunks: 63, 63 but got chunks ", chunks(:2)
  error stop '#1 get_chunk mismatch'
endif
layout = h5f%layout('/small_contig')
if(layout /= H5D_CONTIGUOUS_F) error stop '#1 not contiguous layout'
if(.not.h5f%is_contig('/small_contig')) error stop '#1 not contig layout'
call h5f%chunks('/small_contig', chunks(:2))
if(any(chunks(:2) /= -1)) error stop '#1 get_chunk mismatch'

call h5f%close()

end subroutine test_deflate_props


subroutine test_deflate_whole(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N

type(hdf5_file) :: h5f
real, allocatable :: big3(:,:,:)
integer(hsize_t) :: crat, chunks(3)
integer :: fsize

allocate(big3(N,N,4))

call h5f%open(fn, action='w',comp_lvl=1, debug=.true.)
call h5f%write('/big3', big3) !, chunk_size=[100,100,1])

call h5f%write('/big3_autochunk', big3)
call h5f%chunks('/big3_autochunk', chunks)
if(any(chunks /= [63,125,1])) error stop '#2 auto chunk unexpected chunk size'

call h5f%close()

inquire(file=fn, size=fsize)
crat = (2*N*N*storage_size(big3)/8) / fsize

print '(A,F6.2,A,I6)','#2 filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '#2 3D low compression'
end subroutine test_deflate_whole


subroutine test_deflate_slice(fn, N)

character(*), intent(in) :: fn
integer, intent(in) :: N

type(hdf5_file) :: h5f
integer, allocatable :: ibig3(:,:,:)
integer(hsize_t) :: crat, chunks(3)
integer :: fsize

allocate(ibig3(N,N,4))

ibig3 = 0

call h5f%open(fn, action='w',comp_lvl=1, debug=.true.)

call h5f%write('/ibig3', ibig3(:N-10,:N-20,:))
call h5f%chunks('/ibig3', chunks)
if(any(chunks /= [62,123,1])) error stop '#3 auto chunk unexpected chunk size'

call h5f%close()

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(ibig3)/8) / fsize

print '(A,F6.2,A,I6)','#3 filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '#3 3D low compression'

end subroutine test_deflate_slice


subroutine test_deflate_chunk_size(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h5f
integer, allocatable :: ibig2(:,:)
integer(hsize_t) :: crat
integer :: fsize

allocate(ibig2(N,N))

ibig2 = 0

call h5f%open(fn, action='w',comp_lvl=1, debug=.true.)
call h5f%write('/ibig2', ibig2) !, chunk_size=[100,100])
call h5f%close()

inquire(file=fn, size=fsize)
crat = (N*N*storage_size(ibig2)/8) / fsize

print '(A,F6.2,A,I6)','#4 filesize (Mbytes): ',fsize/1e6, '   3D compression ratio:',crat

if (h5f%comp_lvl > 0 .and. crat < 10) error stop '#4 3D low compression'

end subroutine test_deflate_chunk_size


subroutine test_get_deflate(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h5f

call h5f%open(fn, action='r', debug=.true.)

if (.not. h5f%deflate("/big2")) error stop "expected deflate"

call h5f%close()

end subroutine test_get_deflate

end program
