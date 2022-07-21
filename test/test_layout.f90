program test_layout

use, intrinsic :: iso_fortran_env, only : real32, real64, int32, stderr => error_unit

use h5fortran, only : hdf5_file

use hdf5, only : H5D_COMPACT_F
use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn = 'test_layout.h5'
integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"

call test_layout_read(fn)
if(mpi_id == 0) print *, "OK: read layout"

call test_layout_props(fn)
if(mpi_id == 0) print *, "OK: layout props"

call mpi_finalize(ierr)

contains


subroutine test_layout_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

real(real32) :: r32
real(real64) :: r64
integer(int32) :: i32

real(real32), dimension(1,1,1,1,1,1,1) :: r7_32
real(real64), dimension(1,1,1,1,1,1,1) :: r7_64

!> read casting
call h%open(fn, action="r", mpi=.true.)

call h%read("/compact_r32", r64)
if(abs(r64 - 142) > 0.001) then
  write(stderr,*) r64
  error stop "ERROR:test_layout_read: read real32 => real64: " // fn
endif
call h%read("/compact_r64", r32)
if(abs(r32 - 142) > 0.001) error stop "ERROR:test_layout_read: read real64 => real32"
call h%read("/compact_i32", i32)
if(r32 /= 142) error stop "ERROR:test_layout_read: read int32 => int32"


call h%read("/compact7d_32", r7_64)
if (any(r7_64 /= 42)) error stop "ERROR:test_layout: read real32 => real64"

call h%read("/compact7d_64", r7_32)
if (any(r7_32 /= 42)) error stop "ERROR:test_layout: read real64 => real32"

call h%close()

end subroutine test_layout_read


subroutine test_layout_props(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h%open(fn, action="r", mpi=.true.)

if (h%layout("/compact1d") /= H5D_COMPACT_F) error stop "expected compact"
if (.not. h%is_compact("/compact1d")) error stop "1d is_compact fail"

if (.not. h%is_compact("/compact7d_32")) error stop "7d is_compact fail"
if (.not. h%is_compact("/compact0d")) error stop "0d is_compact fail"

call h%close()

end subroutine test_layout_props

end program
