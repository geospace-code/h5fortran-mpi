program test_scalar

use, intrinsic :: iso_fortran_env, only : real32, real64, int32

use h5fortran, only : hdf5_file
use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

implicit none (type, external)

external :: mpi_finalize

integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"

call test_simple_write('test_write.h5')
if(mpi_id == 0) print *, "OK: test simple write"

if(mpi_id == 0) then
  call test_layout_write('test_layout.h5')
  print *, "OK: test layout write"
endif

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"


contains


subroutine test_simple_write(fn)
character(*), intent(in) :: fn

type(hdf5_file) :: h5
integer(int32) :: d0, d1(1), d2(1,2), d3(1,2,3), d4(1,2,3,4), d5(1,2,3,4,5), d6(1,2,3,4,5,6), d7(1,2,3,4,5,6,7)

call h5%open(fn, action="w", mpi=.true.)

!> just to have some default value
d0 = 0
d1 = 1
d2 = 2
d3 = 3
d4 = 4
d5 = 5
d6 = 6
d7 = 7

call h5%write("/d0", d0)
call h5%write("/d1", d1)
call h5%write("/d2", d2)
call h5%write("/d3", d3)
call h5%write("/d4", d4)
call h5%write("/d5", d5)
call h5%write("/d6", d6)
call h5%write("/d7", d7)

call h5%close()

end subroutine test_simple_write


subroutine test_layout_write(fn)
!! NOTE: compact datsets do not work for collective writes, at least for HDF5 1.12.2. The dataset will be all zero.
character(*), intent(in) :: fn

type(hdf5_file) :: h

real(real32), dimension(1,1,1,1,1,1,1) :: d7_32
real(real64), dimension(1,1,1,1,1,1,1) :: d7_64

d7_32 = 42
d7_64 = 42


call h%open(fn, action="w", mpi=.false.)

call h%write("/compact1d", [1,2,3], compact=.true.)
call h%write("/contig1d", [1,2,3], compact=.false.)

call h%write("/compact0d", 42_int32, compact=.true.)
call h%write("/compact7d_32", d7_32, compact=.true.)
call h%write("/compact7d_64", d7_64, compact=.true.)

call h%write('/compact_r32', 142._real32, compact=.true.)
call h%write('/compact_r64', 142._real64, compact=.true.)
call h%write('/compact_i32', 142_int32, compact=.true.)

call h%close()

end subroutine test_layout_write

end program
