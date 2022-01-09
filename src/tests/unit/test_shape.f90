program test_shape
!! This program shows how HDF5 dimension orders are distinct in different languages

use, intrinsic:: iso_fortran_env, only: real64, stdout=>output_unit, stderr=>error_unit

use mpi, only : mpi_init, mpi_comm_rank, MPI_COMM_WORLD, mpi_barrier

use h5mpi, only: hdf5_file,hsize_t, is_hdf5

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn = 'test_shape.h5'
integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"
if(mpi_id == 0) call test_shape_write(fn)

call mpi_barrier(MPI_COMM_WORLD, ierr)
call test_shape_read(fn)
if(mpi_id==0) print *, "OK: test_shape"

call mpi_finalize(ierr)


contains


subroutine test_shape_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
integer :: d2(3,4), d7(2,1,3,4,7,6,5)

call h%open(fn, action='w', mpi=.false.)
call h%write('/d2', d2)
call h%write('/d7', d7)
call h%close()

end subroutine test_shape_write


subroutine test_shape_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
integer(HSIZE_T), allocatable :: dims(:)
integer :: d2(3,4), d7(2,1,3,4,7,6,5)

call h%open(fn, action='r', mpi=.true.)

call h%shape('/d2', dims)
if (h%ndim('/d2') /= size(dims)) then
  write(stderr,*) "2D: expected rank ", size(dims), " but got ", h%ndim('/d2')
  error stop '2D: rank /= size(dims)'
endif
if (any(dims /= shape(d2))) error stop '2-D: file shape not match variable shape'

!deallocate(dims)

call h%shape('/d7', dims)
if (h%ndim('/d7') /= size(dims)) then
  write(stderr,*) "7D: expected rank ", size(dims), " but got ", h%ndim('/d7')
  error stop '7D: rank /= size(dims)'
endif
if (any(dims /= shape(d7))) error stop '7-D: file shape not match variable shape'

call h%close()

end subroutine test_shape_read

end program
