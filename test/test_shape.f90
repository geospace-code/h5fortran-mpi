program test_shape
!! This program shows how HDF5 dimension orders are distinct in different languages

use, intrinsic:: iso_fortran_env, only: real64, int32, stdout=>output_unit, stderr=>error_unit

use mpi, only : mpi_init, mpi_comm_rank, MPI_COMM_WORLD

use h5mpi, only: hdf5_file,hsize_t, is_hdf5

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn = 'test_write.h5'
integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call test_shape_read(fn)
if(mpi_id==0) print *, "OK: test_shape"

call mpi_finalize(ierr)


contains


subroutine test_shape_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
integer(HSIZE_T), allocatable :: dims(:)

call h%open(fn, action='r', mpi=.true.)

!! scalar
if(h%ndim("/d0") /= 0) error stop "scalar is rank 0"
call h%shape("/d0", dims)
if (size(dims) /= 0) error stop "scalar is rank 0"

!! 1D
if(h%ndim("/d1") /= 1) error stop "1D ndim"
call h%shape("/d1", dims)
if (size(dims) /= 1) error stop "1D rank"
if (any(dims /= [1])) error stop "1D shape"

!! 2D
if(h%ndim("/d2") /= 2) error stop "2D ndim"
call h%shape("/d2", dims)
if (size(dims) /= 2) error stop "2D rank"
if (any(dims /= [1,2])) error stop "2D shape"

!! 3D
if(h%ndim("/d3") /= 3) error stop "3D ndim"
call h%shape("/d3", dims)
if (size(dims) /= 3) error stop "3D rank"
if (any(dims /= [1,2,3])) error stop "3D shape"

!! 4D
if(h%ndim("/d4") /= 4) error stop "4D ndim"
call h%shape("/d4", dims)
if (size(dims) /= 4) error stop "4D rank"
if (any(dims /= [1,2,3,4])) error stop "4D shape"

!! 5D
if(h%ndim("/d5") /= 5) error stop "5D ndim"
call h%shape("/d5", dims)
if (size(dims) /= 5) error stop "5D rank"
if (any(dims /= [1,2,3,4,5])) error stop "5D shape"

!! 6D
if(h%ndim("/d6") /= 6) error stop "6D ndim"
call h%shape("/d6", dims)
if (size(dims) /= 6) error stop "6D rank"
if (any(dims /= [1,2,3,4,5,6])) error stop "6D shape"

!! 7D
if(h%ndim("/d7") /= 7) error stop "7D ndim"
call h%shape("/d7", dims)
if (size(dims) /= 7) error stop "7D rank"
if (any(dims /= [1,2,3,4,5,6,7])) error stop "7D shape"

call h%close()

end subroutine test_shape_read

end program
