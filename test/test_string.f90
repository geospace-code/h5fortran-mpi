program test_string

use, intrinsic:: iso_fortran_env, only:  stderr=>error_unit

use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

use hdf5, only: H5T_STR_SPACEPAD_F
use h5mpi, only: hdf5_file

implicit none (type, external)

external :: mpi_finalize

character(*), parameter :: fn='test_string.h5'

integer :: ierr, mpi_id

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr /= 0) error stop "mpi_comm_rank"

call test_write(fn)
if(mpi_id == 0) print *, "OK: HDF5 string write"

call test_read(fn)
if(mpi_id == 0) print *,'OK: HDF5 string read'

call test_overwrite(fn)
if(mpi_id == 0) print *, "OK: string overwrite"

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

print *,'PASSED: HDF5 string write/read'

contains


subroutine test_write(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h

call h%open(fn, action='w', mpi=.true.)

call h%write('/little', '42')
call h%write('/MySentence', 'this is a little sentence.')
call h%write('/vector_scalar', ['vector scalar'])

call h%close()

end subroutine test_write


subroutine test_read(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
character(2) :: value
character(1024) :: val1k
character(13) :: vs

call h%open(fn, action='r', mpi=.true.)
call h%read('/little', value)

if(len_trim(value) /= 2) then
  write(stderr,'(a,i0,a)') "test_string: read length ", len_trim(value), " /= 2"
  error stop
endif
if (value /= '42') error stop 'test_string:  read/write verification failure. Value: '// value

!> check padding
if (h%get_strpad("/little") /= H5T_STR_SPACEPAD_F) error stop "SPACEPAD expected for /little"

!> longer character than data
call h%read('/little', val1k)

if (len_trim(val1k) /= 2) then
  write(stderr, '(a,i0,/,a)') 'expected character len_trim 2 but got len_trim() = ', len_trim(val1k), val1k
  error stop
endif

!> vector scalar (length 1 vector)
call h%read('/vector_scalar', vs)
if(vs /= "vector scalar") error stop "test_string: vector_scalar"

call h%close()

end subroutine test_read


subroutine test_overwrite(fn)

character(*), intent(in) :: fn

type(hdf5_file) :: h
character(2) :: v

call h%open(fn, action='rw', mpi=.true.)
call h%write('/little', '73')
call h%close()

call h%open(fn, action='r', mpi=.true.)
call h%read('/little', v)
call h%close()

if (v /= '73') error stop 'test_string:  overwrite string failure. Value: '// v // " /= 73"

end subroutine test_overwrite

end program
