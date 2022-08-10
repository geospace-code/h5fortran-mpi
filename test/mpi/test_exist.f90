program exist_tests
!! test "exist" variable

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank, mpi_barrier

use h5fortran, only: hdf5_file, h5exist, is_hdf5, hdf5_close


implicit none (type, external)

external :: mpi_finalize

integer :: ierr, mpi_id


call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr /= 0) error stop "mpi_comm_rank"

call test_is_hdf5()
if(mpi_id == 0) print *, 'OK: is_hdf5'

call test_exist('exist.h5')
if(mpi_id == 0) print *, 'OK: exist'

call test_softlink('soft.h5')
if(mpi_id == 0) print *, "OK: softlink"

call test_multifiles()
if(mpi_id == 0) print *, 'OK: multiple files open at once'

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"


contains


subroutine test_is_hdf5()
integer :: i

if(is_hdf5('apidfjpj-8j9ejfpq984jfp89q39SHf.h5')) error stop 'test_exist: non-existent file declared hdf5'

if(mpi_id == 0) then
  open(newunit=i, file='not_hdf5.h5', action='write')
  write(i,*) 'I am not an HDF5 file.'
  close(i)
endif

call mpi_barrier(MPI_COMM_WORLD, i)
if(i /= 0) error stop "ERROR: test_is_hdf5: mpi_barrier"


if(is_hdf5('not_hdf5.h5')) error stop 'text files are not hdf5'

end subroutine test_is_hdf5


subroutine test_exist(fn)

type(hdf5_file) :: h

character(*), intent(in) :: fn

call h%open(fn, "w", mpi=.true.)
call h%write('/x', 42)
call h%close()

if(.not.is_hdf5(fn)) error stop fn // ' does not exist'

call h%open(fn, "r", mpi=.true.)
if (.not. h%exist('/x')) error stop fn // ' /x exists'

if (h%exist('/A')) error stop 'variable /A should not exist in ' // fn

call h%close()

if(h%is_open()) error stop 'file is closed'

if (.not. h5exist(fn, '/x', mpi=.true.)) error stop 'x exists'
if (h5exist(fn, '/A', mpi=.true.)) error stop 'A not exist'

end subroutine test_exist


subroutine test_softlink(fn)

type(hdf5_file) :: h
character(*), intent(in) :: fn

integer :: y

call h%open(fn, action='w', mpi=.true.)

call h%write("/actual", 142)
call h%softlink("/actual", "/additional")
call h%read("/additional", y)

if (.not.h%exist("/additional")) error stop "softlink not present"

if (y /= 142) error stop "did not read softlink correctly"

!> test dangling link

call h%softlink("/not_here", "/not_yet")
if (h%exist("/not_yet")) error stop "dangling softlink"

call h%write("/not_here", 36)
call h%read("/not_yet", y)
if (y /= 36)  error stop "finalizing dangling link failed"

call h%close()

end subroutine test_softlink


subroutine test_multifiles()

type(hdf5_file) :: f,g,h

call f%open(filename='A.h5', action='w', mpi=.true.)
call g%open(filename='B.h5', action='w', mpi=.true.)
if (h%is_open()) error stop 'is_open not isolated at constructor'
call h%open(filename='C.h5', action='w', mpi=.true.)

call f%flush()

call f%close()
if (.not.g%is_open() .or. .not. h%is_open()) error stop 'is_open not isolated at destructor'
call g%close()
call h%close()

call hdf5_close()

end subroutine test_multifiles

end program
