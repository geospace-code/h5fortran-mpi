program main

use hdf5, only: H5T_STR_NULLPAD_F, H5T_STR_NULLTERM_F
use h5mpi, only: hdf5_file
use mpi, only : mpi_init, MPI_COMM_WORLD, mpi_comm_rank

implicit none (type, external)

external :: mpi_finalize

character(1000) :: pyp, vstr, fstr
character(4), parameter :: smiley = "😀", wink = "😉"
character(4) :: u1


integer :: i, mpi_id, ierr

type(hdf5_file) :: h

call mpi_init(ierr)
if (ierr /= 0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if(ierr/=0) error stop "mpi_comm_rank"

call get_command_argument(1, pyp, status=i)
if (i/=0) error stop "specify file to read"

call h%open(pyp, "r", mpi=.true.)

call h%read("/variable", vstr)
if(vstr /= "Hello World!") error stop "h5py read variable length failed: " // trim(vstr)
if (h%get_strpad("/variable") /= H5T_STR_NULLTERM_F) error stop "NULLTERM expected for /variable"

call h%read("/nullpad", fstr)
if(fstr /= "Hello World!") error stop "h5py read null pad failed: " // trim(fstr)
if (h%get_strpad("/nullpad") /= H5T_STR_NULLPAD_F) error stop "NULLPAD expected for /nullpad"

call h%read("/smiley", u1)
print '(a)', "smiley: " // u1
if(u1 /= smiley) error stop "test_utf8: smiley failed"

call h%read("/wink", u1)
print '(a)', "wink: " // u1
if(u1 /= wink) error stop "test_utf8: wink failed"

call h%readattr("/variable", "smiley", u1)
print '(a)', "attribute: smiley: " // u1
if(u1 /= smiley) error stop "test_utf8:attr: smiley failed"

call h%readattr("/nullpad", "wink", u1)
print '(a)', "attribute: wink: " // u1
if(u1 /= wink) error stop "test_utf8:attr: wink failed"

call h%close()

if(mpi_id == 0) print *, "OK: variable/nullpad length string read"

call mpi_finalize(ierr)
if (ierr /= 0) error stop "mpi_finalize"

end program
