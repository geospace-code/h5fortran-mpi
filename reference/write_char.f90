module proto

use mpi, only : MPI_INFO_NULL, MPI_COMM_WORLD
use hdf5

implicit none (type, external)

private
public :: create_file, create_dataset, write_dataset

contains

subroutine create_file(fn, file_id)

character(*), intent(in) :: fn
integer(HID_T), intent(out) :: file_id

integer(HID_T) ::  fapl
integer :: ierr

! collective: setup for MPI access
call h5pcreate_f(H5P_FILE_ACCESS_F, fapl, ierr)
call h5pset_fapl_mpio_f(fapl, MPI_COMM_WORLD, MPI_INFO_NULL, ierr)

! collective: create file
call h5fcreate_f(fn, H5F_ACC_TRUNC_F, file_id, ierr, access_prp = fapl)
if(ierr/=0) error stop "h5fcreate"
call h5pclose_f(fapl, ierr)
if(ierr/=0) error stop "h5pclose"

end subroutine create_file


subroutine create_dataset(dset_name, file_id, dtype, dset_id, filespace, type_id, charlen)

character(*), intent(in) :: dset_name
integer(HID_T), intent(in) :: file_id, dtype
integer(HID_T), intent(out) :: dset_id, filespace
integer(HID_T), optional, intent(out) :: type_id
integer, optional, intent(in) :: charlen

integer :: ierr
integer(HID_T) :: dcpl

dcpl = H5P_DEFAULT_F

call h5screate_f(H5S_SCALAR_F, filespace, ierr)
if(ierr/=0) error stop "h5screate"

if(dtype == H5T_NATIVE_CHARACTER) then
  if(.not.present(charlen)) error stop "character needs charlen"
  if(.not.present(type_id)) error stop "character needs type_id"
  ! define character
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER, type_id, ierr)
  if(ierr/=0) error stop "h5tcopy"
  CALL h5tset_size_f(type_id, int(charlen, SIZE_T), ierr)
  if(ierr/=0) error stop "h5tset_size"
else
  type_id = dtype
endif

! collective: create dataset
call h5dcreate_f(file_id, dset_name, type_id, filespace, dset_id, ierr, dcpl_id=dcpl)
if(ierr/=0) error stop "h5dcreate"

call h5pclose_f(dcpl, ierr)
if (ierr /= 0) error stop "h5pclose"

end subroutine create_dataset


subroutine write_dataset(dset_id, type_id, data, dset_dims)

integer(HID_T), intent(in) :: dset_id, type_id
class(*), intent(in) :: data
integer(HSIZE_T), intent(in) :: dset_dims(:)

integer(HID_T) :: dxpl
integer :: ierr

  ! Create property list for collective dataset write
call h5pcreate_f(H5P_DATASET_XFER_F, dxpl, ierr)
if (ierr/=0) error stop "h5pcreate"
call h5pset_dxpl_mpio_f(dxpl, H5FD_MPIO_COLLECTIVE_F, ierr)
if (ierr/=0) error stop "h5pset_dxpl"

! For independent write use
! call h5pset_dxpl_mpio_f(dcpl, H5FD_MPIO_INDEPENDENT_F, ierr)

! collective: Write dataset
select type(data)
type is (character(*))
  call h5dwrite_f(dset_id, type_id, data, dset_dims, ierr, xfer_prp = dxpl)
class default
  error stop "write_dataset: unhandled type"
end select
if (ierr/=0) error stop "h5dwrite"


call h5tclose_f(type_id, ierr)
if(ierr/=0) error stop "h5tclose"
call h5pclose_f(dxpl, ierr)
if (ierr/=0) error stop "h5pclose"
call h5dclose_f(dset_id, ierr)
if (ierr/=0) error stop "h5dclose"

end subroutine write_dataset

end module proto


program simple
!! this example has undesirable effect that all workers must have copy of data.
!! we "broadcast" our simulated data here implicitly
!! a more optimal case is to use hyperslabs with each worker

use mpi, only : mpi_init
use hdf5
use proto

implicit none (type, external)

external :: mpi_finalize

character(:), allocatable :: fn

integer(HID_T) :: file_id, dset_id, filespace, dcpl, type_id

character(:), allocatable :: c1, c2

integer(HSIZE_T) :: dset_dims(0)

integer :: ierr, i, j, k

call mpi_init(ierr)

fn = "char.h5"
c1 = "42"
c2 = "this is a Little string!"

call h5open_f(ierr)
if(ierr/=0) error stop "h5open"

call create_file(fn, file_id)

call create_dataset("/c1", file_id, H5T_NATIVE_CHARACTER, dset_id, filespace, type_id, len(c1))
call write_dataset(dset_id, type_id, c1, dset_dims)

call create_dataset("/c2", file_id, H5T_NATIVE_CHARACTER, dset_id, filespace, type_id, len(c2))
call write_dataset(dset_id, type_id, c2, dset_dims)

! wind down
call h5sclose_f(filespace, ierr)
if (ierr/=0) error stop "h5sclose"
call h5fclose_f(file_id, ierr)
if (ierr/=0) error stop "h5fclose"
call h5close_f(ierr)
if (ierr/=0) error stop "h5close"

call mpi_finalize(ierr)
if (ierr/=0) error stop "mpi_finalize"

print *,"OK: write collective"

end program
