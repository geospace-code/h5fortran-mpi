module h5mpi

use, intrinsic :: iso_fortran_env, only : real32, real64, stderr=>error_unit

use mpi, only : MPI_COMM_WORLD, MPI_INFO_NULL
use hdf5

implicit none

integer, parameter :: mpi_h5comm = MPI_COMM_WORLD, mpi_h5info = MPI_INFO_NULL

type :: hdf5_file

character(:), allocatable :: filename
integer(HID_T) :: file_id
logical :: is_open = .false.
logical :: use_mpi = .true.

integer :: comp_lvl = 0 !< compression level (1-9)  0: disable compression

contains

procedure, public :: open => ph5open, close => ph5close, flush => hdf_flush
!! procedures without mapping

generic, public :: write => ph5write2d_r32, ph5write3d_r32
!! mapped procedures

procedure,private :: ph5write2d_r32, ph5write3d_r32
!! mapped procedures must be declared again like this

end type hdf5_file

private
public :: mpi_h5comm, hdf5_file

interface !< write.f90

module subroutine ph5write2d_r32(self, dname, A, dims_file)
!! A is the subset of the array to write to dataset "dname" from this process

class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A(:,:)
integer(HSIZE_T), intent(in) :: dims_file(rank(A))  !< full disk shape of A (not just per worker)
end subroutine ph5write2d_r32

module subroutine ph5write3d_r32(self, dname, A, dims_file)
!! A is the subset of the 3D array to write to dataset "dname" from this process

class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A(:,:,:)
integer(HSIZE_T), intent(in) :: dims_file(rank(A))  !< full disk shape of A (not just per worker)
end subroutine ph5write3d_r32

end interface

contains


subroutine ph5open(self, filename, action, mpi)
!! collective: open/create file
!!
!! PARAMETERS:
!! ----------
!! filename
!! action: "r", "w", "rw"

class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: filename
character(*), intent(in), optional :: action
logical, intent(in), optional :: mpi

character(len=2) :: laction
integer :: ierr
integer(HID_T) :: plist_id
logical :: exists

laction = "rw"
if (present(action)) laction = action

if (present(mpi)) self%use_mpi = mpi

call h5open_f(ierr)
if(ierr/=0) error stop "h5open: could not open HDF5 library"
!! OK to call repeatedly
!! https://support.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Open

inquire(file=filename, exist=exists)
if (action(1:1) == "r".and..not.exists) error stop "h5open: file does not exist: " // filename

if(self%use_mpi) then
  !! collective: setup for MPI access
  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
  call h5pset_fapl_mpio_f(plist_id, mpi_h5comm, mpi_h5info, ierr)
else
  plist_id = H5P_DEFAULT_F
endif

!> from h5fortran
select case(action)
case('r')
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, self%file_id, ierr, access_prp=plist_id)
case('r+')
  call h5fopen_f(filename, H5F_ACC_RDWR_F, self%file_id, ierr, access_prp=plist_id)
case('rw', 'a')
  if(exists) then
    call h5fopen_f(filename, H5F_ACC_RDWR_F, self%file_id, ierr, access_prp=plist_id)
  else
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%file_id, ierr, access_prp=plist_id)
  endif
case ('w')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%file_id, ierr, access_prp=plist_id)
case default
  error stop 'Unsupported action: ' // action
end select

if(ierr/=0) error stop "h5open/create: could not initialize HDF5 file: " // filename

call h5pclose_f(plist_id, ierr)
if(ierr/=0) error stop "h5pclode: " // filename

self%filename = filename
self%is_open = .true.

end subroutine ph5open



subroutine ph5close(self, ierr, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

class(hdf5_file), intent(inout) :: self
logical, intent(in), optional :: close_hdf5_interface
integer, intent(out), optional :: ierr
integer :: ier

if (.not. self%is_open) then
  write(stderr,*) 'WARNING:h5fortran:finalize: file handle is already closed: '// self%filename
  return
endif

!> close hdf5 file
call h5fclose_f(self%file_id, ier)
if (present(ierr)) ierr = ier
if (check(ier, 'ERROR:finalize: HDF5 file close: ' // self%filename)) then
  if (present(ierr)) return
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ier)
    if (present(ierr)) ierr = ier
    if (check(ier, 'ERROR: HDF5 library close')) then
      if (present(ierr)) return
      error stop
    endif
  endif
endif

!> sentinel lid
self%file_id = 0

self%is_open = .false.

end subroutine ph5close


subroutine hdf_flush(self, ierr)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
class(hdf5_file), intent(in) :: self
integer, intent(out), optional :: ierr
integer :: ier

call h5fflush_f(self%file_id, H5F_SCOPE_GLOBAL_F, ier)

if (present(ierr)) ierr = ier
if (check(ier, 'ERROR: HDF5 flush ' // self%filename) .and. .not.present(ierr)) error stop

end subroutine hdf_flush


logical function check(ierr, filename, dname)
integer, intent(in) :: ierr
character(*), intent(in), optional :: filename, dname

character(:), allocatable :: fn, dn

check = .false.
if (ierr==0) return

check = .true.
fn = ""
dn = ""
if (present(filename)) fn = filename
if (present(dname)) dn = dname

write(stderr,*) 'ERROR: ' // fn // ':' // dn // ' error code ', ierr

end function check

end module h5mpi
