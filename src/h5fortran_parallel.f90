module h5mpi

use, intrinsic :: iso_fortran_env, only : real32, real64, int64, stderr=>error_unit

use mpi, only : MPI_COMM_WORLD, MPI_INFO_NULL
use hdf5
use h5lt, only : h5ltget_dataset_info_f, h5ltget_dataset_ndims_f, h5ltpath_valid_f

implicit none

integer, parameter :: mpi_h5comm = MPI_COMM_WORLD, mpi_h5info = MPI_INFO_NULL

type :: hdf5_file

character(:), allocatable :: filename
integer(HID_T) :: file_id
logical :: is_open = .false.
logical :: use_mpi = .false.
logical :: debug = .false.
logical :: parallel_compression = .false.

integer :: comp_lvl = 0
!! compression level (1-9)  0: disable compression
!! compression with MPI requires MPI-3 and HDF5 >= 1.10.2

integer :: libversion(3)  !< major, minor, rel

contains

procedure, public :: open => ph5open, close => ph5close, &
  flush => hdf_flush, shape => h5get_shape, exist => hdf_exist, &
  create => hdf_create, filesize => hdf_filesize
!! procedures without mapping

generic, public :: write => h5write_scalar_r32,h5write_scalar_r64, &
ph5write1d_r32, ph5write2d_r32, ph5write3d_r32, ph5write4d_r32, &
ph5write1d_r64, ph5write2d_r64, ph5write3d_r64, ph5write4d_r64
generic, public :: read => h5read_scalar, &
ph5read_1d, ph5read_2d, ph5read_3d

!! mapped procedures

procedure,private :: h5write_scalar_r32,h5write_scalar_r64, &
ph5write1d_r32, ph5write2d_r32, ph5write3d_r32, ph5write4d_r32, &
ph5write1d_r64, ph5write2d_r64, ph5write3d_r64, ph5write4d_r64
procedure, private :: h5read_scalar, &
ph5read_1d, ph5read_2d, ph5read_3d
!! mapped procedures must be declared again like this

end type hdf5_file


type :: mpi_tags

integer :: a2=102, a3=103

end type mpi_tags

private
public :: mpi_h5comm, hdf5_file, mpi_tags, &
check, hdf_wrapup, hdf_rank_check, hdf_shape_check

interface !< write.f90
module subroutine hdf_create(self, dname, dtype, dims, dims_file, filespace, memspace, dset_id, xfer_id, chunk_size)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), intent(in) :: dims(:), dims_file(:)
integer(HID_T), intent(out), optional :: filespace, memspace, dset_id, xfer_id
integer, intent(in), optional :: chunk_size(:)
end subroutine hdf_create
end interface

interface !< hdf5_config.f90
module subroutine get_hdf5_config(parallel_compression)
logical, intent(out) :: parallel_compression
end subroutine get_hdf5_config
end interface


interface !< writer.f90

module subroutine h5write_scalar_r32(self, dname, A)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A
end subroutine h5write_scalar_r32

module subroutine h5write_scalar_r64(self, dname, A)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: A
end subroutine h5write_scalar_r64

module subroutine ph5write1d_r32(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A(:)
integer(HSIZE_T), intent(in) :: dims_file(1)  !< full disk shape of A (not just per worker)
end subroutine ph5write1d_r32

module subroutine ph5write1d_r64(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: A(:)
integer(HSIZE_T), intent(in) :: dims_file(1)
end subroutine ph5write1d_r64

module subroutine ph5write2d_r32(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A(:,:)
integer(HSIZE_T), intent(in) :: dims_file(2)  !< full disk shape of A (not just per worker)
end subroutine ph5write2d_r32

module subroutine ph5write2d_r64(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: A(:,:)
integer(HSIZE_T), intent(in) :: dims_file(2)
end subroutine ph5write2d_r64

module subroutine ph5write3d_r32(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A(:,:,:)
integer(HSIZE_T), intent(in) :: dims_file(3)
end subroutine ph5write3d_r32

module subroutine ph5write3d_r64(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: A(:,:,:)
integer(HSIZE_T), intent(in) :: dims_file(3)
end subroutine ph5write3d_r64

module subroutine ph5write4d_r32(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real32), intent(in) :: A(:,:,:,:)
integer(HSIZE_T), intent(in) :: dims_file(4)
end subroutine ph5write4d_r32

module subroutine ph5write4d_r64(self, dname, A, dims_file)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: dname
real(real64), intent(in) :: A(:,:,:,:)
integer(HSIZE_T), intent(in) :: dims_file(4)
end subroutine ph5write4d_r64

end interface


interface !< reader.f90

module subroutine h5read_scalar(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(out)        :: value
end subroutine h5read_scalar

module subroutine ph5read_1d(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:)
end subroutine ph5read_1d

module subroutine ph5read_2d(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:)
end subroutine ph5read_2d

module subroutine ph5read_3d(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:)
end subroutine ph5read_3d

end interface

contains


subroutine ph5open(self, filename, action, mpi, comp_lvl, debug)
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
integer, intent(in), optional :: comp_lvl
logical, intent(in), optional :: debug

character(len=2) :: laction
integer :: ierr
integer(HID_T) :: plist_id
logical :: exists

laction = "rw"
if (present(action)) laction = action

if (present(mpi)) self%use_mpi = mpi

if(present(debug)) self%debug = debug

call get_hdf5_config(self%parallel_compression)
if(self%parallel_compression .and. present(comp_lvl)) self%comp_lvl = comp_lvl

call h5open_f(ierr)
if(ierr/=0) error stop "h5open: could not open HDF5 library"
!! OK to call repeatedly
!! https://support.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Open

!> get library version
call h5get_libversion_f(self%libversion(1), self%libversion(2), self%libversion(3), ierr)
if (self%debug) print '(a,i0,a1,i0,a1,i0)', 'HDF5 version: ',self%libversion(1),'.',self%libversion(2),'.',self%libversion(3)
if (check(ierr, 'ERROR:h5fortran: HDF5 library get version')) error stop
if ((self%libversion(2) == 10 .and. self%libversion(3) < 2) .or. self%libversion(2) < 10) error stop &
  "HDF5 >= 1.10.2 required for MPI parallel HDF5. " // &
  "https://www.hdfgroup.org/2018/03/release-of-hdf5-1-10-2-newsletter-160/"

inquire(file=filename, exist=exists)
if (laction(1:1) == "r".and..not.exists) error stop "h5open: file does not exist: " // filename

if(self%use_mpi) then
  !! collective: setup for MPI access
  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
  call h5pset_fapl_mpio_f(plist_id, mpi_h5comm, mpi_h5info, ierr)
else
  plist_id = H5P_DEFAULT_F
endif

!> from h5fortran
select case(laction)
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
  error stop 'Unsupported action: ' // laction
end select

if(ierr/=0) error stop "h5open/create: could not initialize HDF5 file: " // filename // " action: " // laction

call h5pclose_f(plist_id, ierr)
if(ierr/=0) error stop "h5pclose: " // filename

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


subroutine h5get_shape(self, dname, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(inout), allocatable :: dims(:)

!! must get rank before info, as "dims" must be allocated first.
integer(SIZE_T) :: type_size
integer :: type_class, drank, ier

if(.not. self%exist(dname)) error stop 'h5fortran:get_shape: ' // dname // ' does not exist in ' // self%filename

call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ier)
if (ier /= 0) error stop "h5fortran:get_shape: could not get rank of " // dname // " in " // self%filename

allocate(dims(drank))
call h5ltget_dataset_info_f(self%file_id, dname, dims=dims, &
  type_class=type_class, type_size=type_size, errcode=ier)
if (ier /= 0) error stop "h5fortran:get_shape: could not get shape of " // dname // " in " // self%filename

end subroutine h5get_shape


logical function hdf_exist(self, dname) result(exists)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname

integer :: ierr

if(.not.self%is_open) error stop 'h5fortran:exist: file handle is not open'

call h5ltpath_valid_f(self%file_id, dname, .true., exists, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid

if (ierr/=0) error stop 'h5fortran:check_exist: could not determine status of ' // dname // ' in ' // self%filename


end function hdf_exist


subroutine hdf_wrapup(filespace, memspace, dset_id, plist_id)

integer(HID_T), intent(in) :: filespace, memspace, dset_id, plist_id

integer :: ierr

call h5sclose_f(filespace, ierr)
if (memspace /= H5S_ALL_F) call h5sclose_f(memspace, ierr)

call h5dclose_f(dset_id, ierr)
call h5pclose_f(plist_id, ierr)
if(ierr/=0) error stop "closing dataset/space/property"

end subroutine hdf_wrapup


subroutine hdf_rank_check(self, dname, mrank, vector_scalar)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: mrank
logical, intent(out), optional :: vector_scalar

integer(HSIZE_T) :: ddims(1)
integer(SIZE_T) :: type_size
integer :: ierr, drank, type_class

if(present(vector_scalar)) vector_scalar = .false.

if(.not.self%is_open) error stop 'h5fortran:rank_check: file handle is not open'

if (.not.self%exist(dname)) error stop 'ERROR: ' // dname // ' does not exist in ' // self%filename

!> check for matching rank, else bad reads can occur--doesn't always crash without this check
call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ierr)
if (ierr/=0) error stop 'h5fortran:rank_check: get_dataset_ndim ' // dname // ' read ' // self%filename

if (drank == mrank) return

if (present(vector_scalar) .and. drank == 1 .and. mrank == 0) then
  !! check if vector of length 1
  call h5ltget_dataset_info_f(self%file_id, dname, dims=ddims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
  if (ierr/=0) error stop 'h5fortran:rank_check: get_dataset_info ' // dname // ' read ' // self%filename
  if (ddims(1) == 1) then
    vector_scalar = .true.
    return
  endif
endif

write(stderr,'(A,I0,A,I0)') 'h5fortran:rank_check: rank mismatch ' // dname // ' = ',drank,'  variable rank =', mrank
error stop

end subroutine hdf_rank_check


subroutine hdf_shape_check(self, dname, dims, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer(HSIZE_T), intent(out) :: dset_dims(size(dims))

integer :: ierr, type_class
integer(SIZE_T) :: type_size

call hdf_rank_check(self, dname, size(dims))

!> check for matching size, else bad reads can occur.

call h5ltget_dataset_info_f(self%file_id, dname, dims=dset_dims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
if (ierr/=0) error stop 'h5fortran:shape_check: get_dataset_info ' // dname // ' read ' // self%filename


if(any(int(dims, int64) /= dset_dims)) then
  write(stderr,*) 'h5fortran:shape_check: shape mismatch ' // dname // ' = ', dset_dims, '  variable shape =', dims
  error stop
endif

end subroutine hdf_shape_check


integer(HSIZE_T) function hdf_filesize(self)
!! returns the size of the HDF5 file in bytes
class(hdf5_file), intent(inout) :: self

integer :: ierr

logical :: close_self

close_self = .false.

if (.not. self%is_open) then
  close_self = .true.
  call self%open(self%filename, action="r", mpi=.false.)
endif
call h5fget_filesize_f(self%file_id, hdf_filesize, ierr)
if(ierr/=0) error stop "could not get file size " // self%filename

if(close_self) call self%close()

end function hdf_filesize


end module h5mpi
