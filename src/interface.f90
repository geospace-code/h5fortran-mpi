module h5mpi

use, intrinsic :: iso_c_binding, only : C_NULL_CHAR
use, intrinsic :: iso_fortran_env, only : real32, real64, int32, int64, stderr=>error_unit

use mpi, only : MPI_COMM_WORLD, MPI_INFO_NULL, mpi_comm_rank
use hdf5, only : &
HID_T, HSIZE_T, SIZE_T, &
H5I_FILE_F, &
H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
H5T_NATIVE_CHARACTER, H5T_NATIVE_INTEGER, H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_STD_I64LE, &
H5F_ACC_RDONLY_F, H5F_ACC_TRUNC_F, H5F_ACC_RDWR_F, H5F_SCOPE_GLOBAL_F, &
H5F_OBJ_FILE_F, H5F_OBJ_GROUP_F, H5F_OBJ_DATASET_F, H5F_OBJ_DATATYPE_F, H5F_OBJ_ALL_F, &
H5FD_MPIO_COLLECTIVE_F, &
H5P_DEFAULT_F, H5P_FILE_ACCESS_F, H5P_DATASET_CREATE_F, H5P_DATASET_XFER_F, &
H5S_ALL_F, H5S_SELECT_SET_F, &
H5D_CHUNKED_F, H5D_CONTIGUOUS_F, H5D_COMPACT_F, &
h5dcreate_f, h5dopen_f, h5dclose_f, h5dget_space_f, h5dget_create_plist_f, &
h5fopen_f, h5fclose_f, h5fcreate_f, h5fget_filesize_f, h5fflush_f, h5fis_hdf5_f, &
h5fget_obj_count_f, h5fget_obj_ids_f, h5fget_name_f, &
h5iis_valid_f, h5iget_type_f, h5iget_name_f, &
h5pcreate_f, h5pclose_f, h5pset_chunk_f, h5pset_dxpl_mpio_f, h5pset_fapl_mpio_f, h5pall_filters_avail_f, &
h5sselect_hyperslab_f, h5sselect_none_f, h5screate_simple_f, h5sclose_f, &
h5get_libversion_f, &
h5open_f, h5close_f

use h5lt, only : h5ltget_dataset_info_f, h5ltget_dataset_ndims_f, h5ltpath_valid_f

implicit none

integer, parameter :: mpi_h5comm = MPI_COMM_WORLD, mpi_h5info = MPI_INFO_NULL

type :: hdf5_file

character(:), allocatable :: filename
integer(HID_T) :: file_id

logical :: use_mpi = .false.
integer :: mpi_id = -1

logical :: debug = .false.
logical :: parallel_compression = .false.
logical :: fletcher32 = .false.
logical :: shuffle = .false.

integer :: comp_lvl = 0
!! compression level (1-9)  0: disable compression
!! compression with MPI requires MPI-3 and HDF5 >= 1.10.2

contains

procedure, public :: open => ph5open, close => ph5close, &
flush => hdf_flush, ndim => hdf_get_ndim, shape => hdf_get_shape, exist => hdf_exist, &
create => hdf_create, filesize => hdf_filesize, &
class => get_class, dtype => get_native_dtype, &
deflate => get_deflate, &
softlink => create_softlink, &
layout => hdf_get_layout, chunks => hdf_get_chunk, &
is_contig => hdf_is_contig, is_chunked => hdf_is_chunked, is_compact => hdf_is_compact, &
is_open, write_group
!! procedures without mapping

generic, public :: write => h5write_scalar,ph5write_1d, ph5write_2d, ph5write_3d, ph5write_4d, ph5write_5d, ph5write_6d, ph5write_7d

generic, public :: read => h5read_scalar, ph5read_1d, ph5read_2d, ph5read_3d, ph5read_4d, ph5read_5d, ph5read_6d, ph5read_7d
!! mapped procedures

!> write attributes
generic, public :: writeattr => writeattr_char, writeattr_num

!> read attributes
generic, public :: readattr => readattr_char, readattr_num

procedure, private :: writeattr_char, writeattr_num, readattr_char, readattr_num

procedure, private :: h5write_scalar, ph5write_1d, ph5write_2d, ph5write_3d, ph5write_4d, ph5write_5d, ph5write_6d, ph5write_7d

procedure, private :: h5read_scalar, ph5read_1d, ph5read_2d, ph5read_3d, ph5read_4d, ph5read_5d, ph5read_6d, ph5read_7d
!! mapped procedures must be declared again like this

!> flush file to disk and close file if user forgets to do so.
final :: destructor

end type hdf5_file


type :: mpi_tags

integer :: a2=102, a3=103

end type mpi_tags

private
public :: mpi_h5comm, hdf5_file, mpi_tags, has_parallel_compression, is_hdf5, &
hdf_rank_check, hdf_shape_check, mpi_collective, mpi_hyperslab, &
hdf5version, h5exist, hdf5_close, &
HSIZE_T, &
H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE

interface !< write.f90
module subroutine hdf_create(self, dname, dtype, mem_dims, dset_dims, &
  filespace, memspace, dset_id, dtype_id, &
  istart, iend, chunk_size, compact, charlen)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(out) :: filespace, memspace, dset_id
integer(HID_T), intent(out), optional :: dtype_id
integer(HSIZE_T), dimension(:), intent(in), optional :: istart, iend
integer, dimension(:), intent(in), optional :: chunk_size
logical, intent(in), optional :: compact
integer, intent(in), optional :: charlen !< length of character scalar
end subroutine hdf_create

module subroutine write_group(self, group_path)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: group_path   !< full path to group
end subroutine write_group

module subroutine create_softlink(self, tgt, link)
class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: tgt, &  !< target path to link
                            link  !< soft link path to create
end subroutine create_softlink

module subroutine hdf_flush(self)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
class(hdf5_file), intent(in) :: self
end subroutine hdf_flush

end interface

interface !< hdf5_config.f90
module subroutine get_hdf5_config(parallel_compression)
logical, intent(out) :: parallel_compression
end subroutine get_hdf5_config
end interface


interface !< writer.f90

module subroutine h5write_scalar(self, dname, value, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value
logical, intent(in), optional :: compact
end subroutine h5write_scalar

module subroutine ph5write_1d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:)
class(*), intent(in), dimension(1), optional :: dset_dims  !< integer or integer(HSIZE_T) full disk shape (not just per worker)
integer(HSIZE_T), intent(in), dimension(1), optional :: istart, iend
integer, intent(in), dimension(1), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_1d

module subroutine ph5write_2d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:)
class(*), intent(in), dimension(2), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(2), optional :: istart, iend
integer, intent(in), dimension(2), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_2d

module subroutine ph5write_3d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:)
class(*), intent(in), dimension(3), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(3), optional :: istart, iend
integer, intent(in), dimension(3), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_3d

module subroutine ph5write_4d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:)
class(*), intent(in), dimension(4), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(4), optional :: istart, iend
integer, intent(in), dimension(4), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_4d

module subroutine ph5write_5d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:)
class(*), intent(in), dimension(5), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(5), optional :: istart, iend
integer, intent(in), dimension(5), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_5d

module subroutine ph5write_6d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:)
class(*), intent(in), dimension(6), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(6), optional :: istart, iend
integer, intent(in), dimension(6), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_6d

module subroutine ph5write_7d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
class(*), intent(in), dimension(7), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(7), optional :: istart, iend
integer, intent(in), dimension(7), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine ph5write_7d

end interface


interface !< read.f90

module integer function get_class(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function get_class

module integer(hid_t) function get_native_dtype(self, dname, ds_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hid_t), intent(in), optional :: ds_id
end function get_native_dtype

module integer function hdf_get_ndim(self, dname) result (drank)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_get_ndim

module subroutine hdf_get_shape(self, dname, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(out), allocatable :: dims(:)
end subroutine hdf_get_shape

module subroutine h5open_read(self, dname, dims, dset_dims, filespace, memspace, dset_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer(HSIZE_T), intent(out) :: dset_dims(:)
integer(HID_T), intent(out) :: filespace, memspace, dset_id
end subroutine h5open_read

module logical function get_deflate(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function get_deflate

module integer function hdf_get_layout(self, dname) result(layout)
!! H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_get_layout

module subroutine hdf_get_chunk(self, dname, chunk_size)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hsize_t), intent(out) :: chunk_size(:)
end subroutine hdf_get_chunk

end interface


interface !< reader.f90
!! the read "value" are intent(inout) because:
!! * arrays: to work correctly when actual argument is allocatable
!! * scalar: to work correctly with character type

module logical function h5exist(filename, dname, mpi)
character(*), intent(in) :: filename, dname
logical, intent(in) :: mpi
end function h5exist

module subroutine h5read_scalar(self, dname, value)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout)        :: value
end subroutine h5read_scalar

module subroutine ph5read_1d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:)
integer(HSIZE_T), intent(in), dimension(1), optional :: istart, iend
end subroutine ph5read_1d

module subroutine ph5read_2d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:)
integer(HSIZE_T), intent(in), dimension(2), optional :: istart, iend
end subroutine ph5read_2d

module subroutine ph5read_3d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:)
integer(HSIZE_T), intent(in), dimension(3), optional :: istart, iend
end subroutine ph5read_3d

module subroutine ph5read_4d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:)
integer(HSIZE_T), intent(in), dimension(4), optional :: istart, iend
end subroutine ph5read_4d

module subroutine ph5read_5d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:)
integer(HSIZE_T), intent(in), dimension(5), optional :: istart, iend
end subroutine ph5read_5d

module subroutine ph5read_6d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:)
integer(HSIZE_T), intent(in), dimension(6), optional :: istart, iend
end subroutine ph5read_6d

module subroutine ph5read_7d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:,:)
integer(HSIZE_T), intent(in), dimension(7), optional :: istart, iend
end subroutine ph5read_7d

end interface


interface  !< attributes.f90

module subroutine readattr_char(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
character(*), intent(inout) :: attrval
!! intent(inout) for character
end subroutine readattr_char

module subroutine readattr_num(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(inout) :: attrval(:)
end subroutine readattr_num

module subroutine writeattr_char(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
character(*), intent(in) :: attrval
end subroutine writeattr_char

module subroutine writeattr_num(self, dname, attr, attrval)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname, attr
class(*), intent(in) :: attrval(:)
end subroutine writeattr_num

end interface


contains


subroutine ph5open(self, filename, action, mpi, comp_lvl, shuffle, fletcher32, debug)
!! collective: open/create file
!!
!! PARAMETERS:
!! ----------
!! filename
!! action: "r", "w", "rw"

class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: filename
character(*), intent(in), optional :: action
logical, intent(in) :: mpi
integer, intent(in), optional :: comp_lvl
logical, intent(in), optional :: shuffle
logical, intent(in), optional :: fletcher32
logical, intent(in), optional :: debug


character(len=2) :: laction
integer :: ierr
integer(HID_T) :: fapl !< file access property list

laction = "rw"
if (present(action)) laction = action

self%use_mpi = mpi

if(self%use_mpi) then
  call mpi_comm_rank(MPI_COMM_WORLD, self%mpi_id, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:open: could not get MPI ID"
endif

if(present(debug)) self%debug = debug

call get_hdf5_config(self%parallel_compression)
if(self%use_mpi .and. .not. self%parallel_compression .and. self%comp_lvl > 0) then
  write(stderr, '(a)') "h5fortran:open: parallel compression is NOT available"
  !! don't set to 0 because non-MPI writes can compress.
  !! We warn again and disable compression for each attempted MPI compress write.
endif

!> compression parameter
if(present(comp_lvl) .and. laction /= "r") self%comp_lvl = comp_lvl
if(self%comp_lvl > 0) then
  self%shuffle = .true.
  self%fletcher32 = .true.
endif

if(present(shuffle)) self%shuffle = shuffle
if(present(fletcher32)) self%fletcher32 = fletcher32

if(self%comp_lvl < 0) then
  write(stderr, '(a)') "h5fortran:open: compression level must be >= 0, setting comp_lvl = 0"
  self%comp_lvl = 0
elseif(self%comp_lvl > 9) then
  write(stderr, '(a)') "h5fortran:open: compression level must be <= 9, setting comp_lvl = 9"
  self%comp_lvl = 9
endif

call h5open_f(ierr)
if(ierr/=0) error stop "h5open: could not open HDF5 library"
!! OK to call repeatedly
!! https://support.hdfgroup.org/HDF5/doc/RM/RM_H5.html#Library-Open

if(self%use_mpi) then
  !! collective: setup for MPI access
  call h5pcreate_f(H5P_FILE_ACCESS_F, fapl, ierr)
  if(ierr/=0) error stop "h5open:h5pcreate could not collective open property"
  call h5pset_fapl_mpio_f(fapl, mpi_h5comm, mpi_h5info, ierr)
  if(ierr/=0) error stop "h5open:h5pset_fapl_mpio could not collective open file"
else
  fapl = H5P_DEFAULT_F
endif

select case(laction)
case('r')
  if(.not. is_hdf5(filename)) error stop "h5fortran:open: file does not exist: "//filename
  call h5fopen_f(filename, H5F_ACC_RDONLY_F, self%file_id, ierr, access_prp=fapl)
case('r+')
  if(.not. is_hdf5(filename)) error stop "h5fortran:open: file does not exist: "//filename
  call h5fopen_f(filename, H5F_ACC_RDWR_F, self%file_id, ierr, access_prp=fapl)
case('rw', 'a')
  if(is_hdf5(filename)) then
    call h5fopen_f(filename, H5F_ACC_RDWR_F, self%file_id, ierr, access_prp=fapl)
  else
    call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%file_id, ierr, access_prp=fapl)
  endif
case ('w')
  call h5fcreate_f(filename, H5F_ACC_TRUNC_F, self%file_id, ierr, access_prp=fapl)
case default
  error stop 'h5fortran:open Unsupported action= ' // laction
end select

if(ierr/=0) error stop "h5open/create: could not initialize HDF5 file: " // filename // " action: " // laction

if(fapl /= H5P_DEFAULT_F) then
  call h5pclose_f(fapl, ierr)
  if(ierr/=0) error stop "h5mpi:open:h5pclose: " // filename
endif

self%filename = filename

end subroutine ph5open



subroutine ph5close(self, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

class(hdf5_file), intent(in) :: self
logical, intent(in), optional :: close_hdf5_interface

integer :: ierr, i
integer(SIZE_T) :: Ngroup, Ndset, Ndtype, Nfile, Lf_name, Lds_name
integer(HID_T), allocatable :: obj_ids(:)
integer(SIZE_T), parameter :: L = 2048 !< arbitrary length
character(L) :: file_name, dset_name

if (.not. self%is_open()) then
  write(stderr,*) 'WARNING:h5fortran:file_close: file handle is already closed: '// self%filename
  return
endif

!> ref count for better error messages, as this is more of a problem with HDF5-MPI programs
call h5fget_obj_count_f(self%file_id, H5F_OBJ_GROUP_F, Ngroup, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open groups: " // self%filename
if(Ngroup > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ngroup, " groups open: " // self%filename


call h5fget_obj_count_f(self%file_id, H5F_OBJ_DATASET_F, Ndset, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datasets: " // self%filename
if(Ndset > 0) then
  write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndset, " datasets open: " // self%filename

  allocate(obj_ids(Ndset))
  call h5fget_obj_ids_f(self%file_id, H5F_OBJ_DATASET_F, Ndset, obj_ids, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_ids: could not get open dataset ids: " // self%filename

  do i = 1, int(Ndset)
    call h5fget_name_f(obj_ids(i), file_name, Lf_name, ierr)
    if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_name: could not get filename of open dataset: " // self%filename

    call h5iget_name_f(obj_ids(i), dset_name, L, Lds_name, ierr)

    write(stderr,*) "h5fortran:close: open dataset: " // dset_name(:Lds_name) // " in file: " // file_name(:Lf_name)
  end do
endif

call h5fget_obj_count_f(self%file_id, H5F_OBJ_DATATYPE_F, Ndtype, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open datatypes: " // self%filename
if(Ndtype > 0) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Ndtype, " datatypes open: " // self%filename

call h5fget_obj_count_f(self%file_id, H5F_OBJ_FILE_F, Nfile, ierr)
if(ierr /= 0) error stop "ERROR:h5fortran:close:h5fget_obj_count: could not count open files: " // self%filename
if(Nfile < 1) write(stderr,'(a,i0,a)') "ERROR:h5fortran:close: there are ", Nfile, " files open: " // self%filename

if(Ngroup > 0 .or. Ndset > 0 .or. Ndtype > 0) error stop "ERROR:h5fortran:close: hanging HID handles open: " // self%filename


!> close hdf5 file
call h5fclose_f(self%file_id, ierr)
if (ierr /= 0) then
  write(stderr,'(a,i0)') 'ERROR:h5fortran:h5fclose: HDF5 file close: ' // self%filename // ' mpi_id: ', self%mpi_id
  error stop
endif

if (present(close_hdf5_interface)) then
  if (close_hdf5_interface) then
    call h5close_f(ierr)
    if (ierr /= 0) error stop 'ERROR:h5fortran:h5close: HDF5 library close'
  endif
endif

end subroutine ph5close


logical function is_open(self)
!! check if file handle is open

class(hdf5_file), intent(in) :: self

! integer :: hid_type
integer :: ierr

call h5iis_valid_f(self%file_id, is_open, ierr)
if(ierr /= 0) error stop "h5fortran:is_open:h5iis_valid: " // self%filename

! call h5iget_type_f(self%file_id, hid_type, ierr)
! if(ierr /= 0 .or. hid_type /= H5I_FILE_F) is_open = .false.

end function is_open


logical function has_parallel_compression()
call get_hdf5_config(has_parallel_compression)
end function has_parallel_compression


subroutine destructor(self)
!! Close file and handle if user forgets to do so

type(hdf5_file), intent(in) :: self

if (.not. self%is_open()) return

print '(a)', "auto-closing " // self%filename
call self%close()

end subroutine destructor


function hdf5version() result(v)
!! tell HDF5 library version (major, minor, release)
integer :: v(3), ierr

!> get library version
call h5get_libversion_f(v(1), v(2), v(3), ierr)
if (ierr/=0) error stop 'ERROR:h5fortran: HDF5 library get version'

if ((v(2) == 10 .and. v(3) < 2) .or. v(2) < 10) then
  write(stderr,'(a,I0,a1,I0,a1,I0,/,a)') "WARNING: HDF5 >= 1.10.2 required for MPI-HDF5. Your HDF5 version: ", &
  v(1), ".", v(2), ".", v(3), &
  "https://www.hdfgroup.org/2018/03/release-of-hdf5-1-10-2-newsletter-160/"
end if

end function hdf5version


logical function hdf_exist(self, dname) result(exists)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname

integer :: ierr

if(.not. self%is_open()) error stop 'h5fortran:exist: file handle is not open: ' // self%filename

call h5ltpath_valid_f(self%file_id, dname, .true., exists, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid
if (ierr/=0) error stop 'h5fortran:check_exist: could not determine status of ' // dname // ' in ' // self%filename

end function hdf_exist


subroutine hdf5_close()
!! this subroutine will close ALL existing file handles
!! only call it at end of your program
!! "Flushes all data to disk, closes all open identifiers, and cleans up memory."
!! "Should be called by all HDF5 Fortran programs"

integer :: ier

call h5close_f(ier)
if (ier /= 0) error stop 'ERROR: h5fortran:h5close: HDF5 library close'

end subroutine hdf5_close


logical function hdf_is_contig(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname

hdf_is_contig = self%layout(dname) == H5D_CONTIGUOUS_F
end function hdf_is_contig


logical function hdf_is_compact(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname

hdf_is_compact = self%layout(dname) == H5D_COMPACT_F
end function hdf_is_compact


logical function hdf_is_chunked(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
hdf_is_chunked = self%layout(dname) == H5D_CHUNKED_F
end function hdf_is_chunked


logical function is_hdf5(filename)
!! is this file HDF5?

character(*), intent(in) :: filename
integer :: ierr

inquire(file=filename, exist=is_hdf5)
!! avoid warning/error messages
if (.not. is_hdf5) return

call h5fis_hdf5_f(filename, is_hdf5, ierr)

if (ierr/=0) is_hdf5 = .false.
!! sometimes h5fis_hdf5_f is .true. for missing file

end function is_hdf5


function id2name(id)
!! get name of object with given id

integer(HID_T) :: id
character(:), allocatable :: id2name

integer(SIZE_T) :: L
integer :: ierr

character(2048) :: name

call h5iget_name_f(id, name, len(name, SIZE_T), L, ierr)
if(ierr /= 0) error stop "h5fortran:id2name:h5iget_name"

id2name = name(:L)

end function id2name


subroutine mpi_hyperslab(mem_dims, dset_dims, dset_id, filespace, memspace, istart, iend)
!! Each process defines dataset in memory and writes it to the hyperslab in the file.

integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(in) :: dset_id
integer(HID_T), intent(inout) :: filespace, memspace
integer(HSIZE_T), dimension(:), intent(in) :: istart
integer(HSIZE_T), dimension(size(istart)), intent(in) :: iend

integer(HSIZE_T), dimension(size(mem_dims)) :: c_mem_dims, i0
integer(HID_T) :: dcpl
integer :: ierr
logical :: filters_OK
character(:), allocatable :: dset_name

dset_name = id2name(dset_id)

!> check that all necessary filters to access dataset are available on the system.
call h5dget_create_plist_f(dset_id, dcpl, ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5dget_create_plist: " // dset_name

call h5pall_filters_avail_f(dcpl, filters_OK, ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5pall_filters_avail: " // dset_name
if (.not. filters_OK) then
  error stop "h5fortran: filter(s) missing necessary for dataset " // dset_name // " in parallel with MPI. This is " // &
    "typically caused by missing DEFLATE compression with HDF5-MPI."
endif

call h5pclose_f(dcpl, ierr)
if(ierr/=0) error stop "h5fortran:mpi_hyperslab:h5pclose: " // dset_name

if(filespace == H5S_ALL_F) then
  !> create dataspace
  call h5screate_simple_f(rank=size(dset_dims), dims=dset_dims, space_id=filespace, hdferr=ierr)
  if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5screate_simple:filespace " // dset_name
endif

!> Select hyperslab in the file.
call h5dget_space_f(dset_id, filespace, ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5dget_space: " // dset_name


! blk(1) = 1
! blk(2:) = dset_dims(2:)

!! NOTE: 0-based hyperslab vs. 1-based Fortran
i0 = istart - 1
c_mem_dims = iend - i0

if(any(c_mem_dims /= mem_dims)) then
  write(stderr,*) "ERROR:h5fortran:mpi_hyperslab: memory size /= dataset size: check variable slice (index). " // &
    " Dset_dims:", dset_dims, "C Mem_dims", c_mem_dims
  error stop "ERROR:h5fortran:mpi_hyperslab"
endif

! print *, 'TRACE:mpi_hyperslab: ' // dset_name //': istart', i0, 'C mem_dims: ', c_mem_dims, 'mem_dims', mem_dims

if(any(c_mem_dims < 1)) error stop "h5mpi:hyperslab:non-positive hyperslab: " // dset_name

call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, &
start=i0, &
count=c_mem_dims, &
hdferr=ierr)
! stride=1, &  !< for now we don't stride data
! block=blk  !< would this help performance?

if (ierr/=0) error stop "g5fortran:mpi_hyperslab:h5sselect_hyperslab: " // dset_name

!> create memory dataspace
call h5screate_simple_f(rank=size(c_mem_dims), dims=c_mem_dims, space_id=memspace, hdferr=ierr)
if (ierr/=0) error stop "h5fortran:mpi_hyperslab:h5screate_simple:memspace " // dset_name

end subroutine mpi_hyperslab


integer(HID_T) function mpi_collective(dname) result(xfer_id)

character(*), intent(in) :: dname !< just for error messages
integer :: ierr

!! Create property list for collective dataset operations
call h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, ierr)
if (ierr/=0) error stop "h5pcreate dataset xfer: " // dname

call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ierr)
if (ierr/=0) error stop "h5pset_dxpl_mpio collective: " // dname

! For independent dataset operations
! call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_INDEPENDENT_F, ierr)

end function mpi_collective


subroutine hdf_rank_check(self, dname, mrank, vector_scalar)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: mrank
logical, intent(out), optional :: vector_scalar

integer(HSIZE_T) :: ddims(1)
integer(SIZE_T) :: type_size
integer :: ierr, drank, type_class

if(present(vector_scalar)) vector_scalar = .false.

if(.not.self%is_open()) error stop 'ERROR:h5fortran:rank_check: file handle is not open: ' // self%filename

if (.not.self%exist(dname)) error stop 'ERROR::h5fortran:rank_check: ' // dname // ' does not exist in ' // self%filename

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
integer(HSIZE_T), intent(out), optional :: dset_dims(size(dims))

integer :: ierr, type_class
integer(SIZE_T) :: type_size
integer(HSIZE_T) :: dsdims(size(dims))

call hdf_rank_check(self, dname, size(dims))

!> check for matching size, else bad reads can occur.

call h5ltget_dataset_info_f(self%file_id, dname, dims=dsdims, &
    type_class=type_class, type_size=type_size, errcode=ierr)
if (ierr/=0) error stop 'ERROR:h5fortran:shape_check: get_dataset_info ' // dname // ' read ' // self%filename

if(present(dset_dims)) dset_dims = dsdims

if(self%use_mpi) return

if(any(int(dims, int64) /= dsdims)) then
  write(stderr,*) 'ERROR:h5fortran:shape_check: shape mismatch ' // dname // ' = ', dsdims, '  variable shape =', dims
  error stop
endif

end subroutine hdf_shape_check


integer(HSIZE_T) function hdf_filesize(self)
!! returns the size of the HDF5 file in bytes
class(hdf5_file), intent(in) :: self

integer :: ierr

if(.not. self%is_open()) error stop 'h5fortran:filesize: file handle is not open: ' // self%filename

call h5fget_filesize_f(self%file_id, hdf_filesize, ierr)
if(ierr/=0) error stop "ERROR:h5fortran: could not get file size " // self%filename

end function hdf_filesize


end module h5mpi
