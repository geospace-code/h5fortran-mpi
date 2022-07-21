module h5fortran

use, intrinsic :: iso_c_binding, only : c_ptr, c_loc
use, intrinsic :: iso_fortran_env, only : real32, real64, int32, int64, stderr=>error_unit

use hdf5, only : HID_T, SIZE_T, HSIZE_T, &
H5S_ALL_F, H5S_SELECT_SET_F, &
H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE, &
H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
H5P_DEFAULT_F

implicit none (type, external)

private

!> main type
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

procedure, public :: open => h5open
procedure, public :: close => h5close
procedure, public :: write_group
procedure, public :: create => hdf_create_user
procedure, public :: flush => hdf_flush
procedure, public :: filesize => hdf_filesize
procedure, public :: ndim => hdf_get_ndim
procedure, public :: shape => hdf_get_shape
procedure, public :: layout => hdf_get_layout
procedure, public :: chunks => hdf_get_chunk
procedure, public :: class => get_class
procedure, public :: dtype => get_native_dtype
procedure, public :: deflate => get_deflate
procedure, public :: exist => hdf_check_exist
procedure, public :: is_contig => hdf_is_contig
procedure, public :: is_chunked => hdf_is_chunked
procedure, public :: is_compact => hdf_is_compact
procedure, public :: get_strpad
procedure, public :: softlink => create_softlink
procedure, public :: is_open
procedure, public :: delete_attr => attr_delete
procedure, public :: exist_attr => attr_exist
!! procedures without mapping

!> below are procedure that need generic mapping (type or rank agnostic)

generic, public :: write => h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d
procedure, private :: h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d

generic, public :: read => h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d
procedure, private :: h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d

!> attributes
generic, public :: writeattr => writeattr_scalar, writeattr_1d, writeattr_2d, writeattr_3d, writeattr_4d, writeattr_5d, &
  writeattr_6d, writeattr_7d
procedure, private :: writeattr_scalar, writeattr_1d, writeattr_2d, writeattr_3d, writeattr_4d, writeattr_5d, &
  writeattr_6d, writeattr_7d

generic, public :: readattr => readattr_scalar, readattr_1d, readattr_2d, readattr_3d, readattr_4d, readattr_5d, &
readattr_6d, readattr_7d
procedure, private :: readattr_scalar, readattr_1d, readattr_2d, readattr_3d, readattr_4d, readattr_5d, &
readattr_6d, readattr_7d

!> flush file to disk and close file if user forgets to do so.
final :: destructor

end type hdf5_file

public :: has_parallel_compression
public :: hdf5_file, is_hdf5
public :: hdf_rank_check, hdf_shape_check, hdf5version, h5exist, hdf5_close
public :: mpi_collective, mpi_hyperslab

!! for submodules only
public :: HSIZE_T, H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE
public :: H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F

interface !< write.f90

module subroutine hdf_create_user(self, dname, dtype, dset_dims, mem_dims, chunk_size, compact, charlen, fill_value)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer, dimension(:), intent(in) :: dset_dims
integer, dimension(:), intent(in), optional :: mem_dims
integer, intent(in), dimension(:), optional :: chunk_size  !< (:) instead of size(dims) due to intel fortran quirk
logical, intent(in), optional :: compact
integer, intent(in), optional :: charlen
class(*), intent(in), optional :: fill_value
end subroutine

module subroutine hdf_create(self, dname, dtype, mem_dims, dset_dims, &
  filespace_id, memspace, dset_id, dtype_id, &
  istart, iend, stride, chunk_size, compact, charlen, fill_value)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HID_T), intent(in) :: dtype
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(out) :: filespace_id, memspace, dset_id
integer(HID_T), intent(out), optional :: dtype_id
integer, intent(in), dimension(:), optional :: chunk_size, istart, iend, stride
logical, intent(in), optional :: compact
integer, intent(in), optional :: charlen !< length of character scalar
class(*), intent(in), optional :: fill_value
end subroutine

module subroutine write_group(self, group_path)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: group_path   !< full path to group
end subroutine

module subroutine create_softlink(self, tgt, link)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: tgt, &  !< target path to link
                            link  !< soft link path to create
end subroutine

module subroutine hdf_flush(self)
!! request operating system flush data to disk.
!! The operating system can do this when it desires, which might be a while.
class(hdf5_file), intent(in) :: self
end subroutine

end interface

interface !< hdf5_config.f90
module subroutine get_hdf5_config(parallel_compression)
logical, intent(out) :: parallel_compression
end subroutine
end interface


interface !< writer.f90

module subroutine h5write_scalar(self, dname, A, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_1d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:)
integer, intent(in), dimension(1), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_2d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:,:)
integer, intent(in), dimension(2), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_3d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:,:,:)
integer, intent(in), dimension(3), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_4d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:,:,:,:)
integer, intent(in), dimension(4), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_5d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:,:,:,:,:)
integer, intent(in), dimension(5), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_6d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:,:,:,:,:,:)
integer, intent(in), dimension(6), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

module subroutine h5write_7d(self, dname, A, dset_dims, istart, iend, stride, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: A(:,:,:,:,:,:,:)
integer, intent(in), dimension(7), optional :: chunk_size, istart, iend, stride, dset_dims
logical, intent(in), optional :: compact
end subroutine

end interface


interface !< read.f90

module integer function get_class(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module integer(hid_t) function get_native_dtype(self, dname, ds_id)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(hid_t), intent(in), optional :: ds_id
end function

module integer function hdf_get_ndim(self, dname) result (drank)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module subroutine hdf_get_shape(self, dname, dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(out), allocatable :: dims(:)
end subroutine

module integer function get_strpad(self, dset_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dset_name
end function

module logical function get_deflate(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module integer function hdf_get_layout(self, dname) result(layout)
!! H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module subroutine hdf_get_chunk(self, dname, chunk_size)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(out) :: chunk_size(:)
end subroutine

module logical function hdf_check_exist(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

end interface


interface !< reader.f90
!! the read "value" are intent(inout) because:
!! * arrays: to work correctly when actual argument is allocatable
!! * scalar: to work correctly with character type

module logical function h5exist(filename, dname, mpi)
character(*), intent(in) :: filename, dname
logical, intent(in) :: mpi
end function

module subroutine h5read_scalar(self, dname, A)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout)        :: A
end subroutine

module subroutine h5read_1d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:)
integer, intent(in), dimension(1), optional :: istart, iend, stride
end subroutine

module subroutine h5read_2d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:,:)
integer, intent(in), dimension(2), optional :: istart, iend, stride
end subroutine

module subroutine h5read_3d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:,:,:)
integer, intent(in), dimension(3), optional :: istart, iend, stride
end subroutine

module subroutine h5read_4d(self, dname, A,  istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:,:,:,:)
integer, intent(in), dimension(4), optional :: istart, iend, stride
end subroutine

module subroutine h5read_5d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:,:,:,:,:)
integer, intent(in), dimension(5), optional :: istart, iend, stride
end subroutine

module subroutine h5read_6d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:,:,:,:,:,:)
integer, intent(in), dimension(6), optional :: istart, iend, stride
end subroutine

module subroutine h5read_7d(self, dname, A, istart, iend, stride)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: A(:,:,:,:,:,:,:)
integer, intent(in), dimension(7), optional :: istart, iend, stride
end subroutine

end interface


interface  !< attributes.f90

module subroutine readattr_scalar(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A
end subroutine

module subroutine readattr_1d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:)
end subroutine

module subroutine readattr_2d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:,:)
end subroutine

module subroutine readattr_3d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:,:,:)
end subroutine

module subroutine readattr_4d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:,:,:,:)
end subroutine

module subroutine readattr_5d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:,:,:,:,:)
end subroutine

module subroutine readattr_6d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:,:,:,:,:,:)
end subroutine

module subroutine readattr_7d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A(:,:,:,:,:,:,:)
end subroutine


module subroutine writeattr_scalar(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A
end subroutine

module subroutine writeattr_1d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:)
end subroutine

module subroutine writeattr_2d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:)
end subroutine

module subroutine writeattr_3d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:)
end subroutine

module subroutine writeattr_4d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:)
end subroutine

module subroutine writeattr_5d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:,:)
end subroutine

module subroutine writeattr_6d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:,:,:)
end subroutine

module subroutine writeattr_7d(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(in) :: A(:,:,:,:,:,:,:)
end subroutine


module subroutine attr_delete(self, obj_name, attr_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
end subroutine

module logical function attr_exist(self, obj_name, attr_name)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
end function

end interface


interface !< utils.f90

module function id2name(id)
!! get name of object with given id
integer(HID_T), intent(in) :: id
character(:), allocatable :: id2name
end function

module subroutine h5open(self, filename, action, mpi, comp_lvl, shuffle, fletcher32, debug)
!! collective: open/create file
!!
!! PARAMETERS:
!! ----------
!! filename
!! action: "r", "w", "rw"

class(hdf5_file), intent(inout) :: self
character(*), intent(in) :: filename
character(*), intent(in), optional :: action !< r, r+, rw, w, a
logical, intent(in) :: mpi
integer, intent(in), optional :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
logical, intent(in), optional :: shuffle
logical, intent(in), optional :: fletcher32
logical, intent(in), optional :: debug
end subroutine

module subroutine h5close(self, close_hdf5_interface)
!! This must be called on each HDF5 file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! We don't reference count because applications might also invoke HDF5
!! directly.
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

class(hdf5_file), intent(in) :: self
logical, intent(in), optional :: close_hdf5_interface
end subroutine

module logical function is_open(self)
!! check if file handle is open
class(hdf5_file), intent(in) :: self
end function

module logical function has_parallel_compression()
end function

module subroutine destructor(self)
!! Close file and handle if user forgets to do so
type(hdf5_file), intent(in) :: self
end subroutine

module function hdf5version() result(v)
!! tell HDF5 library version (major, minor, release)
integer , dimension(3) :: v
end function

module subroutine hdf5_close()
!! this subroutine will close ALL existing file handles
!! only call it at end of your program
!! "Flushes all data to disk, closes all open identifiers, and cleans up memory."
!! "Should be called by all HDF5 Fortran programs
end subroutine

module logical function hdf_is_contig(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module logical function hdf_is_compact(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module logical function hdf_is_chunked(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function

module logical function is_hdf5(filename)
!! is this file HDF5?
character(*), intent(in) :: filename
end function

module subroutine mpi_hyperslab(mem_dims, dset_dims, dset_id, filespace, memspace, istart, iend, stride)
!! Each process defines dataset in memory and writes it to the hyperslab in the file.
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(in) :: dset_id
integer(HID_T), intent(inout) :: filespace, memspace
integer, dimension(:), intent(in) :: istart
integer, dimension(size(istart)), intent(in) :: iend
integer, dimension(size(istart)), intent(in), optional :: stride
end subroutine

module subroutine hdf_rank_check(self, dname, mrank, vector_scalar)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: mrank
logical, intent(out), optional :: vector_scalar
end subroutine

module subroutine hdf_shape_check(self, dname, dims, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer(HSIZE_T), intent(out), optional :: dset_dims(size(dims))
end subroutine

module integer(HSIZE_T) function hdf_filesize(self)
!! returns the size of the HDF5 file in bytes
class(hdf5_file), intent(in) :: self
end function

end interface


interface !< mpi.f90

module integer(HID_T) function mpi_collective(dname, use_mpi) result(xfer_id)
character(*), intent(in) :: dname !< just for error messages
logical, intent(in) :: use_mpi
end function

module subroutine mpi_opener(filename, use_mpi, mpi_id, fapl)
character(*), intent(in) :: filename !< just for error messages
logical, intent(in) :: use_mpi
integer, intent(out) :: mpi_id
integer(HID_T), intent(out) :: fapl
end subroutine

end interface

end module h5fortran
