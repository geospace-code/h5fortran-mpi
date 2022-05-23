module h5mpi

use, intrinsic :: iso_c_binding, only : C_NULL_CHAR
use, intrinsic :: iso_fortran_env, only : real32, real64, int32, int64, stderr=>error_unit

use mpi, only : MPI_COMM_WORLD, MPI_INFO_NULL, mpi_comm_rank
use hdf5, only : HID_T, SIZE_T, HSIZE_T, &
H5S_ALL_F, H5S_SELECT_SET_F, &
H5T_NATIVE_DOUBLE, H5T_NATIVE_REAL, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE, &
H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F, &
H5P_DEFAULT_F

implicit none (type, external)

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

procedure, public :: open => h5open
procedure, public :: close => h5close
procedure, public :: write_group
procedure, public :: create => hdf_create
procedure, public :: flush => hdf_flush
procedure, public :: filesize => hdf_filesize
procedure, public :: ndim => hdf_get_ndim
procedure, public :: shape => hdf_get_shape
procedure, public :: layout => hdf_get_layout
procedure, public :: chunks => hdf_get_chunk
procedure, public :: exist => hdf_check_exist
procedure, public :: class => get_class
procedure, public :: dtype => get_native_dtype
procedure, public :: deflate => get_deflate
procedure, public :: is_contig => hdf_is_contig
procedure, public :: is_chunked => hdf_is_chunked
procedure, public :: is_compact => hdf_is_compact
procedure, public :: softlink => create_softlink
procedure, public :: is_open
!! procedures without mapping

generic, public :: write => h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d

generic, public :: read => h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d
!! mapped procedures

!> write attributes
generic, public :: writeattr => writeattr_char, writeattr_num

!> read attributes
generic, public :: readattr => readattr_char, readattr_num

procedure, private :: writeattr_char, writeattr_num, readattr_char, readattr_num

procedure, private :: h5write_scalar, h5write_1d, h5write_2d, h5write_3d, h5write_4d, h5write_5d, h5write_6d, h5write_7d

procedure, private :: h5read_scalar, h5read_1d, h5read_2d, h5read_3d, h5read_4d, h5read_5d, h5read_6d, h5read_7d
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

module subroutine h5write_1d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:)
class(*), intent(in), dimension(1), optional :: dset_dims  !< integer or integer(HSIZE_T) full disk shape (not just per worker)
integer(HSIZE_T), intent(in), dimension(1), optional :: istart, iend
integer, intent(in), dimension(1), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_1d

module subroutine h5write_2d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:)
class(*), intent(in), dimension(2), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(2), optional :: istart, iend
integer, intent(in), dimension(2), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_2d

module subroutine h5write_3d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:)
class(*), intent(in), dimension(3), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(3), optional :: istart, iend
integer, intent(in), dimension(3), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_3d

module subroutine h5write_4d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:)
class(*), intent(in), dimension(4), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(4), optional :: istart, iend
integer, intent(in), dimension(4), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_4d

module subroutine h5write_5d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:)
class(*), intent(in), dimension(5), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(5), optional :: istart, iend
integer, intent(in), dimension(5), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_5d

module subroutine h5write_6d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:)
class(*), intent(in), dimension(6), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(6), optional :: istart, iend
integer, intent(in), dimension(6), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_6d

module subroutine h5write_7d(self, dname, value, dset_dims, istart, iend, chunk_size, compact)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
class(*), intent(in) :: value(:,:,:,:,:,:,:)
class(*), intent(in), dimension(7), optional :: dset_dims
integer(HSIZE_T), intent(in), dimension(7), optional :: istart, iend
integer, intent(in), dimension(7), optional :: chunk_size
logical, intent(in), optional :: compact
end subroutine h5write_7d

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
class(*), intent(out) :: chunk_size(:)
end subroutine hdf_get_chunk

module logical function hdf_check_exist(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_check_exist

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

module subroutine h5read_1d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:)
integer(HSIZE_T), intent(in), dimension(1), optional :: istart, iend
end subroutine h5read_1d

module subroutine h5read_2d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:)
integer(HSIZE_T), intent(in), dimension(2), optional :: istart, iend
end subroutine h5read_2d

module subroutine h5read_3d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:)
integer(HSIZE_T), intent(in), dimension(3), optional :: istart, iend
end subroutine h5read_3d

module subroutine h5read_4d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:)
integer(HSIZE_T), intent(in), dimension(4), optional :: istart, iend
end subroutine h5read_4d

module subroutine h5read_5d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:)
integer(HSIZE_T), intent(in), dimension(5), optional :: istart, iend
end subroutine h5read_5d

module subroutine h5read_6d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:)
integer(HSIZE_T), intent(in), dimension(6), optional :: istart, iend
end subroutine h5read_6d

module subroutine h5read_7d(self, dname, value, istart, iend)
class(hdf5_file), intent(in)     :: self
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(:,:,:,:,:,:,:)
integer(HSIZE_T), intent(in), dimension(7), optional :: istart, iend
end subroutine h5read_7d

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
class(*), intent(out) :: attrval(:)
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


interface !< utils.f90

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
end subroutine h5open

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
end subroutine h5close

module logical function is_open(self)
!! check if file handle is open
class(hdf5_file), intent(in) :: self
end function is_open

module logical function has_parallel_compression()
end function has_parallel_compression

module subroutine destructor(self)
!! Close file and handle if user forgets to do so
type(hdf5_file), intent(in) :: self
end subroutine destructor

module function hdf5version() result(v)
!! tell HDF5 library version (major, minor, release)
integer , dimension(3) :: v
end function hdf5version

module subroutine hdf5_close()
!! this subroutine will close ALL existing file handles
!! only call it at end of your program
!! "Flushes all data to disk, closes all open identifiers, and cleans up memory."
!! "Should be called by all HDF5 Fortran programs
end subroutine hdf5_close

module logical function hdf_is_contig(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_is_contig

module logical function hdf_is_compact(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_is_compact

module logical function hdf_is_chunked(self, dname)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
end function hdf_is_chunked

module logical function is_hdf5(filename)
!! is this file HDF5?
character(*), intent(in) :: filename
end function is_hdf5

module subroutine mpi_hyperslab(mem_dims, dset_dims, dset_id, filespace, memspace, istart, iend)
!! Each process defines dataset in memory and writes it to the hyperslab in the file.
integer(HSIZE_T), dimension(:), intent(in) :: mem_dims, dset_dims
integer(HID_T), intent(in) :: dset_id
integer(HID_T), intent(inout) :: filespace, memspace
integer(HSIZE_T), dimension(:), intent(in) :: istart
integer(HSIZE_T), dimension(size(istart)), intent(in) :: iend
end subroutine mpi_hyperslab

module integer(HID_T) function mpi_collective(dname) result(xfer_id)
character(*), intent(in) :: dname !< just for error messages
end function mpi_collective

module subroutine hdf_rank_check(self, dname, mrank, vector_scalar)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(in) :: mrank
logical, intent(out), optional :: vector_scalar
end subroutine hdf_rank_check

module subroutine hdf_shape_check(self, dname, dims, dset_dims)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer(HSIZE_T), intent(in) :: dims(:)
integer(HSIZE_T), intent(out), optional :: dset_dims(size(dims))
end subroutine hdf_shape_check

module integer(HSIZE_T) function hdf_filesize(self)
!! returns the size of the HDF5 file in bytes
class(hdf5_file), intent(in) :: self
end function hdf_filesize

end interface

end module h5mpi
