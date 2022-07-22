submodule (h5fortran:attr_smod) attr_read

use, intrinsic :: iso_c_binding, only : C_CHAR, C_NULL_CHAR, C_F_POINTER

use hdf5, only : H5Aread_f, H5Aget_info_f, H5Aget_type_f, &
H5Tis_variable_str_f, H5Tget_class_f, H5Tget_native_type_f, H5Tget_size_f, H5Tget_strpad_f, &
H5T_DIR_ASCEND_F
use h5lt, only: h5ltget_attribute_float_f, h5ltget_attribute_double_f, h5ltget_attribute_int_f

implicit none (type, external)

contains

module procedure readattr_scalar

integer(HSIZE_T) :: attr_dims(0)
integer(HID_T) :: attr_id
integer :: attr_class, ier
logical :: is_scalar

call attr_rank_check(self, obj_name, attr, rank(A), is_scalar)

call H5Aopen_by_name_f(self%file_id, obj_name, attr, attr_id, ier)
if(ier/=0) error stop 'ERROR:h5fortran:readattr:H5Aopen ' // obj_name // ' in ' // self%filename

call get_attr_class(self, obj_name, attr, attr_class, attr_id)

!> cast the dataset read from disk to the variable type presented by user h5f%readattr("/my_dataset", x, "y")
!> We only cast when needed to save memory.
!! select case doesn't allow H5T_*
if(attr_class == H5T_FLOAT_F) then
  select type(A)
  type is (real(real64))
    call H5Aread_f(attr_id, H5T_NATIVE_DOUBLE, A, attr_dims, ier)
  type is (real(real32))
    call H5Aread_f(attr_id, H5T_NATIVE_REAL, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: real disk dataset ' // obj_name // ':' // attr // ' needs real memory variable'
  end select
elseif(attr_class == H5T_INTEGER_F) then
  select type(A)
  type is (integer(int32))
    call H5Aread_f(attr_id, H5T_NATIVE_INTEGER, A, attr_dims, ier)
  type is (integer(int64))
    call H5Aread_f(attr_id, H5T_STD_I64LE, A, attr_dims, ier)
  class default
    error stop 'ERROR:h5fortran:readattr: integer disk dataset ' // obj_name // ':' // attr // ' needs integer memory variable'
  end select
elseif(attr_class == H5T_STRING_F) then
  call readattr_char(self, obj_name, attr, A)
else
  error stop 'ERROR:h5fortran:reader: non-handled datatype--please reach out to developers.'
end if
if(ier/=0) error stop 'ERROR:h5fortran:readattr: reading ' // obj_name // ':' // attr // ' from ' // self%filename

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr_scalar: closing dataset: " // obj_name // ':' // attr // " in " // self%filename

end procedure readattr_scalar


subroutine readattr_char(self, obj_name, attr, A)
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr
class(*), intent(inout) :: A

!! NOTE: HDF5 character attributes are scalar.
integer(HID_T) :: type_id, attr_id
integer :: ier, i, L
integer(HSIZE_T) :: dsize

logical :: vstatus, f_corder_valid
integer :: corder, cset, attr_class

!> variable length string
TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string
character(:), allocatable :: buf_char !< fixed length read

integer(HSIZE_T) :: attr_dims(0)

select type(A)
type is (character(*)) !< kind=c_char too
  L = len(A)
end select

call H5Aopen_by_name_f(self%file_id, obj_name, attr, attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name_f failed: " // obj_name // ":" // attr

call get_attr_class(self, obj_name, attr, attr_class, attr_id)

if(attr_class /= H5T_STRING_F) then
  error stop 'ERROR:h5fortran:readattr: character disk attribute ' // obj_name // ':' // attr // ' needs character memory variable'
endif

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // obj_name // ":" // attr
call H5Aget_info_f(attr_id, f_corder_valid, corder, cset, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_info " // obj_name // ":" // attr

call H5Tis_variable_str_f(type_id, vstatus, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // obj_name // ":" // attr

if(vstatus) then
  allocate(cbuf(1:dsize))
  f_ptr = C_LOC(cbuf(1))

  call H5Aread_f(attr_id, type_id, f_ptr, ier)
  if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr

  call C_F_POINTER(cbuf(1), cstr)

  i = index(cstr, c_null_char) - 1
  if (i == -1) i = len_trim(cstr)
  if(i > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr_vlen: buffer too small: ", i, " > ", L, obj_name // ":" // attr
    error stop
  endif

  select type(A)
  type is (character(*)) !< kind=c_char too
    A = cstr(:i)
  end select
else
  if(dsize > L) then
    write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, obj_name // ":" // attr
    error stop
  endif

  allocate(character(dsize) :: buf_char)

  call H5Aread_f(attr_id, type_id, buf_char, attr_dims, ier)
  if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr

  i = index(buf_char, c_null_char) - 1
  if (i == -1) i = len_trim(buf_char)

  select type(A)
  type is (character(*)) !< kind=c_char too
    A = buf_char(:i)
  end select
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr

call H5Aclose_f(attr_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Aclose " // obj_name // ":" // attr

end subroutine readattr_char


module procedure readattr_1d
include 'attr_read.inc'
end procedure

module procedure readattr_2d
include 'attr_read.inc'
end procedure

module procedure readattr_3d
include 'attr_read.inc'
end procedure

module procedure readattr_4d
include 'attr_read.inc'
end procedure

module procedure readattr_5d
include 'attr_read.inc'
end procedure

module procedure readattr_6d
include 'attr_read.inc'
end procedure

module procedure readattr_7d
include 'attr_read.inc'
end procedure

module procedure lt0readattr

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr(obj_name, attr, A)
call h%close()

end procedure lt0readattr


module procedure lt1readattr

type(hdf5_file) :: h

call h%open(filename, action='r')
call h%readattr(obj_name, attr, A)
call h%close()

end procedure lt1readattr


subroutine get_attr_class(self, obj_name, attr_name, class, attr_id, size_bytes, pad_type)
!! get the attribute class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer, intent(out) :: class
integer(HID_T), intent(in), optional :: attr_id
integer(SIZE_T), intent(out), optional :: size_bytes
integer, intent(out), optional :: pad_type

logical :: attr_exists
integer :: ierr
integer(HID_T) :: type_id, native_type_id, a_id

if(present(attr_id)) then
  a_id = attr_id
else
  call H5Aexists_by_name_f(self%file_id, obj_name, attr_name, attr_exists, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:get_attr_class:H5Aexists_by_name_f failed: " // obj_name // ":" // attr_name
  if(.not.attr_exists) error stop 'ERROR:h5fortran:get_attr_class: attribute not exist: ' // obj_name // ":" // attr_name

  call H5Aopen_by_name_f(self%file_id, obj_name, attr_name, a_id, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:readattr:H5Aopen_by_name_f failed: " // obj_name // ":" // attr_name
endif

call H5Aget_type_f(a_id, type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: type_id ' // obj_name // ":" // attr_name

call H5Tget_native_type_f(type_id, H5T_DIR_ASCEND_F, native_type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: native_type_id ' // obj_name // ":" // attr_name

!> compose datatype inferred
call H5Tget_class_f(native_type_id, class, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: class ' // obj_name // ":" // attr_name

if(present(size_bytes)) then
  call H5Tget_size_f(native_type_id, size_bytes, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_attr_class: byte size ' // obj_name // ":" // attr_name
endif

if(present(pad_type)) then
  if(class /= H5T_STRING_F) error stop "ERROR:h5fortran:get_attr_class: pad_type only for string"

  call H5Tget_strpad_f(type_id, pad_type, ierr)
  if(ierr /= 0) error stop "ERROR:h5fortran:get_attr_class:h5tget_strpad " // obj_name // ":" // attr_name
endif

!> close to avoid memory leaks
call H5Tclose_f(native_type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing native dtype ' // obj_name // ":" // attr_name

call H5Tclose_f(type_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing dtype ' // obj_name // ":" // attr_name

if(.not.present(attr_id)) then
  call H5Aclose_f(a_id, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_class: close attribute ' // obj_name // ':' // attr_name
endif

end subroutine get_attr_class


end submodule attr_read
