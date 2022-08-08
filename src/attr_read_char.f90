submodule (h5fortran:attr_read) attr_read_char

implicit none (type, external)

contains


subroutine open_attr_char(self, obj_name, attr_name, attr_id, space_id, type_id, attr_dims)
!! work for 1-D...7-D
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, space_id
integer(HID_T), intent(out) :: type_id, attr_dims(:)

integer :: drank, ier
integer(HSIZE_T), dimension(size(attr_dims)) :: maxdims

call H5Aget_type_f(attr_id, type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aget_type " // obj_name // ":" // attr_name // " " // self%filename

CALL H5Sget_simple_extent_ndims_f(space_id, drank, ier)
if(ier /= 0) error stop "ERROR:h5fortran:readattr:H5Sget_simple_extent_ndims " // obj_name // ":" // attr_name

if(drank > 0) then
  CALL H5Sget_simple_extent_dims_f(space_id, attr_dims, maxdims, ier)
  if(ier /= drank) error stop "ERROR:h5fortran:readattr:H5Sget_simple_extent_dims " // obj_name // ":" // attr_name
else
  attr_dims(1) = 1
endif

end subroutine open_attr_char


subroutine readattr_char_scalar_vlen(self, obj_name, attr_name, attr_id, type_id, Lbuf, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: Lbuf
character(*), intent(inout) :: A

TYPE(C_PTR), DIMENSION(:), ALLOCATABLE, TARGET :: cbuf
TYPE(C_PTR) :: f_ptr

CHARACTER(10000, kind=c_char), POINTER :: cstr !< arbitrary maximum variable length string

integer :: ier, i, L

L = len(A)

allocate(cbuf(1:Lbuf))
f_ptr = C_LOC(cbuf(1))

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "h5fortran:read:readattr:H5Aread " // obj_name // ":" // attr_name // " " // self%filename

call C_F_POINTER(cbuf(1), cstr)

i = index(cstr, c_null_char) - 1
if (i == -1) i = len_trim(cstr)
if(i > L) then
  write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr_vlen: buffer too small: ", &
    i, " > ", L, obj_name // ":" // attr_name // " " // self%filename
  error stop
endif

A = cstr(:i)

end subroutine readattr_char_scalar_vlen


subroutine readattr_char_scalar_fixed(self, obj_name, attr_name, attr_id, type_id, Lbuf, A)

class(hdf5_file), intent(in) :: self
character(*), intent(in) :: obj_name, attr_name
integer(HID_T), intent(in) :: attr_id, type_id
integer(HSIZE_T), intent(in) :: Lbuf
character(*), intent(inout) :: A

TYPE(C_PTR) :: f_ptr
integer(HSIZE_T) :: dsize

CHARACTER(:), DIMENSION(:), ALLOCATABLE, TARGET :: buf_char

integer :: i, L, ier

L = len(A)

call H5Tget_size_f(type_id, dsize, ier)
if(ier/=0) error stop "ERROR:h5fortran:attr_read:H5Tget_size " // obj_name // ":" // attr_name // " " // self%filename

if(dsize > L) then
  write(stderr,'(a,i0,a3,i0,1x,a)') "ERROR:h5fortran:readattr: buffer too small: ", dsize, " > ", L, obj_name // ":" // attr_name
  error stop
endif

allocate(character(dsize) :: buf_char(Lbuf))

f_ptr = C_LOC(buf_char(1)(1:1))

call H5Aread_f(attr_id, type_id, f_ptr, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Aread " // obj_name // ":" // attr_name

i = index(buf_char(1), c_null_char) - 1
if (i == -1) i = len_trim(buf_char(1))

A = buf_char(1)(:i)

end subroutine readattr_char_scalar_fixed


module procedure readattr_char_scalar

integer(HSIZE_T) :: attr_dims(1), Lbuf
integer(HID_T) :: type_id
integer :: ier

logical :: is_vlen


call open_attr_char(self, obj_name, attr_name, attr_id, space_id, type_id, attr_dims)

Lbuf = sum(attr_dims)

call H5Tis_variable_str_f(type_id, is_vlen, ier)
if(ier/=0) error stop "ERROR:h5fortran:readattr:H5Tis_variable_str " // obj_name // ":" // attr_name

if(is_vlen) then
  call readattr_char_scalar_vlen(self, obj_name, attr_name, attr_id, type_id, Lbuf, A)
else
  call readattr_char_scalar_fixed(self, obj_name, attr_name, attr_id, type_id, Lbuf, A)
endif

call H5Tclose_f(type_id, ier)
if(ier/=0) error stop "ERROR:h5fortran:read:H5Tclose " // obj_name // ":" // attr_name

end procedure readattr_char_scalar

end submodule attr_read_char
