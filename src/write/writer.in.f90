submodule (h5mpi:write) writer

use h5lt, only : h5ltmake_dataset_double_f, h5ltmake_dataset_float_f, h5ltmake_dataset_int_f, h5ltmake_dataset_string_f

implicit none (type, external)

contains


module procedure h5write_scalar

integer :: ier
integer(HSIZE_T) :: dims(0)

if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open'

dims = shape(value)

select type (value)
type is (real(real32))
  call h5ltmake_dataset_float_f(self%file_id, dname, rank(value), dims, value, ier)
type is (real(real64))
  call h5ltmake_dataset_double_f(self%file_id, dname, rank(value), dims, value, ier)
type is (integer(int32))
  call h5ltmake_dataset_int_f(self%file_id, dname, rank(value), dims, value, ier)
type is (character(*))
  call h5ltmake_dataset_string_f(self%file_id, dname, value, ier)
class default
  error stop "h5fortran:write: unsupported type for " // dname
end select
if (ier/=0) error stop 'h5fortran:write: could not write ' // dname // ' to ' // self%filename
end procedure h5write_scalar

module procedure ph5write_1d
@writer_template@
end procedure ph5write_1d

module procedure ph5write_2d
@writer_template@
end procedure ph5write_2d

module procedure ph5write_3d
@writer_template@
end procedure ph5write_3d

module procedure ph5write_4d
@writer_template@
end procedure ph5write_4d

module procedure ph5write_5d
@writer_template@
end procedure ph5write_5d

module procedure ph5write_6d
@writer_template@
end procedure ph5write_6d

module procedure ph5write_7d
@writer_template@
end procedure ph5write_7d


end submodule writer
