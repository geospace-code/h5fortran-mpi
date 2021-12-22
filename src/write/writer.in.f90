submodule (h5mpi:write) writer

use h5lt, only : h5ltmake_dataset_double_f, h5ltmake_dataset_float_f, h5ltmake_dataset_int_f

implicit none (type, external)

contains

module procedure h5write_scalar
integer :: ier
if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open'
select type (value)
type is (real(real32))
  call h5ltmake_dataset_float_f(self%file_id, dname, rank(value), shape(value, HSIZE_T), value, ier)
type is (real(real64))
  call h5ltmake_dataset_double_f(self%file_id, dname, rank(value), shape(value, HSIZE_T), value, ier)
type is (integer(int32))
  call h5ltmake_dataset_int_f(self%file_id, dname, rank(value), shape(value, HSIZE_T), value, ier)
class default
  error stop "h5fortran:write: unsupported type for " // dname
end select
if (ier/=0) error stop 'h5fortran:write: could not write ' // dname // ' to ' // self%filename
end procedure h5write_scalar

module procedure ph5write1d
@writer_template@
end procedure ph5write1d

module procedure ph5write2d
@writer_template@
end procedure ph5write2d

module procedure ph5write3d
@writer_template@
end procedure ph5write3d

module procedure ph5write4d
@writer_template@
end procedure ph5write4d

module procedure ph5write5d
@writer_template@
end procedure ph5write5d

module procedure ph5write6d
@writer_template@
end procedure ph5write6d

module procedure ph5write7d
@writer_template@
end procedure ph5write7d


end submodule writer
