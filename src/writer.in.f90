submodule (h5mpi:write) writer

use h5lt, only : h5ltmake_dataset_double_f, h5ltmake_dataset_float_f

implicit none (type, external)

contains

module procedure hdf_write_scalar_r32
integer :: ier
if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open'
call h5ltmake_dataset_float_f(self%file_id, dname, rank(A), shape(A, HSIZE_T), A, ier)
if (ier/=0) error stop 'h5fortran:write: could not write ' // dname // ' to ' // self%filename
end procedure hdf_write_scalar_r32


module procedure hdf_write_scalar_r64
integer :: ier
if(.not.self%is_open) error stop 'h5fortran:write: file handle is not open'
call h5ltmake_dataset_double_f(self%file_id, dname, rank(A), shape(A, HSIZE_T), A, ier)
if (ier/=0) error stop 'h5fortran:write: could not write ' // dname // ' to ' // self%filename
end procedure hdf_write_scalar_r64


module procedure ph5write1d_r32
@writer_template_r32@
end procedure ph5write1d_r32

module procedure ph5write1d_r64
@writer_template_r64@
end procedure ph5write1d_r64


module procedure ph5write2d_r32
@writer_template_r32@
end procedure ph5write2d_r32

module procedure ph5write2d_r64
@writer_template_r64@
end procedure ph5write2d_r64


module procedure ph5write3d_r32
@writer_template_r32@
end procedure ph5write3d_r32

module procedure ph5write3d_r64
@writer_template_r64@
end procedure ph5write3d_r64

module procedure ph5write4d_r32
@writer_template_r32@
end procedure ph5write4d_r32

module procedure ph5write4d_r64
@writer_template_r64@
end procedure ph5write4d_r64


end submodule writer
