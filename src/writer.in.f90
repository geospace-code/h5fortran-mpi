submodule (h5mpi:write) writer

implicit none (type, external)

contains

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
