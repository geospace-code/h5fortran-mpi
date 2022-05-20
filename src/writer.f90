submodule (h5mpi:write) writer

use h5lt, only : h5ltmake_dataset_double_f, h5ltmake_dataset_float_f, h5ltmake_dataset_int_f, h5ltmake_dataset_string_f

implicit none (type, external)

contains

module procedure ph5write_1d
include "writer_template.inc"
end procedure ph5write_1d

module procedure ph5write_2d
include "writer_template.inc"
end procedure ph5write_2d

module procedure ph5write_3d
include "writer_template.inc"
end procedure ph5write_3d

module procedure ph5write_4d
include "writer_template.inc"
end procedure ph5write_4d

module procedure ph5write_5d
include "writer_template.inc"
end procedure ph5write_5d

module procedure ph5write_6d
include "writer_template.inc"
end procedure ph5write_6d

module procedure ph5write_7d
include "writer_template.inc"
end procedure ph5write_7d


end submodule writer
