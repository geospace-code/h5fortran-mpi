submodule (h5fortran:write) writer

use hdf5, only: h5dwrite_f, h5sselect_none_f, H5S_ALL_F

implicit none (type, external)

contains

module procedure h5write_1d
include "writer.inc"
end procedure

module procedure h5write_2d
include "writer.inc"
end procedure

module procedure h5write_3d
include "writer.inc"
end procedure

module procedure h5write_4d
include "writer.inc"
end procedure

module procedure h5write_5d
include "writer.inc"
end procedure

module procedure h5write_6d
include "writer.inc"
end procedure

module procedure h5write_7d
include "writer.inc"
end procedure

end submodule writer
