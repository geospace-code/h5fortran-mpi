submodule (h5fortran:hdf5_read) reader_lt

implicit none (type, external)

contains

module procedure h5exist

type(hdf5_file) :: h

call h%open(filename, action='r', mpi=mpi)
h5exist = h%exist(dname)
call h%close()

end procedure h5exist


module procedure lt0read
include "reader_lt.inc"
end procedure

module procedure lt1read
include "reader_lt.inc"
end procedure

module procedure lt2read
include "reader_lt.inc"
end procedure

module procedure lt3read
include "reader_lt.inc"
end procedure

module procedure lt4read
include "reader_lt.inc"
end procedure

module procedure lt5read
include "reader_lt.inc"
end procedure

module procedure lt6read
include "reader_lt.inc"
end procedure

module procedure lt7read
include "reader_lt.inc"
end procedure

end submodule reader_lt
