submodule (h5fortran) h5conf

implicit none (type, external)

contains

module procedure get_hdf5_config
parallel_compression = .true.
end procedure

end submodule h5conf
