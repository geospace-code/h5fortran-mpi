submodule (h5mpi) h5conf

implicit none (type, external)

contains

module procedure get_hdf5_config

parallel_compression = @hdf5_parallel_compression@

end procedure get_hdf5_config

end submodule h5conf
