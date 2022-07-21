include(${PROJECT_SOURCE_DIR}/cmake/hdf5_compression.cmake)
hdf5_compression_flag()

configure_file(hdf5_config.f90.in hdf5_config.f90 @ONLY)

# main library
target_sources(h5fortran PRIVATE
utils.f90
${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
read.f90 read_scalar.f90 read_scalar_ascii.f90 reader.f90
write.f90 write_scalar.f90 writer.f90
interface.f90
attr.f90 attr_read.f90 attr_write.f90
mpi.F90
)
