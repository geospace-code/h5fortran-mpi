include(${PROJECT_SOURCE_DIR}/cmake/hdf5_compression.cmake)
hdf5_compression_flag()

configure_file(hdf5_config.f90.in hdf5_config.f90 @ONLY)

set(s ${CMAKE_CURRENT_SOURCE_DIR})

# main library
target_sources(h5fortran PRIVATE
${s}/utils.f90
${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
${s}/read.f90 ${s}/read_scalar.f90 ${s}/read_scalar_ascii.f90 ${s}/reader.f90
${s}/write.f90 ${s}/write_scalar.f90 ${s}/writer.f90
${s}/reader_lt.f90 ${s}/writer_lt.f90
${s}/interface.f90
${s}/attr.f90 ${s}/attr_read.f90 ${s}/attr_write.f90
${s}/mpi.F90
)
