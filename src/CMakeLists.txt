include(${PROJECT_SOURCE_DIR}/cmake/hdf5_compression.cmake)
hdf5_compression_flag()

configure_file(hdf5_config.f90.in hdf5_config.f90 @ONLY)

# main library
add_library(h5mpi interface.f90
attributes.f90
${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
read.f90 reader.f90
write.f90 writer.f90
)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
set_target_properties(h5mpi PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
target_include_directories(h5mpi PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)

install(TARGETS h5mpi EXPORT ${PROJECT_NAME}-targets)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5mpi.mod TYPE INCLUDE)
