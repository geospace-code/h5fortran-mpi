# verify HDF5 was configured for parallel compression
cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)

find_file(hdf5_settings_file
NAMES libhdf5.settings
HINTS ${HDF5_LIBRARY_DIR} ${HDF5_DIR}
PATH_SUFFIXES lib share share/hdf5 share/hdf5-mpi
NO_DEFAULT_PATH
REQUIRED
)

set(hdf5_parallel_compression .false.)
message(CHECK_START "Checking if HDF5 configured for parallel compression")
if(MPI_VERSION VERSION_GREATER_EQUAL 3)
  file(READ ${hdf5_settings_file} hdf5_settings)
  string(REGEX MATCH "Parallel Filtered Dataset Writes:[ ]*([a-zA-Z]+)" hdf5_parallel_compression_match ${hdf5_settings})
  if(${CMAKE_MATCH_1})
    message(CHECK_PASS "${CMAKE_MATCH_1}")
    set(hdf5_parallel_compression .true.)
  else()
    message(CHECK_FAIL "no")
  endif()
else()
  message(CHECK_FAIL "no since MPI version < 3")
endif()
configure_file(hdf5_config.in.f90 hdf5_config.f90 @ONLY)

# main library
add_library(h5mpi h5fortran_parallel.f90
${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
set_target_properties(h5mpi PROPERTIES Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
target_include_directories(h5mpi
PUBLIC ${PROJECT_BINARY_DIR}/include
$<INSTALL_INTERFACE:include>
)

add_subdirectory(read)
add_subdirectory(write)
