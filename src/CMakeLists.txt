
## object-oriented HDF5 interface
file(READ writer_template_r32.f90 writer_template_r32)
file(READ writer_template_r64.f90 writer_template_r64)
file(READ reader_template.f90 reader_template)

configure_file(reader.in.f90 reader.f90 @ONLY)
configure_file(writer.in.f90 writer.f90 @ONLY)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS "reader_template.f90;writer_template_r32.f90;writer_template_r64.f90")


# see if HDF5 was configured for parallel compression
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
add_library(h5mpi h5fortran_parallel.f90 write.f90 read.f90
${CMAKE_CURRENT_BINARY_DIR}/writer.f90
${CMAKE_CURRENT_BINARY_DIR}/reader.f90
${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)

add_library(cli OBJECT utils/cli.f90)

add_library(partition OBJECT partition.f90)

add_library(perf OBJECT utils/perf.f90 utils/median.f90)
target_link_libraries(perf PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_library(kernel OBJECT utils/kernel.f90)

add_executable(runner frontend.f90)
target_link_libraries(runner PRIVATE partition cli HWLOCfortran::hwloc_ifc)
