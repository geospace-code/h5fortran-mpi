# verify HDF5 was configured for parallel compression
function(hdf5_compression_flag)

if(DEFINED CACHE{hdf5_parallel_compression})
  return()
endif()

cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)

find_file(hdf5_settings_file
NAMES libhdf5.settings
HINTS ${HDF5_LIBRARY_DIR} ${HDF5_DIR}
PATH_SUFFIXES lib share share/hdf5 share/hdf5-mpi
NO_DEFAULT_PATH
REQUIRED
)

message(CHECK_START "Checking if HDF5 configured for parallel compression")

if(HDF5_VERSION VERSION_LESS 1.10.2)
  # https://www.hdfgroup.org/2018/04/why-should-i-care-about-the-hdf5-1-10-2-release/
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression: HDF5 < 1.10.2")
  message(CHECK_FAIL "NO - HDF5 version ${HDF5_VERSION} < 1.10.2")
  return()
endif()

if(MPI_VERSION VERSION_LESS 3)
  message(CHECK_FAIL "NO - MPI version ${MPI_VERSION} < 3")
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression: MPI < 3")
  return()
endif()

file(READ ${hdf5_settings_file} hdf5_settings)
string(REGEX MATCH "Parallel Filtered Dataset Writes:[ ]*([a-zA-Z]+)" hdf5_parallel_compression_match ${hdf5_settings})
if(${CMAKE_MATCH_1})
  message(CHECK_PASS "${CMAKE_MATCH_1}")
  set(hdf5_parallel_compression .true. CACHE STRING "HDF5 configured for parallel compression")
else()
  message(CHECK_FAIL "NO")
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression")
endif()

endfunction()
