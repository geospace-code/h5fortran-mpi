# verify HDF5 was configured for parallel compression

set(hdf5_parallel_compression .false.)

message(CHECK_START "HDF5 parallel compression")

if(HDF5_VERSION VERSION_LESS 1.10.2)
  # https://www.hdfgroup.org/2018/04/why-should-i-care-about-the-hdf5-1-10-2-release/
  message(CHECK_FAIL "NO - HDF5 version ${HDF5_VERSION} < 1.10.2")
  return()
endif()

if(MPI_VERSION VERSION_LESS 3)
  message(CHECK_FAIL "NO - MPI version ${MPI_VERSION} < 3")
  return()
endif()

set(_hdf5_settings_hints)
if(DEFINED HDF5_C_LIBRARY)
  # system/installed HDF5 found via find_package(HDF5): HDF5_C_LIBRARY is the libhdf5 file itself
  cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
  cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)
  list(APPEND _hdf5_settings_hints ${HDF5_LIBRARY_DIR} ${HDF5_DIR})
elseif(DEFINED hdf5_BINARY_DIR)
  # HDF5 built in-tree via FetchContent: settings file lives in the build tree, not yet installed
  list(APPEND _hdf5_settings_hints ${hdf5_BINARY_DIR}/src)
endif()

find_file(HDF5_CONFIG_FILE
NAMES H5pubconf.h H5pubconf-64.h
HINTS ${HDF5_INCLUDE_DIRS} ${hdf5_BINARY_DIR}/src
NO_DEFAULT_PATH
)
message(VERBOSE "HDF5 config: ${HDF5_CONFIG_FILE}")

if(NOT HDF5_CONFIG_FILE)
  return()
endif()

# check HDF5 features that require link of external libraries.

# Always check for HDF5 MPI support because HDF5 link fails if MPI is linked into HDF5.
check_symbol_exists(H5_HAVE_PARALLEL ${HDF5_CONFIG_FILE} HDF5_IS_PARALLEL)
check_symbol_exists(H5_HAVE_FILTER_DEFLATE ${HDF5_CONFIG_FILE} HDF5_HAVE_DEFLATE)
check_symbol_exists(H5_HAVE_PARALLEL_FILTERED_WRITES ${HDF5_CONFIG_FILE} HDF5_HAVE_PARALLEL_FILTERED_WRITES)

if(HDF5_IS_PARALLEL AND HDF5_HAVE_DEFLATE AND HDF5_HAVE_PARALLEL_FILTERED_WRITES)
  set(hdf5_parallel_compression .true.)
  message(CHECK_PASS "YES - HDF5 has parallel compression")
else()
  message(CHECK_FAIL "NO - HDF5 does not have parallel compression")
endif()
