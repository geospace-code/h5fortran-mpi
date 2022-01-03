# verify HDF5 was configured for parallel compression
# needs a run test as sometimes libhdf5.settings says filter collective
# is enabled when it actually fails at runtime (Windows Intel oneAPI)

include(CheckSourceRuns)


function(hdf5_compression_flag)

if(DEFINED CACHE{hdf5_parallel_compression})
  return()
endif()

cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)

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

find_file(hdf5_settings_file
NAMES libhdf5_openmpi.settings libhdf5_mpich.settings libhdf5.settings
HINTS ${HDF5_LIBRARY_DIR} ${HDF5_DIR}
PATH_SUFFIXES lib hdf5/openmpi hdf5/mpich share/hdf5-mpi share/hdf5 share
NO_DEFAULT_PATH
REQUIRED
)

# self/general: lib share
# Ubuntu: hdf5/openmpi hdf5/mpich
# Homebrew: share/hdf5-mpi share/hdf5

file(READ ${hdf5_settings_file} hdf5_settings)
string(REGEX MATCH "Parallel Filtered Dataset Writes:[ ]*([a-zA-Z]+)" hdf5_parallel_compression_match ${hdf5_settings})
if(${CMAKE_MATCH_1})

  set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5 MPI::MPI_Fortran MPI::MPI_C)

  file(READ ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/Modules/check_hdf5_mpi.f90 src)
  check_source_runs(Fortran ${src} hdf5_parallel_compression_run)

  if(hdf5_parallel_compression_run)
    message(CHECK_PASS "${CMAKE_MATCH_1}")
    set(hdf5_parallel_compression .true. CACHE STRING "HDF5 configured for parallel compression")
  else()
    message(CHECK_FAIL "HDF5-MPI run check failed")
    set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression")
  endif()
else()
  message(CHECK_FAIL "NO")
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression")
endif()

endfunction()
