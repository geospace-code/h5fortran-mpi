cmake_minimum_required(VERSION 3.20)
# https://github.com/HDFGroup/hdf5/blob/develop/CMakeLists.txt

project(HDF5_build
LANGUAGES C Fortran
)

option(hdf5_parallel "build HDF5 parallel MPI" on)

# --- system checks
file(MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX})

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

if(hdf5_parallel)
  if(NOT MPI_ROOT AND DEFINED ENV{MPI_ROOT})
    set(MPI_ROOT $ENV{MPI_ROOT})
  endif()

  if(CMAKE_SYSTEM_NAME STREQUAL "Linux" AND MPI_ROOT)
    set(ld_path $ENV{LD_LIBRARY_PATH})
    cmake_path(CONVERT "${ld_path}" TO_CMAKE_PATH_LIST ld_path NORMALIZE)
    cmake_path(CONVERT "${MPI_ROOT}" TO_CMAKE_PATH_LIST MPI_ROOT NORMALIZE)

    find_package(MPI COMPONENTS C REQUIRED)

    include(${PROJECT_SOURCE_DIR}/../cmake/check_mpi.cmake)
    check_mpi_version()

    list(GET MPI_C_LIBRARIES 0 mpi_lib)
    cmake_path(GET mpi_lib PARENT_PATH mpi_libdir)

    if(NOT "${ld_path}" MATCHES "${mpi_libdir}")
      message(WARNING "${mpi_libdir} not found in LD_LIBRARY_PATH: $ENV{LD_LIBRARY_PATH}
      HDF5 build may fail due to bugs in HDF5 package CMake scripts.
      Fix this by adding to ~/.bashrc or similar:
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:{mpi_libdir}")
    endif()
  endif()
endif(hdf5_parallel)

# HDF5 install fails to work (link) if prior HDF5 library is installed there
find_package(HDF5 CONFIG PATHS ${CMAKE_INSTALL_PREFIX} NO_DEFAULT_PATH QUIET)
if(HDF5_FOUND)
  message(FATAL_ERROR "HDF5 library already installed: ${HDF5_DIR}
  Please pick a new install location or completely remove the old HDF5 install directory.
  Otherwise, HDF5 will fail to link correctly with prior version and this version mixed.")
endif()

# --- commence HDF5 build/install
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/libraries.json json)

set_property(DIRECTORY PROPERTY EP_UPDATE_DISCONNECTED true)

include(${PROJECT_SOURCE_DIR}/../cmake/hdf5.cmake)

message(STATUS "Build / install HDF5 ${hdf5_tag} to ${CMAKE_INSTALL_PREFIX}")

# --- features
include(FeatureSummary)

add_feature_info(HDF5parallel hdf5_parallel "HDF5 MPI layer")

feature_summary(WHAT ENABLED_FEATURES DISABLED_FEATURES)

file(GENERATE OUTPUT .gitignore CONTENT "*")
