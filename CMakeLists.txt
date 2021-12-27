cmake_minimum_required(VERSION 3.20...3.22)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "cmake -B <build> to specify build directory")
endif()

project(HDF5benchmark LANGUAGES C Fortran)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)
if(ourFindMPI)
  list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules/MPI)
endif()

find_package(MPI COMPONENTS C Fortran REQUIRED)

include(cmake/options.cmake)
include(cmake/compilers.cmake)
include(cmake/libraries.cmake)

# --- external libraries:  HDF5, HWLOC

if(NOT hdf5_external)
  if(autobuild)
    find_package(HDF5 COMPONENTS Fortran HL parallel)
  else()
    find_package(HDF5 COMPONENTS Fortran HL parallel REQUIRED)
  endif()
endif()

if(NOT HDF5_FOUND OR HDF5_VERSION VERSION_LESS 1.10.2)
  set(HDF5_VERSION 1.12.1 CACHE STRING "HDF5 version built")
  set(hdf5_external true CACHE BOOL "build HDF5-MPI")
endif()

if(NOT HDF5_FOUND OR hdf5_external)
  include(cmake/libraries_hdf5.cmake)
  include(cmake/hdf5.cmake)
endif()

include(cmake/hwloc-fortran.cmake)

# --- HDF5-MPI object oriented API

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirectory(src/utils)
  add_subdirectory(src/tests)
endif()


include(FeatureSummary)
add_feature_info(BuildHDF5 hdf5_external "autobuild HDF5 library")
feature_summary(WHAT ENABLED_FEATURES)
