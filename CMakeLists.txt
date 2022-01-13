cmake_minimum_required(VERSION 3.20...3.22)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "cmake -B <build> to specify build directory")
endif()

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Release default")
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/codemeta.json _j)
string(JSON PROJECT_VERSION GET ${_j} version)

project(h5mpi
LANGUAGES C Fortran
VERSION ${PROJECT_VERSION}
DESCRIPTION "Lightweight object-oriented HDF5 MPI parallel Fortran interface"
HOMEPAGE_URL https://github.com/geospace-code/h5fortran-mpi
)

include(CTest)
if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

if(CMAKE_GENERATOR STREQUAL "Ninja Multi-Config")
  set(EXTPROJ_GENERATOR "Ninja")
else()
  set(EXTPROJ_GENERATOR ${CMAKE_GENERATOR})
endif()

if(BUILD_SHARED_LIBS)
  if(WIN32 AND CMAKE_VERSION VERSION_LESS 3.21 AND BUILD_TESTING)
    message(STATUS "Windows with shared libs needs CMake >= 3.21 to run tests")
  endif()
  if(MSVC)
    message(WARNING "Intel oneAPI has trouble with shared libs in general on Windows, try
      cmake -DBUILD_SHARED_LIBS=off")
  endif()
endif()

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)
if(ourFindMPI)
  list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules/MPI)
endif()

find_package(MPI COMPONENTS C Fortran REQUIRED)

include(cmake/options.cmake)
include(cmake/compilers.cmake)
include(cmake/libraries.cmake)

include(cmake/gcd.cmake)  # math functions

# --- external libraries:  HDF5, HWLOC

include(cmake/check_mpi.cmake)
check_mpi_version()

if(NOT hdf5_external)
  if(autobuild)
    find_package(HDF5 1.10.2...2 COMPONENTS Fortran HL parallel)
  else()
    find_package(HDF5 1.10.2...2 COMPONENTS Fortran HL parallel REQUIRED)
  endif()
endif()

if(NOT HDF5_FOUND OR hdf5_external)
  set(HDF5_VERSION 1.12.1)  # not CACHE to override old FindHDF5 HDF5_VERSION variable

  include(cmake/libraries_hdf5.cmake)
  include(cmake/hdf5.cmake)
endif()

if(ENABLE_BENCHMARKS)
  include(cmake/hwloc-fortran.cmake)

  add_subdirectory(src/utils)
  add_subdirectory(src/tests/benchmarks)
endif()

# --- HDF5-MPI object oriented API

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

# additional Find*.cmake necessary
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindHDF5.cmake
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/check_hdf5_mpi.f90
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindSZIP.cmake
DESTINATION cmake
)

include(cmake/install.cmake)

include(FeatureSummary)
add_feature_info(BuildHDF5 hdf5_external "autobuild HDF5 library")
feature_summary(WHAT ENABLED_FEATURES)
