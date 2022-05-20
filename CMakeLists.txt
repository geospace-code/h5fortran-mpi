cmake_minimum_required(VERSION 3.20)

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

include(cmake/options.cmake)
include(cmake/compilers.cmake)

if(ourFindMPI)
  list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules/MPI)
endif()

find_package(MPI COMPONENTS C Fortran REQUIRED)


include(cmake/gcd.cmake)  # math functions

# --- external libraries:  HDF5, HWLOC

include(cmake/check_mpi.cmake)
check_mpi_version()

find_package(HDF5 1.10.2 COMPONENTS Fortran HL parallel REQUIRED)

if(ENABLE_BENCHMARKS)
  find_package(HWLOC 2.4 REQUIRED)
  find_package(HWLOCfortran CONFIG REQUIRED)

  add_subdirectory(benchmark)
  add_subdirectory(benchmark/test)
endif()

# --- HDF5-MPI object oriented API

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

# additional Find*.cmake necessary
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindHDF5.cmake
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/check_hdf5_mpi.f90
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindSZIP.cmake
DESTINATION cmake
)

include(cmake/install.cmake)
