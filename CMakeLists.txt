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

include(cmake/hwloc-fortran.cmake)

find_package(HDF5 COMPONENTS Fortran parallel REQUIRED)
if(HDF5_VERSION VERSION_LESS 1.10.2)
  message(WARNING "HDF5 >= 1.10.2 is required for MPI-HDF5")
endif()

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirectory(src/utils)
  add_subdirectory(src/tests)
endif()
