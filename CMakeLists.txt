cmake_minimum_required(VERSION 3.20...3.22)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "cmake -B <build> to specify build directory")
endif()

project(HDF5benchmark LANGUAGES C Fortran)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

find_package(MPI COMPONENTS C Fortran REQUIRED)

include(cmake/options.cmake)
include(cmake/compilers.cmake)
include(cmake/libraries.cmake)

include(cmake/hwloc-fortran.cmake)

find_package(HDF5 COMPONENTS Fortran parallel REQUIRED)

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirctory(src/tests)
endif()
