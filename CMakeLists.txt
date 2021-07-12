cmake_minimum_required(VERSION 3.19...3.21)
project(HDF5benchmark LANGUAGES C Fortran)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

find_package(MPI COMPONENTS Fortran REQUIRED)

include(cmake/compilers.cmake)
include(cmake/SetTargProps.cmake)

find_package(HDF5 COMPONENTS Fortran parallel REQUIRED)

add_subdirectory(src)
