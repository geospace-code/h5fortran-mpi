cmake_minimum_required(VERSION 3.14...3.20)
project(HDF5benchmark LANGUAGES Fortran)

find_package(MPI COMPONENTS Fortran REQUIRED)

# HDF5 parallel is required
set(HDF5_PREFER_PARALLEL true)
find_package(HDF5 COMPONENTS Fortran HL REQUIRED)
if(NOT HDF5_IS_PARALLEL)
  message(FATAL_ERROR "HDF5 PARALLEL library is required")
endif()

add_library(original out3f_mpi_h5.f90)
target_link_libraries(original PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
