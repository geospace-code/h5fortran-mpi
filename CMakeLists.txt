cmake_minimum_required(VERSION 3.14...3.20)
project(HDF5benchmark LANGUAGES Fortran)

find_package(MPI COMPONENTS Fortran REQUIRED)
find_package(HDF5 COMPONENTS Fortran HL REQUIRED)

add_library(original out3f_mpi_h5.f90)
target_link_libraries(original PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
