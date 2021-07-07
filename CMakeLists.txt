cmake_minimum_required(VERSION 3.14...3.21)
project(HDF5benchmark LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

find_package(MPI COMPONENTS Fortran REQUIRED)

include(cmake/compilers.cmake)

find_package(HDF5 COMPONENTS Fortran parallel REQUIRED)


add_library(h5mpi OBJECT mpi_h5write.f90)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab simple_slab.f90)
target_link_libraries(slab PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)


# old standalone example, not useful for production.
add_executable(simple simple.f90)
target_link_libraries(simple PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
set_target_properties(simple PROPERTIES EXCLUDE_FROM_ALL true)
