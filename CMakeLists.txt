cmake_minimum_required(VERSION 3.14...3.21)
project(HDF5benchmark LANGUAGES C Fortran)

enable_testing()

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

find_package(MPI COMPONENTS Fortran REQUIRED)

include(cmake/compilers.cmake)

find_package(HDF5 COMPONENTS Fortran parallel REQUIRED)

# object-oriented HDF5 interface
file(READ writer_template.f90 writer_template)
configure_file(write.in.f90 write.f90)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS writer_template.f90)
add_library(h5mpi OBJECT mpi_h5write.f90 ${CMAKE_CURRENT_BINARY_DIR}/write.f90)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)

# demo of OO interface
add_executable(slab simple_slab.f90)
target_link_libraries(slab PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_test(NAME Slab
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:slab>)
set_tests_properties(Slab PROPERTIES TIMEOUT 10)


# old standalone example, not useful for production.
add_executable(simple simple.f90)
target_link_libraries(simple PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
set_target_properties(simple PROPERTIES EXCLUDE_FROM_ALL true)
