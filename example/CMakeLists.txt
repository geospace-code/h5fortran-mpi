cmake_minimum_required(VERSION 3.11...3.25)

project(h5mpiExample
LANGUAGES C Fortran
)

enable_testing()

find_package(h5fortran CONFIG REQUIRED)

# --- example 1
add_executable(example1 example1.f90)
target_link_libraries(example1 h5fortran::h5fortran MPI::MPI_Fortran MPI::MPI_C)

add_test(NAME Example1 COMMAND example1)
set_property(TEST Example1 PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_property(TEST Example1 PROPERTY PROPERTY TIMEOUT 30)
