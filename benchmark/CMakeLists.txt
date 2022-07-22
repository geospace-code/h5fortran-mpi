cmake_minimum_required(VERSION 3.20...3.24)

project(h5mpi-benchmark
LANGUAGES C CXX Fortran
)

set_property(DIRECTORY PROPERTY LABELS benchmark)

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../cmake/Modules)

find_package(MPI COMPONENTS Fortran REQUIRED)
find_package(HDF5 COMPONENTS parallel Fortran REQUIRED)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

set(runner_os \"${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_PROCESSOR}\")
configure_file(perf.in.f90 perf.f90 @ONLY)
add_library(perf OBJECT
median.f90
${CMAKE_CURRENT_BINARY_DIR}/perf.f90
)
target_link_libraries(perf PRIVATE h5fortran HDF5::HDF5 MPI::MPI_Fortran)

add_library(kernel OBJECT kernel.f90)
add_library(partition OBJECT partition.f90)

add_library(cli OBJECT cli.f90)

add_executable(runner frontend.f90 cpu.cpp)
target_link_libraries(runner PRIVATE partition cli)
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set_property(TARGET runner PROPERTY LINKER_LANGUAGE Fortran)
else()
  set_property(TARGET runner PROPERTY LINKER_LANGUAGE CXX)
endif()

foreach(t cli kernel perf)
  set_property(TARGET ${t} PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
  target_include_directories(${t} INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()


add_subdirectory(test)
