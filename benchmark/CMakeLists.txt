cmake_minimum_required(VERSION 3.20)

project(h5mpi-benchmark
LANGUAGES C Fortran
)

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../cmake/Modules)

find_package(MPI COMPONENTS Fortran REQUIRED)
find_package(HDF5 COMPONENTS parallel Fortran REQUIRED)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

add_subdirectory(test)

set(runner_os \"${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_PROCESSOR}\")
configure_file(perf.in.f90 perf.f90 @ONLY)
add_library(perf OBJECT
median.f90
${CMAKE_CURRENT_BINARY_DIR}/perf.f90
)
target_link_libraries(perf PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_library(kernel OBJECT kernel.f90)
add_library(partition OBJECT partition.f90)

add_library(cli OBJECT cli.f90)

add_executable(runner frontend.f90)
target_link_libraries(runner PRIVATE partition cli HWLOCfortran::hwloc_ifc)
set_target_properties(runner PROPERTIES
RUNTIME_OUTPUT_DIRECTORY $<TARGET_FILE_DIR:slab_mpi_write>
)

foreach(t cli kernel perf)
  set_target_properties(${t} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
  target_include_directories(${t} INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()
