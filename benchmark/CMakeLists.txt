cmake_minimum_required(VERSION 3.11...3.24)

project(h5mpi-benchmark
LANGUAGES C CXX Fortran
)

enable_testing()

set_property(DIRECTORY PROPERTY LABELS benchmark)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

if(CMAKE_VERSION VERSION_LESS 3.21)
  get_property(not_top DIRECTORY PROPERTY PARENT_DIRECTORY)
  if(NOT not_top)
    set(PROJECT_IS_TOP_LEVEL true)
  endif()
endif()

if(PROJECT_IS_TOP_LEVEL)
  find_package(h5fortran CONFIG REQUIRED)

  add_library(cpu_count OBJECT ${PROJECT_SOURCE_DIR}/../test/cpu_count.cpp)
  target_compile_features(cpu_count PRIVATE cxx_std_11)
  target_compile_definitions(cpu_count PRIVATE $<$<BOOL:${MSVC}>:_CRT_SECURE_NO_WARNINGS>)
  target_include_directories(cpu_count PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
endif()

set(runner_os \"${CMAKE_SYSTEM_NAME} ${CMAKE_SYSTEM_PROCESSOR}\")
configure_file(perf.in.f90 perf.f90 @ONLY)
add_library(perf median.f90
${CMAKE_CURRENT_BINARY_DIR}/perf.f90
)
target_link_libraries(perf PRIVATE h5fortran::h5fortran HDF5::HDF5 MPI::MPI_Fortran)

add_library(kernel kernel.f90)
add_library(partition partition.f90)

add_library(cli cli.f90)

add_executable(runner frontend.f90 $<TARGET_OBJECTS:cpu_count>)
target_include_directories(runner PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(runner PRIVATE partition cli)
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set_property(TARGET runner PROPERTY LINKER_LANGUAGE Fortran)
else()
  set_property(TARGET runner PROPERTY LINKER_LANGUAGE CXX)
endif()

set_property(TARGET cli kernel perf PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET cli kernel perf PROPERTY INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}/include)

add_subdirectory(test)
