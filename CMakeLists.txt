cmake_minimum_required(VERSION 3.20...3.24)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "cmake -B <build> to specify build directory")
endif()

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Release default")
endif()

file(READ ${CMAKE_CURRENT_LIST_DIR}/codemeta.json _j)
string(JSON PROJECT_VERSION GET ${_j} version)

project(h5fortran
LANGUAGES C Fortran
VERSION ${PROJECT_VERSION}
DESCRIPTION "Lightweight object-oriented HDF5 MPI parallel Fortran interface"
HOMEPAGE_URL https://github.com/geospace-code/h5fortran-mpi
)

include(CTest)
if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
endif()

if(${PROJECT_NAME}_BUILD_TESTING)
  enable_language(CXX)
endif()

include(cmake/options.cmake)
include(cmake/compilers.cmake)

if(hdf5_parallel)
  find_package(MPI COMPONENTS C Fortran REQUIRED)

  include(cmake/check_mpi.cmake)
  check_mpi_version()

  find_package(HDF5 COMPONENTS Fortran parallel REQUIRED)
  if(HDF5_VERSION VERSION_LESS 1.10.2)
    message(WARNING "HDF5 >= 1.10.2 is needed for HDF5-MPI")
  endif()
else()
  find_package(HDF5 COMPONENTS Fortran REQUIRED)
endif()

# --- h5fortran object oriented API

add_library(h5fortran)
target_compile_definitions(h5fortran PRIVATE
$<$<BOOL:${hdf5_parallel}>:h5fortran_HAVE_PARALLEL>
)
target_include_directories(h5fortran PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)
target_link_libraries(h5fortran PUBLIC HDF5::HDF5
$<$<BOOL:${hdf5_parallel}>:MPI::MPI_Fortran>
)
set_property(TARGET h5fortran PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET h5fortran PROPERTY LABELS core)
set_property(TARGET h5fortran PROPERTY VERSION ${PROJECT_VERSION})

# GLOBAL needed for use by FetchContent
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod TYPE INCLUDE)

install(TARGETS h5fortran EXPORT ${PROJECT_NAME}-targets)

add_subdirectory(src)

if(${PROJECT_NAME}_BUILD_TESTING)
  add_subdirectory(test)
endif()

if(ENABLE_BENCHMARKS)
  add_subdirectory(benchmark)
endif()


# additional Find*.cmake necessary
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/FindHDF5.cmake
DESTINATION cmake
)

include(cmake/install.cmake)
