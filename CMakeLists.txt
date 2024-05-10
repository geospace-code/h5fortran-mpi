cmake_minimum_required(VERSION 3.21...3.30)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  message(FATAL_ERROR "please use out-of-source build
  cmake -Bbuild")
endif()

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Release default")
endif()

project(h5fortran
LANGUAGES C Fortran
VERSION 2.1.0
)

include(CTest)

include(options.cmake)

if(${PROJECT_NAME}_BUILD_TESTING)
  find_package(Python COMPONENTS Interpreter)

  enable_language(CXX)
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# --- avoid Anaconda libraries
if(DEFINED ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PREFIX_PATH $ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PATH $ENV{CONDA_PREFIX}/bin)
  # need CMAKE_IGNORE_PATH for CMake < 3.23
  # and to ensure system env var PATH doesn't interfere
  # despite CMAKE_IGNORE_PREFIX_PATH
endif()

include(cmake/compilers.cmake)

# if HDF5-MPI, ensure all the pieces are working together at configure time.
if(hdf5_parallel)
  find_package(MPI COMPONENTS C Fortran REQUIRED)
  include(cmake/check_mpi.cmake)
  check_mpi_version()

  if(find_hdf5)
    find_package(HDF5 COMPONENTS Fortran parallel)
  endif()

  if(HDF5_FOUND)
    if(HDF5_VERSION VERSION_LESS 1.10.2)
      message(WARNING "HDF5 >= 1.10.2 is needed for HDF5-MPI")
    endif()
    include(cmake/hdf5_compression.cmake)
    hdf5_compression_flag()
  else()
    include(cmake/hdf5.cmake)
  endif()
else()
  if(find_hdf5)
    find_package(HDF5 COMPONENTS Fortran)
  endif()
  if(NOT HDF5_FOUND)
    include(cmake/hdf5.cmake)
  endif()
  set(hdf5_parallel_compression ".false.")
endif()

# --- h5fortran library

configure_file(src/hdf5_config.f90.in hdf5_config.f90 @ONLY)

add_library(h5fortran ${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90)
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

if(benchmark)
  add_subdirectory(benchmark)
endif()

# additional Find*.cmake necessary
install(FILES
${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindHDF5.cmake
DESTINATION cmake
)

include(cmake/install.cmake)

file(GENERATE OUTPUT .gitignore CONTENT "*")
