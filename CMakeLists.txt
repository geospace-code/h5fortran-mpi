cmake_minimum_required(VERSION 3.26...4.5)
# 3.26 for building HDF5 if needed

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

enable_testing()

include(options.cmake)

if(${PROJECT_NAME}_BUILD_TESTING)
  find_package(Python COMPONENTS Interpreter)

  enable_language(CXX)
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# --- avoid Anaconda libraries
if(DEFINED ENV{CONDA_PREFIX} AND h5fortran_IGNORE_CONDA_LIBRARIES)
  list(APPEND CMAKE_IGNORE_PREFIX_PATH $ENV{CONDA_PREFIX})
  list(APPEND CMAKE_IGNORE_PATH $ENV{CONDA_PREFIX}/bin)
  # need CMAKE_IGNORE_PATH for CMake < 3.23
  # and to ensure system env var PATH doesn't interfere
  # despite CMAKE_IGNORE_PREFIX_PATH
endif()

include(cmake/compilers.cmake)

# if HDF5-MPI, ensure all the pieces are working together at configure time.

if(hdf5_parallel)

  set(MPI_DETERMINE_LIBRARY_VERSION true)

  find_package(MPI COMPONENTS C Fortran REQUIRED)
  include(cmake/check_mpi.cmake)
  check_mpi_version()

  message(STATUS "MPI: ${MPI_Fortran_LIBRARY_VERSION_STRING}")

  include(cmake/hdf5.cmake)

  include(cmake/hdf5_compression.cmake)

  if(NOT HDF5_HAVE_DEFLATE AND HDF5_HAVE_PARALLEL_FILTERED_WRITES)
    message(WARNING "HDF5-MPI does not have parallel compression: HDF5 built without zlib")
  endif()
else()
  set(hdf5_parallel_compression .false.)
endif()

if(WIN32 AND CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
if(HDF5_VERSION MATCHES "(1\.14\.[2-6]|2\.0\.0)")
  message(STATUS "HDF5: applying workaround for HDF5 bug #3663 with Intel oneAPI on Windows")
  target_link_libraries(HDF5::HDF5 INTERFACE shlwapi)
endif()
endif()

# --- h5fortran library

configure_file(src/hdf5_config.f90.in hdf5_config.f90 @ONLY)

add_library(h5fortran ${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
src/utils.f90
src/read.f90 src/read_scalar.f90 src/read_ascii.f90 src/reader.f90
src/write.f90 src/write_scalar.f90 src/writer.f90
src/reader_lt.f90 src/writer_lt.f90
src/interface.f90
src/attr.f90
src/attr_read.f90
src/attr_write.f90
src/mpi.F90
)

target_compile_definitions(h5fortran PRIVATE $<$<BOOL:${hdf5_parallel}>:h5fortran_HAVE_PARALLEL>)

target_include_directories(h5fortran PUBLIC
$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
$<INSTALL_INTERFACE:include>
)

target_link_libraries(h5fortran PUBLIC HDF5::HDF5
$<$<BOOL:${hdf5_parallel}>:MPI::MPI_Fortran>
)

set_target_properties(h5fortran PROPERTIES
Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
LABELS core
VERSION ${PROJECT_VERSION}
)

# GLOBAL needed for use by FetchContent
add_library(h5fortran::h5fortran INTERFACE IMPORTED GLOBAL)
target_link_libraries(h5fortran::h5fortran INTERFACE h5fortran)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod TYPE INCLUDE)

install(TARGETS h5fortran EXPORT ${PROJECT_NAME}-targets)

if(h5fortran_BUILD_TESTING)
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
