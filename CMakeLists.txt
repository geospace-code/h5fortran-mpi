cmake_minimum_required(VERSION 3.11...3.25)

get_property(is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT is_multi_config AND NOT (CMAKE_BUILD_TYPE OR DEFINED ENV{CMAKE_BUILD_TYPE}))
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Release default")
endif()

project(h5fortran
LANGUAGES C Fortran
VERSION 2.0.2
)

enable_testing()

if(NOT DEFINED ${PROJECT_NAME}_BUILD_TESTING)
  if(DEFINED BUILD_TESTING)
    set(${PROJECT_NAME}_BUILD_TESTING ${BUILD_TESTING})
  else()
    set(${PROJECT_NAME}_BUILD_TESTING true)
  endif()
endif()

if(benchmark)
  set(${PROJECT_NAME}_BUILD_TESTING true)
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

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/h5fortran.mod DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(TARGETS h5fortran EXPORT ${PROJECT_NAME}-targets
LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

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
