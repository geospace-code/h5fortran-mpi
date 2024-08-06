message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION}  CMake ${CMAKE_VERSION}")

option(find_hdf5 "search for HDF5 and Zlib" true)

option(benchmark "Run benchmarks")

option(h5fortran_BUILD_TESTING "Build tests" ${h5fortran_IS_TOP_LEVEL})
if(benchmark)
  set(h5fortran_BUILD_TESTING true)
endif()

option(coverage "Code coverage tests")
option(tidy "Run clang-tidy on the code")

option(hdf5_parallel "use HDF5-MPI layer" true)

include(GNUInstallDirs)

set_property(DIRECTORY PROPERTY EP_UPDATE_DISCONNECTED true)

if(BUILD_SHARED_LIBS AND MSVC)
  message(WARNING "Intel oneAPI has trouble with shared libs in general on Windows, try
    cmake -DBUILD_SHARED_LIBS=off")
endif()

# Necessary for shared library with Visual Studio / Windows oneAPI
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)
