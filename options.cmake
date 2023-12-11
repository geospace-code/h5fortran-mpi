include(GNUInstallDirs)

message(STATUS "${PROJECT_NAME} ${PROJECT_VERSION}  CMake ${CMAKE_VERSION}")

option(benchmark "Run benchmarks")
option(coverage "Code coverage tests")
option(tidy "Run clang-tidy on the code")

option(hdf5_parallel "use HDF5-MPI layer" true)

option(CMAKE_TLS_VERIFY "Verify TLS certificates" true)

include(GNUInstallDirs)


if(BUILD_SHARED_LIBS AND MSVC)
  message(WARNING "Intel oneAPI has trouble with shared libs in general on Windows, try
    cmake -DBUILD_SHARED_LIBS=off")
endif()

# Necessary for shared library with Visual Studio / Windows oneAPI
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS true)

file(GENERATE OUTPUT .gitignore CONTENT "*")
