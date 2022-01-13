option(autobuild "autobuild libraries if not available" true)
option(hdf5_parallel "enable parallel HDF5" true)

option(ENABLE_BENCHMARKS "write / read benchmarks")

option(ENABLE_COVERAGE "Code coverage tests")

option(dev "developer mode")

option(ourFindMPI "Use our FindMPI")

option(zlib_legacy "use old ZLIB 1.x")

set(CMAKE_TLS_VERIFY true)

if(dev)
else()
  set_directory_properties(PROPERTIES EP_UPDATE_DISCONNECTED true)
endif()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # will not take effect without FORCE
  # CMAKE_BINARY_DIR in case it's used from FetchContent
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR} CACHE PATH "Install top-level directory" FORCE)
endif()

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()
