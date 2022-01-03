option(ENABLE_BENCHMARKS "write / read benchmarks")

option(autobuild "autobuild libraries if not available" true)

option(hdf5_parallel "enable parallel HDF5" true)

option(dev "Development mode")

option(ourFindMPI "Use our FindMPI")

option(zlib_legacy "legacy ZLIB 1.x")


set(CMAKE_TLS_VERIFY true)

if(dev)

else()
  set_directory_properties(PROPERTIES EP_UPDATE_DISCONNECTED true)
endif()


if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR} CACHE PATH "default ROOT" FORCE)
endif()
