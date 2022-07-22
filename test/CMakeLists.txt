add_subdirectory(nompi)

if(hdf5_parallel)
  add_subdirectory(mpi)
endif()
