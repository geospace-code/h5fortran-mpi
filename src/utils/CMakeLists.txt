add_library(perf OBJECT perf.f90 median.f90)
target_link_libraries(perf PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_library(kernel OBJECT kernel.f90)
add_library(partition OBJECT partition.f90)

add_library(cli OBJECT cli.f90)

add_executable(runner frontend.f90)
target_link_libraries(runner PRIVATE partition cli HWLOCfortran::hwloc_ifc)

foreach(t cli kernel perf)
  set_target_properties(${t} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
  target_include_directories(${t} INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
endforeach()
