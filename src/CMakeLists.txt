
## object-oriented HDF5 interface
file(READ writer_template.f90 writer_template)
configure_file(write.in.f90 write.f90)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS writer_template.f90)

add_library(h5mpi OBJECT mpi_h5write.f90 ${CMAKE_CURRENT_BINARY_DIR}/write.f90)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)

add_library(cli OBJECT utils/cli.f90)

add_library(partition OBJECT partition.f90)

## demo of OO interface
add_executable(slab simple_slab.f90 utils/perf.f90)
target_link_libraries(slab PRIVATE h5mpi partition cli HDF5::HDF5 MPI::MPI_Fortran)
set_target_properties(slab PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

add_subdirectory(vendor/pathlib)

add_executable(runner frontend.f90)
target_link_libraries(runner PRIVATE partition cli pathlib HWLOC::HWLOC_Fortran)
set_target_properties(runner PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

add_test(NAME SmallSlab
  COMMAND $<TARGET_FILE:runner> 16 4 8 -exe $<TARGET_FILE:slab> -mpiexec ${MPIEXEC_EXECUTABLE}
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
set_tests_properties(SmallSlab PROPERTIES
  TIMEOUT 10
  LABELS mpi_hdf5)

set_targ_props(h5mpi cli)
