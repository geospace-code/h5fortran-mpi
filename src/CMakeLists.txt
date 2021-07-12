
## object-oriented HDF5 interface
file(READ writer_template.f90 writer_template)
configure_file(write.in.f90 write.f90)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS writer_template.f90)

add_library(h5mpi OBJECT mpi_h5write.f90 ${CMAKE_CURRENT_BINARY_DIR}/write.f90)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)

## demo of OO interface
add_executable(slab simple_slab.f90)
target_link_libraries(slab PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)
set_target_properties(slab PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

find_package(HWLOC)
if(HWLOC_FOUND)
  add_subdirectory(hwloc)
else()
  return()
endif()

add_subdirectory(pathlib)

configure_file(simsize.txt ${PROJECT_BINARY_DIR})

add_executable(runner frontend.f90 partition.f90)
target_link_libraries(runner PRIVATE hwloc_ifc pathlib)
set_target_properties(runner PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

add_test(NAME Slab
  COMMAND $<TARGET_FILE:runner> $<TARGET_FILE:slab>
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
set_tests_properties(Slab PROPERTIES TIMEOUT 10)

set_targ_props(h5mpi)
