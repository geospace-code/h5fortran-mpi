
## object-oriented HDF5 interface
file(READ writer_template_r32.f90 writer_template_r32)
file(READ writer_template_r64.f90 writer_template_r64)

configure_file(writer.in.f90 writer.f90)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS "writer_template_r32.f90;writer_template_r64.f90")


# see if HDF5 was configured for parallel compression
cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)
find_file(hdf5_settings_file
NAMES libhdf5.settings
HINTS ${HDF5_LIBRARY_DIR} ${HDF5_DIR}
PATH_SUFFIXES lib share share/hdf5 share/hdf5-mpi
NO_DEFAULT_PATH
REQUIRED
)

set(hdf5_parallel_compression .false.)
message(CHECK_START "Checking if HDF5 configured for parallel compression")
if(MPI_VERSION VERSION_GREATER_EQUAL 3)
  file(READ ${hdf5_settings_file} hdf5_settings)
  string(REGEX MATCH "Parallel Filtered Dataset Writes:[ ]*([a-zA-Z]+)" hdf5_parallel_compression_match ${hdf5_settings})
  if(${CMAKE_MATCH_1})
    message(CHECK_PASS "${CMAKE_MATCH_1}")
    set(hdf5_parallel_compression .true.)
  else()
    message(CHECK_FAIL "no")
  endif()
else()
  message(CHECK_FAIL "no since MPI version < 3")
endif()
configure_file(hdf5_config.in.f90 hdf5_config.f90 @ONLY)

# main library
add_library(h5mpi h5fortran_parallel.f90 write.f90
${CMAKE_CURRENT_BINARY_DIR}/writer.f90
${CMAKE_CURRENT_BINARY_DIR}/hdf5_config.f90
)
target_link_libraries(h5mpi PRIVATE HDF5::HDF5 MPI::MPI_Fortran)

add_library(cli OBJECT utils/cli.f90)

add_library(partition OBJECT partition.f90)

add_library(perf OBJECT utils/perf.f90 utils/median.f90)
target_link_libraries(perf PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_library(kernel OBJECT utils/kernel.f90)

## demo of OO interface
add_executable(slab_mpi slab_mpi.f90)
target_link_libraries(slab_mpi PRIVATE h5mpi partition cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_mpi_serial slab_mpi_serial.f90)
target_link_libraries(slab_mpi_serial PRIVATE h5mpi partition cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_serial slab_serial.f90)
target_link_libraries(slab_serial PRIVATE h5mpi partition cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

add_executable(runner frontend.f90)
target_link_libraries(runner PRIVATE partition cli HWLOCfortran::hwloc_ifc)

add_executable(tell_cpu_count utils/tell_cpu_count.f90)
target_link_libraries(tell_cpu_count PRIVATE partition cli HWLOCfortran::hwloc_ifc)

# tests

set(Nrun 3)

add_test(NAME MPI-Slab16
COMMAND runner 16 4 8 -exe $<TARGET_FILE:slab_mpi> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o out16mpi.h5
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(MPI-Slab16 PROPERTIES
TIMEOUT 10
LABELS mpi_hdf5
RUN_SERIAL true
)

add_test(NAME Serial-Slab16
COMMAND slab_serial 16 4 8 -Nrun ${Nrun} -o out16serial.h5
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(Serial-Slab16 PROPERTIES
TIMEOUT 10
LABELS serial_hdf5
RUN_SERIAL true
)

add_test(NAME MPI-Slab10000
COMMAND runner 10000 4 8 -exe $<TARGET_FILE:slab_mpi> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o out10000mpi.h5
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(MPI-Slab10000 PROPERTIES
TIMEOUT 180
LABELS mpi_hdf5
RUN_SERIAL true
)

add_test(NAME Serial-Slab10000
COMMAND slab_serial 10000 4 8 -Nrun ${Nrun} -o out10000serial.h5
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(Serial-Slab10000 PROPERTIES
TIMEOUT 180
LABELS serial_hdf5
RUN_SERIAL true
)
