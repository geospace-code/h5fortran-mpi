set_directory_properties(PROPERTIES LABELS unit)

set(MPI_MAX 1000)  # tests a priori have maximum dimension length 1000, which is more than most contemporary computers
max_gcd(${MPI_MAX} ${MPIEXEC_MAX_NUMPROCS} Nmpi)

message(STATUS "Unit tests using ${Nmpi} processes")

set(test_names cast deflate_write deflate_props deflate_read destructor exist layout shape string)

foreach(t IN LISTS test_names)

  add_executable(test_${t} test_${t}.f90)
  target_link_libraries(test_${t} PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

  add_test(NAME ${t}
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${Nmpi} $<TARGET_FILE:test_${t}>
  )

endforeach()

# --- write test data
add_executable(test_write test_write.f90)
target_link_libraries(test_write PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_test(NAME write COMMAND test_write)
set_tests_properties(write PROPERTIES
FIXTURES_SETUP test_files
TIMEOUT 10
)

# --- test dependencies

set_tests_properties(${test_names} PROPERTIES
TIMEOUT 10
PROCESSORS ${Nmpi}
)

set_tests_properties(layout shape PROPERTIES
FIXTURES_REQUIRED test_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/test_write.h5
)

set_tests_properties(deflate_write PROPERTIES
FIXTURES_SETUP deflate_files
)

set_tests_properties(deflate_props deflate_read PROPERTIES
FIXTURES_REQUIRED deflate_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)
