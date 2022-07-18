set_directory_properties(PROPERTIES LABELS unit)

# --- coverage

if(ENABLE_COVERAGE)
setup_target_for_coverage_gcovr_html(
NAME coverage
EXECUTABLE ${CMAKE_CTEST_COMMAND}
)
endif()

# --- unit tests

add_executable(test_runner runner.f90
${PROJECT_SOURCE_DIR}/benchmark/cpu.cpp
${PROJECT_SOURCE_DIR}/benchmark/partition.f90
${PROJECT_SOURCE_DIR}/benchmark/cli.f90
)
# not linked as libraries in case benchmarks aren't built
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set_target_properties(test_runner PROPERTIES LINKER_LANGUAGE Fortran)
else()
  set_target_properties(test_runner PROPERTIES LINKER_LANGUAGE CXX)
endif()

foreach(t attributes cast destructor exist groups layout shape string)

  add_executable(test_${t} test_${t}.f90)
  target_link_libraries(test_${t} PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

  add_test(NAME ${t}
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:test_${t}>
  )
  # these tests could also be -n 2 instead of max_numprocs.
  # Just trying to keep aware of possible problems vs. MPI worker count.

endforeach()


foreach(t deflate_write deflate_props deflate_read)

  add_executable(test_${t} test_${t}.f90)
  target_link_libraries(test_${t} PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

  add_test(NAME ${t}
  COMMAND test_runner -exe $<TARGET_FILE:test_${t}> -mpiexec ${MPIEXEC_EXECUTABLE} -lx 1000
  )

endforeach()


# --- write test data
add_executable(test_write test_write.f90)
target_link_libraries(test_write PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_test(NAME write COMMAND test_write)

# --- test dependencies

set_tests_properties(${test_names} write PROPERTIES
TIMEOUT 10
)

set_tests_properties(write PROPERTIES
FIXTURES_SETUP test_files
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
