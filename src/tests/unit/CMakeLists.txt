set_directory_properties(PROPERTIES LABELS unit)

set(MPI_MAX 1000)  # tests a priori have maximum dimension length 1000, which is more than most contemporary computers
max_gcd(${MPI_MAX} ${MPIEXEC_MAX_NUMPROCS} Nmpi)

message(STATUS "Unit tests using ${Nmpi} processes")

set(test_names deflate deflate_props deflate_read)

foreach(t IN LISTS test_names)

  add_executable(test_${t} test_${t}.f90)
  target_link_libraries(test_${t} PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

  add_test(NAME ${t}
  COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${Nmpi} $<TARGET_FILE:test_${t}>
  )

endforeach()

set_tests_properties(${test_names} PROPERTIES
TIMEOUT 10
PROCESSORS ${Nmpi}
)

set_tests_properties(deflate PROPERTIES
FIXTURES_SETUP deflate_files
)

set_tests_properties(deflate_props deflate_read PROPERTIES
FIXTURES_REQUIRED deflate_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)
