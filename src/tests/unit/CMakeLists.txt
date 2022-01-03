set_directory_properties(PROPERTIES LABELS unit)

add_executable(test_deflate test_deflate.f90)
target_link_libraries(test_deflate PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

set(MPI_MAX 1000)  # tests a priori have maximum dimension length 1000, which is more than most contemporary computers
max_gcd(${MPI_MAX} ${MPIEXEC_MAX_NUMPROCS} Nmpi)

message(STATUS "Unit tests using ${Nmpi} processes")

add_test(NAME deflate
COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${Nmpi} $<TARGET_FILE:test_deflate>
)

set_tests_properties(deflate PROPERTIES
TIMEOUT 10
PROCESSORS ${Nmpi}
)
