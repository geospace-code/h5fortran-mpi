set_directory_properties(PROPERTIES LABELS unit)

add_executable(test_deflate test_deflate.f90)
target_link_libraries(test_deflate PRIVATE h5mpi HDF5::HDF5 MPI::MPI_Fortran)

add_test(NAME Deflate COMMAND test_deflate)

set_tests_properties(Deflate PROPERTIES TIMEOUT 10)
