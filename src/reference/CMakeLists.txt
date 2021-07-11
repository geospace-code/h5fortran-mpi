# old standalone example, not useful for production.
add_executable(simple simple.f90)
target_link_libraries(simple PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
