add_executable(slab_mpi slab_mpi.f90)
target_link_libraries(slab_mpi PRIVATE h5mpi cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_mpi_serial slab_mpi_serial.f90)
target_link_libraries(slab_mpi_serial PRIVATE h5mpi cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_serial slab_serial.f90)
target_link_libraries(slab_serial PRIVATE h5mpi cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

set(Nrun 3)

add_test(NAME MPI-Slab16
COMMAND runner -lx 16 4 8 -exe $<TARGET_FILE:slab_mpi> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o out16mpi.h5 -debug
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(MPI-Slab16 PROPERTIES
TIMEOUT 30
LABELS mpi_hdf5
RUN_SERIAL true
)

add_test(NAME Serial-Slab16
COMMAND slab_serial -lx 16 4 8 -Nrun ${Nrun} -o out16serial.h5
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(Serial-Slab16 PROPERTIES
TIMEOUT 10
LABELS serial_hdf5
RUN_SERIAL true
)

add_test(NAME MPI-Slab-8192
COMMAND runner -lx 8192 4 8 -exe $<TARGET_FILE:slab_mpi> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o out8192mpi.h5 -debug
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(MPI-Slab-8192 PROPERTIES
TIMEOUT 180
LABELS mpi_hdf5
RUN_SERIAL true
)

add_test(NAME Serial-Slab-8192
COMMAND slab_serial -lx 8192 4 8 -Nrun ${Nrun} -o out8192serial.h5
WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set_tests_properties(Serial-Slab-8192 PROPERTIES
TIMEOUT 180
LABELS serial_hdf5
RUN_SERIAL true
)
