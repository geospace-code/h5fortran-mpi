add_executable(slab_mpi_write slab_mpi_write.f90)
target_link_libraries(slab_mpi_write PRIVATE h5mpi cli perf test_utils kernel HDF5::HDF5 MPI::MPI_Fortran)
add_executable(slab_mpi_read slab_mpi_read.f90)
target_link_libraries(slab_mpi_read PRIVATE h5mpi cli perf HDF5::HDF5 MPI::MPI_Fortran)

add_library(test_utils OBJECT utils.f90)
target_link_libraries(test_utils PRIVATE kernel MPI::MPI_Fortran)

add_executable(slab_mpi_serial_write slab_mpi_serial_write.f90)
target_link_libraries(slab_mpi_serial_write PRIVATE h5mpi cli perf test_utils kernel HDF5::HDF5 MPI::MPI_Fortran)
add_executable(slab_mpi_serial_read slab_mpi_serial_read.f90)
target_link_libraries(slab_mpi_serial_read PRIVATE h5mpi cli perf HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_serial_write slab_serial_write.f90)
target_link_libraries(slab_serial_write PRIVATE h5mpi cli perf test_utils kernel HDF5::HDF5 MPI::MPI_Fortran)
add_executable(slab_serial_read slab_serial_read.f90)
target_link_libraries(slab_serial_read PRIVATE h5mpi cli perf HDF5::HDF5 MPI::MPI_Fortran)


set(Nrun 2)

set(lx 512 128 96)

add_test(NAME WRITE_serial-Slab
COMMAND slab_serial_write -lx ${lx} -Nrun ${Nrun} -o ${CMAKE_CURRENT_BINARY_DIR}/outserial.h5
)
set_tests_properties(WRITE_serial-Slab PROPERTIES
TIMEOUT 15
FIXTURES_SETUP write_serial-Slab
LABELS "serial_hdf5;write"
RUN_SERIAL true
)

add_test(NAME READ_serial-Slab
COMMAND slab_serial_read -Nrun ${Nrun} -o ${CMAKE_CURRENT_BINARY_DIR}/outserial.h5
)
set_tests_properties(READ_serial-Slab PROPERTIES
TIMEOUT 15
FIXTURES_REQUIRED write_serial-Slab
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/outserial.h5
LABELS "serial_hdf5;read"
RUN_SERIAL true
)


add_test(NAME WRITE_MPI_root-Slab
COMMAND runner -lx ${lx} -exe $<TARGET_FILE:slab_mpi_serial_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${CMAKE_CURRENT_BINARY_DIR}/outmpi_root.h5 -debug
)
set_tests_properties(WRITE_MPI_root-Slab PROPERTIES
TIMEOUT 30
FIXTURES_SETUP write_mpi_root-Slab
LABELS "mpi_root;write"
RUN_SERIAL true
)

add_test(NAME READ_MPI_root-Slab
COMMAND runner -lx ${lx} -exe $<TARGET_FILE:slab_mpi_serial_read> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${CMAKE_CURRENT_BINARY_DIR}/outmpi_root.h5 -debug
)
set_tests_properties(READ_MPI_root-Slab PROPERTIES
TIMEOUT 30
FIXTURES_REQUIRED write_mpi_root-Slab
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/outmpi_root.h5
LABELS "mpi_root;read"
RUN_SERIAL true
)


add_test(NAME WRITE_MPI-Slab
COMMAND runner -lx ${lx} -exe $<TARGET_FILE:slab_mpi_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${CMAKE_CURRENT_BINARY_DIR}/outmpi.h5 -debug
)
set_tests_properties(WRITE_MPI-Slab PROPERTIES
TIMEOUT 30
FIXTURES_SETUP write_mpi-Slab
LABELS "mpi_hdf5;write"
RUN_SERIAL true
)

add_test(NAME READ_MPI-Slab
COMMAND runner -lx ${lx} -exe $<TARGET_FILE:slab_mpi_read> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${CMAKE_CURRENT_BINARY_DIR}/outmpi.h5 -debug
)
set_tests_properties(READ_MPI-Slab PROPERTIES
TIMEOUT 30
FIXTURES_REQUIRED write_mpi-Slab
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/outmpi.h5
LABELS "mpi_hdf5;read"
RUN_SERIAL true
)


find_package(Python COMPONENTS Interpreter)
if(NOT Python_FOUND)
  return()
endif()

set(pyargs ${PROJECT_SOURCE_DIR}/scripts/bench_slab.py --datadir ${CMAKE_CURRENT_BINARY_DIR} -B $<TARGET_FILE_DIR:slab_mpi_write> -Nrun ${Nrun} -lx ${lx})
add_test(NAME PythonRunner COMMAND Python::Interpreter ${pyargs})
set_tests_properties(PythonRunner PROPERTIES
FIXTURES_SETUP PyRun_fxt
TIMEOUT 60
)

add_test(NAME Plotter COMMAND Python::Interpreter ${PROJECT_SOURCE_DIR}/scripts/bench_plot.py --datadir ${CMAKE_CURRENT_BINARY_DIR} -lx ${lx})
set_tests_properties(Plotter PROPERTIES
FIXTURES_REQUIRED PyRun_fxt
TIMEOUT 30
)
