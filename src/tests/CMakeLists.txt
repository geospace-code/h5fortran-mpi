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

cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)
find_program(H5DIFF
NAMES h5diff
HINTS ${HDF5_ROOT} ${HDF5_DIR}
)


set(write_timeout 30)  # 15 timed out MacOS Github actions on Debug
set(read_timeout 15)
set(Nrun 1)

# set(lx3 512 128 96)
set(lx3 8 6 1)

set(serialfn ${CMAKE_CURRENT_BINARY_DIR}/outserial.h5)
add_test(NAME WRITE_serial-Slab
COMMAND slab_serial_write -lx ${lx3} -Nrun ${Nrun} -o ${serialfn} -debug
)
set_tests_properties(WRITE_serial-Slab PROPERTIES
TIMEOUT ${write_timeout}
FIXTURES_SETUP write_serial-Slab
LABELS "serial_hdf5;write"
RUN_SERIAL true
)

add_test(NAME READ_serial-Slab
COMMAND slab_serial_read -Nrun ${Nrun} -o ${serialfn} -debug
)
set_tests_properties(READ_serial-Slab PROPERTIES
TIMEOUT ${read_timeout}
FIXTURES_REQUIRED write_serial-Slab
FIXTURES_SETUP read_serial-Slab
REQUIRED_FILES ${serialfn}
LABELS "serial_hdf5;read"
RUN_SERIAL true
)

add_test(NAME serial-cleanup COMMAND ${CMAKE_COMMAND} -E rm ${serialfn})
set_tests_properties(serial-cleanup PROPERTIES
FIXTURES_REQUIRED "read_serial-Slab;read_mpi_root-Slab;read_mpi-Slab"
TIMEOUT ${read_timeout}
)

# --- MPI-ROOT
set(mpi_rootfn ${CMAKE_CURRENT_BINARY_DIR}/mpi_root.h5)
add_test(NAME WRITE_MPI_root-Slab
COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_serial_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpi_rootfn} -debug
)
set_tests_properties(WRITE_MPI_root-Slab PROPERTIES
TIMEOUT ${write_timeout}
FIXTURES_SETUP write_mpi_root-Slab
LABELS "mpi_root;write"
RUN_SERIAL true
)

add_test(NAME COMPARE_MPI_root-Slab
COMMAND ${H5DIFF} -v --delta=0.01 ${mpi_rootfn} ${serialfn}
)
set_tests_properties(COMPARE_MPI_root-Slab PROPERTIES
FIXTURES_REQUIRED "write_serial-Slab;write_mpi_root-Slab"
FIXTURES_SETUP compare_mpi_root-Slab
REQUIRED_FILES "${mpi_rootfn};${serialfn}"
LABELS "mpi_root;compare"
TIMEOUT ${read_timeout}
DISABLED $<NOT:$<BOOL:${H5DIFF}>>
)

add_test(NAME READ_MPI_root-Slab
COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_serial_read> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpi_rootfn} -ref ${serialfn} -debug
)
set_tests_properties(READ_MPI_root-Slab PROPERTIES
TIMEOUT ${read_timeout}
FIXTURES_REQUIRED "compare_mpi_root-Slab;write_serial-Slab;write_mpi_root-Slab"
FIXTURES_SETUP read_mpi_root-Slab
REQUIRED_FILES "${mpi_rootfn};${serialfn}"
LABELS "mpi_root;read"
RUN_SERIAL true
)

add_test(NAME root-cleanup COMMAND ${CMAKE_COMMAND} -E rm ${mpi_rootfn})
set_tests_properties(root-cleanup PROPERTIES
FIXTURES_REQUIRED read_mpi_root-Slab
TIMEOUT ${read_timeout}
)

# --- MPI-HDF5

set(mpifn ${CMAKE_CURRENT_BINARY_DIR}/outmpi.h5)
add_test(NAME WRITE_MPI-Slab
COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpifn} -debug
)
set_tests_properties(WRITE_MPI-Slab PROPERTIES
TIMEOUT ${write_timeout}
FIXTURES_SETUP write_mpi-Slab
LABELS "mpi_hdf5;write"
RUN_SERIAL true
)

add_test(NAME COMPARE_MPI-Slab
COMMAND ${H5DIFF} -v --delta=0.01 ${mpifn} ${serialfn}
)
set_tests_properties(COMPARE_MPI-Slab PROPERTIES
FIXTURES_REQUIRED "write_serial-Slab;write_mpi-Slab"
FIXTURES_SETUP compare_mpi_hdf5-Slab
REQUIRED_FILES "${mpifn};${serialfn}"
LABELS "mpi_hdf5;compare"
TIMEOUT ${read_timeout}
DISABLED $<NOT:$<BOOL:${H5DIFF}>>
)

add_test(NAME READ_MPI-Slab
COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_read> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpifn} -ref ${serialfn} -debug
)
set_tests_properties(READ_MPI-Slab PROPERTIES
TIMEOUT ${read_timeout}
FIXTURES_REQUIRED "compare_mpi_hdf5-Slab;write_mpi-Slab;write_serial-Slab"
FIXTURES_SETUP read_mpi-Slab
REQUIRED_FILES "${mpifn};${serialfn}"
LABELS "mpi_hdf5;read"
RUN_SERIAL true
)

add_test(NAME mpi-cleanup COMMAND ${CMAKE_COMMAND} -E rm ${mpifn})
set_tests_properties(mpi-cleanup PROPERTIES
FIXTURES_REQUIRED read_mpi-Slab
TIMEOUT ${read_timeout}
)


find_package(Python COMPONENTS Interpreter)
if(NOT Python_FOUND)
  set(PySkip 1 CACHE STRING "python not ready")
endif()

if(NOT DEFINED PySkip)
  execute_process(COMMAND ${Python_EXECUTABLE} -c "import h5py,numpy,pandas,matplotlib"
  TIMEOUT 10
  RESULT_VARIABLE ret
  )
  if(ret EQUAL 0)
    set(PySkip 0 CACHE STRING "Python ready")
  else()
    set(PySkip 1 CACHE STRING "python not ready")
  endif()
endif()


set(pyargs ${PROJECT_SOURCE_DIR}/scripts/bench_slab.py --datadir ${CMAKE_CURRENT_BINARY_DIR} -B $<TARGET_FILE_DIR:slab_mpi_write> -Nrun ${Nrun} -lx ${lx3} --debug)
add_test(NAME PythonRunner COMMAND Python::Interpreter ${pyargs})
set_tests_properties(PythonRunner PROPERTIES
FIXTURES_REQUIRED "read_serial-Slab;read_mpi_root-Slab;read_mpi-Slab"
FIXTURES_SETUP PyRun_fxt
LABELS python
TIMEOUT 60
DISABLED ${PySkip}
RUN_SERIAL true
)

add_test(NAME Plotter COMMAND Python::Interpreter ${PROJECT_SOURCE_DIR}/scripts/bench_plot.py --datadir ${CMAKE_CURRENT_BINARY_DIR} -lx ${lx3})
set_tests_properties(Plotter PROPERTIES
FIXTURES_REQUIRED PyRun_fxt
LABELS python
TIMEOUT 30
DISABLED ${PySkip}
)
