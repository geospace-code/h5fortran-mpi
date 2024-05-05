add_library(test_utils utils.f90)
target_link_libraries(test_utils PRIVATE kernel MPI::MPI_Fortran)

add_executable(slab_mpi_write slab_mpi_write.f90)
target_link_libraries(slab_mpi_write PRIVATE cli perf test_utils kernel h5fortran::h5fortran)
add_executable(slab_mpi_read slab_mpi_read.f90)
target_link_libraries(slab_mpi_read PRIVATE cli perf h5fortran::h5fortran)

add_executable(slab_mpi_serial_write slab_mpi_serial_write.f90)
target_link_libraries(slab_mpi_serial_write PRIVATE cli perf test_utils kernel h5fortran::h5fortran)
add_executable(slab_mpi_serial_read slab_mpi_serial_read.f90)
target_link_libraries(slab_mpi_serial_read PRIVATE cli perf h5fortran::h5fortran)

add_executable(slab_serial_write slab_serial_write.f90)
target_link_libraries(slab_serial_write PRIVATE cli perf test_utils kernel h5fortran::h5fortran)
add_executable(slab_serial_read slab_serial_read.f90)
target_link_libraries(slab_serial_read PRIVATE cli perf h5fortran::h5fortran)

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

set(test_file_dir $<TARGET_FILE_DIR:slab_serial_write>)

foreach(comp_lvl 0 1)
  set(serialfn ${test_file_dir}/serial-comp${comp_lvl}.h5)

  add_test(NAME WRITE_serial-comp${comp_lvl}-Slab
  COMMAND slab_serial_write -lx ${lx3} -Nrun ${Nrun} -o ${serialfn} -debug -comp ${comp_lvl}
  )
  set_property(TEST WRITE_serial-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${write_timeout})
  set_property(TEST WRITE_serial-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP write_serial-comp${comp_lvl}-Slab)
  set_property(TEST WRITE_serial-comp${comp_lvl}-Slab PROPERTY LABELS "serial_hdf5;write")
  set_property(TEST WRITE_serial-comp${comp_lvl}-Slab PROPERTY RUN_SERIAL true)

  add_test(NAME READ_serial-comp${comp_lvl}-Slab
  COMMAND slab_serial_read -Nrun ${Nrun} -o ${serialfn} -debug
  )
  set_property(TEST READ_serial-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${read_timeout})
  set_property(TEST READ_serial-comp${comp_lvl}-Slab PROPERTY FIXTURES_REQUIRED write_serial-comp${comp_lvl}-Slab)
  set_property(TEST READ_serial-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP read_serial-comp${comp_lvl}-Slab)
  set_property(TEST READ_serial-comp${comp_lvl}-Slab PROPERTY REQUIRED_FILES ${serialfn})
  set_property(TEST READ_serial-comp${comp_lvl}-Slab PROPERTY LABELS "serial_hdf5;read")
  set_property(TEST READ_serial-comp${comp_lvl}-Slab PROPERTY RUN_SERIAL true)

  add_test(NAME serial-cleanup-comp${comp_lvl} COMMAND ${CMAKE_COMMAND} -E rm ${serialfn})
  set_property(TEST serial-cleanup-comp${comp_lvl} PROPERTY FIXTURES_REQUIRED "read_serial-comp${comp_lvl}-Slab;read_mpi_root-comp${comp_lvl}-Slab;read_mpi-comp${comp_lvl}-Slab")
  set_property(TEST serial-cleanup-comp${comp_lvl} PROPERTY REQUIRED_FILES ${serialfn})
  set_property(TEST serial-cleanup-comp${comp_lvl} PROPERTY TIMEOUT ${read_timeout})

  # --- MPI-ROOT
  set(mpi_rootfn ${test_file_dir}/mpi_root-comp${comp_lvl}.h5)

  add_test(NAME WRITE_MPI_root-comp${comp_lvl}-Slab
  COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_serial_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpi_rootfn} -debug -comp ${comp_lvl}
  )
  set_property(TEST WRITE_MPI_root-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${write_timeout})
  set_property(TEST WRITE_MPI_root-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP write_mpi_root-comp${comp_lvl}-Slab)
  set_property(TEST WRITE_MPI_root-comp${comp_lvl}-Slab PROPERTY LABELS "mpi_root;write")
  set_property(TEST WRITE_MPI_root-comp${comp_lvl}-Slab PROPERTY RUN_SERIAL true)

  add_test(NAME COMPARE_MPI_root-comp${comp_lvl}-Slab
  COMMAND ${H5DIFF} -v --delta=0.01 ${mpi_rootfn} ${serialfn}
  )
  set_property(TEST COMPARE_MPI_root-comp${comp_lvl}-Slab PROPERTY FIXTURES_REQUIRED "write_serial-comp${comp_lvl}-Slab;write_mpi_root-comp${comp_lvl}-Slab")
  set_property(TEST COMPARE_MPI_root-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP compare_mpi_root-comp${comp_lvl}-Slab)
  set_property(TEST COMPARE_MPI_root-comp${comp_lvl}-Slab PROPERTY REQUIRED_FILES "${mpi_rootfn};${serialfn}")
  set_property(TEST COMPARE_MPI_root-comp${comp_lvl}-Slab PROPERTY LABELS "mpi_root;compare")
  set_property(TEST COMPARE_MPI_root-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${read_timeout})
  set_property(TEST COMPARE_MPI_root-comp${comp_lvl}-Slab PROPERTY DISABLED $<NOT:$<BOOL:${H5DIFF}>>)

  add_test(NAME READ_MPI_root-comp${comp_lvl}-Slab
  COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_serial_read> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpi_rootfn} -ref ${serialfn} -debug
  )
  set_property(TEST READ_MPI_root-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${read_timeout})
  set_property(TEST READ_MPI_root-comp${comp_lvl}-Slab PROPERTY FIXTURES_REQUIRED "compare_mpi_root-comp${comp_lvl}-Slab;write_serial-comp${comp_lvl}-Slab;write_mpi_root-comp${comp_lvl}-Slab")
  set_property(TEST READ_MPI_root-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP read_mpi_root-comp${comp_lvl}-Slab)
  set_property(TEST READ_MPI_root-comp${comp_lvl}-Slab PROPERTY REQUIRED_FILES "${mpi_rootfn};${serialfn}")
  set_property(TEST READ_MPI_root-comp${comp_lvl}-Slab PROPERTY LABELS "mpi_root;read")
  set_property(TEST READ_MPI_root-comp${comp_lvl}-Slab PROPERTY RUN_SERIAL true)

  add_test(NAME root-cleanup-comp${comp_lvl} COMMAND ${CMAKE_COMMAND} -E rm ${mpi_rootfn})
  set_property(TEST root-cleanup-comp${comp_lvl} PROPERTY FIXTURES_REQUIRED read_mpi_root-comp${comp_lvl}-Slab)
  set_property(TEST root-cleanup-comp${comp_lvl} PROPERTY REQUIRED_FILES ${mpi_rootfn})
  set_property(TEST root-cleanup-comp${comp_lvl} PROPERTY TIMEOUT ${read_timeout})

  # --- MPI-HDF5

  set(mpifn ${test_file_dir}/mpi-comp${comp_lvl}.h5)

  add_test(NAME WRITE_MPI-comp${comp_lvl}-Slab
  COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpifn} -debug -comp ${comp_lvl}
  )
  set_property(TEST WRITE_MPI-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${write_timeout})
  set_property(TEST WRITE_MPI-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP write_mpi-comp${comp_lvl}-Slab)
  set_property(TEST WRITE_MPI-comp${comp_lvl}-Slab PROPERTY LABELS "mpi_hdf5;write")
  set_property(TEST WRITE_MPI-comp${comp_lvl}-Slab PROPERTY RUN_SERIAL true)

  add_test(NAME COMPARE_MPI-comp${comp_lvl}-Slab
  COMMAND ${H5DIFF} -v --delta=0.01 ${mpifn} ${serialfn}
  )
  set_property(TEST COMPARE_MPI-comp${comp_lvl}-Slab PROPERTY FIXTURES_REQUIRED "write_serial-comp${comp_lvl}-Slab;write_mpi-comp${comp_lvl}-Slab")
  set_property(TEST COMPARE_MPI-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP compare_mpi_hdf5-comp${comp_lvl}-Slab)
  set_property(TEST COMPARE_MPI-comp${comp_lvl}-Slab PROPERTY REQUIRED_FILES "${mpifn};${serialfn}")
  set_property(TEST COMPARE_MPI-comp${comp_lvl}-Slab PROPERTY LABELS "mpi_hdf5;compare")
  set_property(TEST COMPARE_MPI-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${read_timeout})
  set_property(TEST COMPARE_MPI-comp${comp_lvl}-Slab PROPERTY DISABLED $<NOT:$<BOOL:${H5DIFF}>>)

  add_test(NAME READ_MPI-comp${comp_lvl}-Slab
  COMMAND runner -lx ${lx3} -exe $<TARGET_FILE:slab_mpi_read> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o ${mpifn} -ref ${serialfn} -debug
  )
  set_property(TEST READ_MPI-comp${comp_lvl}-Slab PROPERTY TIMEOUT ${read_timeout})
  set_property(TEST READ_MPI-comp${comp_lvl}-Slab PROPERTY FIXTURES_REQUIRED "compare_mpi_hdf5-comp${comp_lvl}-Slab;write_mpi-comp${comp_lvl}-Slab;write_serial-comp${comp_lvl}-Slab")
  set_property(TEST READ_MPI-comp${comp_lvl}-Slab PROPERTY FIXTURES_SETUP read_mpi-comp${comp_lvl}-Slab)
  set_property(TEST READ_MPI-comp${comp_lvl}-Slab PROPERTY REQUIRED_FILES "${mpifn};${serialfn}")
  set_property(TEST READ_MPI-comp${comp_lvl}-Slab PROPERTY LABELS "mpi_hdf5;read")
  set_property(TEST READ_MPI-comp${comp_lvl}-Slab PROPERTY RUN_SERIAL true)

  add_test(NAME mpi-cleanup-comp${comp_lvl} COMMAND ${CMAKE_COMMAND} -E rm ${mpifn})
  set_property(TEST mpi-cleanup-comp${comp_lvl} PROPERTY FIXTURES_REQUIRED read_mpi-comp${comp_lvl}-Slab)
  set_property(TEST mpi-cleanup-comp${comp_lvl} PROPERTY REQUIRED_FILES ${mpifn})
  set_property(TEST mpi-cleanup-comp${comp_lvl} PROPERTY TIMEOUT ${read_timeout})
endforeach()

if(Python_Interpreter_FOUND AND NOT DEFINED PyOK)
  execute_process(COMMAND ${Python_EXECUTABLE} -c "import h5py,numpy,pandas,matplotlib"
  RESULT_VARIABLE ret
  )
  if(ret EQUAL 0)
    set(PyOK true CACHE BOOL "Python ready")
  else()
    set(PyOK false CACHE BOOL "python not ready")
  endif()
endif()


set(pyargs ${PROJECT_SOURCE_DIR}/bench_slab.py --datadir ${CMAKE_CURRENT_BINARY_DIR} -B ${PROJECT_BINARY_DIR} -Nrun ${Nrun} -lx ${lx3} --debug)
add_test(NAME PythonRunner COMMAND Python::Interpreter ${pyargs})
set_property(TEST PythonRunner PROPERTY FIXTURES_SETUP PyRun_fxt)
set_property(TEST PythonRunner PROPERTY RUN_SERIAL true)

add_test(NAME Plotter COMMAND Python::Interpreter ${PROJECT_SOURCE_DIR}/bench_plot.py --datadir ${CMAKE_CURRENT_BINARY_DIR} -lx ${lx3})
set_property(TEST Plotter PROPERTY FIXTURES_REQUIRED PyRun_fxt)

set_property(TEST PythonRunner Plotter PROPERTY LABELS python)
set_property(TEST PythonRunner Plotter PROPERTY TIMEOUT 60)
set_property(TEST PythonRunner Plotter PROPERTY DISABLED $<NOT:$<BOOL:${PyOK}>>)
