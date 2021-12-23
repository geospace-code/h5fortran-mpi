add_executable(slab_mpi_write slab_mpi_write.f90)
target_link_libraries(slab_mpi_write PRIVATE h5mpi cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_mpi_serial_write slab_mpi_serial_write.f90)
target_link_libraries(slab_mpi_serial_write PRIVATE h5mpi cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)
add_executable(slab_mpi_serial_read slab_mpi_serial_read.f90)
target_link_libraries(slab_mpi_serial_read PRIVATE h5mpi cli perf HDF5::HDF5 MPI::MPI_Fortran)

add_executable(slab_serial_write slab_serial_write.f90)
target_link_libraries(slab_serial_write PRIVATE h5mpi cli perf kernel HDF5::HDF5 MPI::MPI_Fortran)
add_executable(slab_serial_read slab_serial_read.f90)
target_link_libraries(slab_serial_read PRIVATE h5mpi cli perf HDF5::HDF5 MPI::MPI_Fortran)


set(Nrun 3)

foreach(lx1 16 8192)

  add_test(NAME WRITE_serial-Slab${lx1}
  COMMAND slab_serial_write -lx ${lx1} 4 8 -Nrun ${Nrun} -o out${lx1}serial.h5
  )
  set_tests_properties(WRITE_serial-Slab${lx1} PROPERTIES
  TIMEOUT 15
  FIXTURES_SETUP write_serial-Slab${lx1}
  LABELS "serial_hdf5;write"
  RUN_SERIAL true
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  add_test(NAME READ_serial-Slab${lx1}
  COMMAND slab_serial_read -Nrun ${Nrun} -o out${lx1}serial.h5
  )
  set_tests_properties(READ_serial-Slab${lx1} PROPERTIES
  TIMEOUT 15
  FIXTURES_REQUIRED write_serial-Slab${lx1}
  REQUIRED_FILES out${lx1}serial.h5
  LABELS "serial_hdf5;read"
  RUN_SERIAL true
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )


  add_test(NAME WRITE_MPI-Slab${lx1}
  COMMAND runner -lx ${lx1} 4 8 -exe $<TARGET_FILE:slab_mpi_write> -mpiexec ${MPIEXEC_EXECUTABLE} -Nrun ${Nrun} -o out${lx1}mpi.h5 -debug
  )
  set_tests_properties(WRITE_MPI-Slab${lx1} PROPERTIES
  TIMEOUT 30
  LABELS "mpi_hdf5;write"
  RUN_SERIAL true
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

endforeach()
