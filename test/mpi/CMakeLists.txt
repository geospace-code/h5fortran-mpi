set_directory_properties(PROPERTIES LABELS mpi)

# --- helper functions

function(mpi_test names use_runner)

foreach(name IN LISTS names)

set(tname "${name}_mpi")

add_executable(test_${tname} test_${name}.f90)
target_link_libraries(test_${tname} PRIVATE h5fortran::h5fortran)

if(use_runner)
  set(cmd test_runner -exe $<TARGET_FILE:test_${tname}> -mpiexec ${MPIEXEC_EXECUTABLE} -lx 1000)
else()
  set(cmd ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:test_${tname}>)
  # these tests could also be -n 2 instead of max_numprocs.
  # Just trying to keep aware of possible problems vs. MPI worker count.
endif()

if(${name} STREQUAL "string_read")
  list(APPEND cmd ${string_file})
endif()

add_test(NAME ${tname} COMMAND ${cmd})

endforeach()

endfunction(mpi_test)

# --- test files

cmake_path(SET string_file ${CMAKE_CURRENT_BINARY_DIR}/test_string_py.h5)

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


# --- MPI tests

set(mpi_tests array attributes cast destructor exist
fill groups layout shape
string string_read write
)

mpi_test("${mpi_tests}" false)

set(runner_tests deflate_write deflate_props deflate_read)

mpi_test("${runner_tests}" true)

# --- test dependencies

set_tests_properties(write_mpi PROPERTIES
FIXTURES_SETUP test_files
)

set_tests_properties(layout_mpi shape_mpi PROPERTIES
FIXTURES_REQUIRED test_files
REQUIRED_FILES "${CMAKE_CURRENT_BINARY_DIR}/test_write.h5;${CMAKE_CURRENT_BINARY_DIR}/test_layout.h5"
)

set_tests_properties(deflate_write_mpi PROPERTIES
FIXTURES_SETUP deflate_files_mpi
)

set_tests_properties(deflate_props_mpi deflate_read_mpi PROPERTIES
FIXTURES_REQUIRED deflate_files_mpi
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)

if(ENABLE_COVERAGE)
setup_target_for_coverage_gcovr_html(
NAME coverage
EXECUTABLE ${CMAKE_CTEST_COMMAND}
)
endif()

set_tests_properties(string_read_mpi PROPERTIES
FIXTURES_REQUIRED h5str_mpi
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

add_test(NAME PythonString_mpi
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_string.py ${string_file}
)

set_tests_properties(PythonString_mpi PROPERTIES
FIXTURES_SETUP h5str_mpi
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

# --- test properties

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${test_names} PROPERTIES
TIMEOUT 30
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
RESOURCE_LOCK cpu_mpi
)

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
  set_tests_properties(${test_names} PROPERTIES
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
  )
endif()
