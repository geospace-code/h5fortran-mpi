set_directory_properties(PROPERTIES LABELS unit)

# --- unit tests

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

# --- h5fortran unit tests

function(setup_test names use_runner)

foreach(name IN LISTS names)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5mpi::h5mpi)

if(use_runner)
set(cmd test_runner -exe $<TARGET_FILE:test_${name}> -mpiexec ${MPIEXEC_EXECUTABLE} -lx 1000)
else()
set(cmd ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} $<TARGET_FILE:test_${name}>)
# these tests could also be -n 2 instead of max_numprocs.
# Just trying to keep aware of possible problems vs. MPI worker count.
if(${name} STREQUAL "string_read")
  list(APPEND cmd ${string_file})
endif()
endif()



add_test(NAME ${name} COMMAND ${cmd})

endforeach()

endfunction(setup_test)

cmake_path(SET string_file ${CMAKE_CURRENT_BINARY_DIR}/test_string_py.h5)

# --- write test data
add_executable(test_write test_write.f90)
target_link_libraries(test_write PRIVATE h5mpi::h5mpi)

add_test(NAME write COMMAND test_write)

set(test_names array attributes cast destructor exist groups layout shape
string string_read
)

setup_test("${test_names}" false)

set(runner_tests deflate_write deflate_props deflate_read)

setup_test("${runner_tests}" true)

# --- test dependencies

set_tests_properties(write PROPERTIES
FIXTURES_SETUP test_files
)

set_tests_properties(layout shape PROPERTIES
FIXTURES_REQUIRED test_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/test_write.h5
)

set_tests_properties(deflate_write PROPERTIES
FIXTURES_SETUP deflate_files
)

set_tests_properties(deflate_props deflate_read PROPERTIES
FIXTURES_REQUIRED deflate_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)

if(ENABLE_COVERAGE)
setup_target_for_coverage_gcovr_html(
NAME coverage
EXECUTABLE ${CMAKE_CTEST_COMMAND}
)
endif()

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
  set_tests_properties(${test_names} PROPERTIES
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
  )
endif()

# --- Python h5py
find_package(Python COMPONENTS Interpreter)
if(NOT DEFINED h5py_ok)
  execute_process(COMMAND ${Python_EXECUTABLE} -c "import h5py"
  RESULT_VARIABLE h5py_code
  )
  if(h5py_code EQUAL 0)
    set(h5py_ok true CACHE BOOL "h5py OK")
  else()
    set(h5py_ok false CACHE BOOL "h5py not OK")
  endif()
endif()

set_tests_properties(string_read PROPERTIES
FIXTURES_REQUIRED h5str
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

add_test(NAME PythonString
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_string.py ${string_file}
)

set_tests_properties(PythonString PROPERTIES
FIXTURES_REQUIRED h5lib
FIXTURES_SETUP h5str
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${test_names} PROPERTIES
TIMEOUT 30
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
