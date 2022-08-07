set_property(DIRECTORY PROPERTY LABELS mpi)

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

add_executable(test_runner runner.f90
${PROJECT_SOURCE_DIR}/benchmark/partition.f90
${PROJECT_SOURCE_DIR}/benchmark/cli.f90
$<TARGET_OBJECTS:cpu_count>
)
target_include_directories(test_runner PRIVATE ${PROJECT_SOURCE_DIR}/benchmark)
# not linked as libraries in case benchmarks aren't built
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set_property(TARGET test_runner PROPERTY LINKER_LANGUAGE Fortran)
else()
  set_property(TARGET test_runner PROPERTY LINKER_LANGUAGE CXX)
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

set_property(TEST write_mpi PROPERTY FIXTURES_SETUP test_files)

set_property(TEST layout_mpi shape_mpi PROPERTY FIXTURES_REQUIRED test_files)
set_property(TEST layout_mpi shape_mpi PROPERTY REQUIRED_FILES "${CMAKE_CURRENT_BINARY_DIR}/test_write.h5;${CMAKE_CURRENT_BINARY_DIR}/test_layout.h5")

set_property(TEST deflate_write_mpi PROPERTY FIXTURES_SETUP deflate_files_mpi)

set_property(TEST deflate_props_mpi deflate_read_mpi PROPERTY FIXTURES_REQUIRED deflate_files_mpi)
set_property(TEST deflate_props_mpi deflate_read_mpi PROPERTY REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5)

set_property(TEST string_read_mpi PROPERTY FIXTURES_REQUIRED h5str)
set_property(TEST string_read_mpi PROPERTY DISABLED $<NOT:$<BOOL:${h5py_ok}>>)

# --- test properties

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_property(TEST ${test_names} PROPERTY TIMEOUT 30)
set_property(TEST ${test_names} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set_property(TEST ${test_names} PROPERTY RESOURCE_LOCK cpu_mpi)

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
set_property(TEST ${test_names} PROPERTY
ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
)
endif()
