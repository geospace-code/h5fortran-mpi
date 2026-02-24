set_property(DIRECTORY PROPERTY LABELS mpi)

include(${PROJECT_SOURCE_DIR}/cmake/mpi_launcher.cmake)

# --- helper functions

function(mpi_test name use_runner)

set(tname "${name}_mpi")

add_executable(test_${tname} test_${name}.f90)
target_link_libraries(test_${tname} PRIVATE h5fortran::h5fortran)

if(use_runner)
  add_test(NAME ${tname}
   COMMAND test_runner -exe $<TARGET_FILE:test_${tname}> -mpiexec ${MPIEXEC_EXECUTABLE} -lx 1000
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
   )
else()
  add_test(NAME ${tname} COMMAND test_${tname}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
endif()


set_property(TEST ${tname} PROPERTY DISABLED $<AND:$<BOOL:${use_runner}>,$<NOT:$<TARGET_EXISTS:test_runner>>>)

if(NOT use_runner)
  test_mpi_launcher(test_${tname} ${tname} 2)
endif()

endfunction(mpi_test)

# --- test files

if(APPLE AND "$ENV{GITHUB_ACTIONS}")
  message(WARNING "Skipping MPI test_runner due to build quirk in GitHub Actions")
else()
add_executable(test_runner runner.f90
${PROJECT_SOURCE_DIR}/benchmark/partition.f90
${PROJECT_SOURCE_DIR}/benchmark/cli.f90
$<TARGET_OBJECTS:cpu_count>
)
target_include_directories(test_runner PRIVATE ${PROJECT_SOURCE_DIR}/benchmark)
# not linked as libraries in case benchmarks aren't built
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set_property(TARGET test_runner PROPERTY LINKER_LANGUAGE Fortran)
endif()
endif()


# --- MPI tests

set(mpi_tests array attributes cast destructor exist
fill groups layout shape
string string_read write
)

foreach(t IN LISTS mpi_tests)
  mpi_test(${t} false)
endforeach()

set(runner_tests deflate_write deflate_props deflate_read)

foreach(t IN LISTS runner_tests)
  mpi_test(${t} true)
endforeach()

# --- test dependencies

set_property(TEST write_mpi PROPERTY FIXTURES_SETUP test_files)

set_tests_properties(layout_mpi shape_mpi PROPERTIES
FIXTURES_REQUIRED test_files
REQUIRED_FILES "${CMAKE_CURRENT_BINARY_DIR}/test_write.h5;${CMAKE_CURRENT_BINARY_DIR}/test_layout.h5"
)

set_property(TEST deflate_write_mpi PROPERTY FIXTURES_SETUP deflate_files_mpi)

set_tests_properties(deflate_props_mpi deflate_read_mpi PROPERTIES
FIXTURES_REQUIRED deflate_files_mpi
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)

set_tests_properties(string_read_mpi PROPERTIES
FIXTURES_REQUIRED h5str
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/..
)

# --- test properties

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${test_names} PROPERTIES
RESOURCE_LOCK cpu_mpi
)

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
set_property(TEST ${test_names} PROPERTY
ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
)
endif()
