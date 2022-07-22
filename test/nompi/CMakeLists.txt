set_directory_properties(PROPERTIES LABELS nompi)

function(nompi_test names)

foreach(name IN LISTS names)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)

add_test(NAME ${name} COMMAND test_${name})

endforeach()

endfunction(nompi_test)


set(nompi_tests array attributes cast deflate_props deflate_read deflate_write destructor
exist fill
)

nompi_test("${nompi_tests}")

set_tests_properties(deflate_write PROPERTIES
FIXTURES_SETUP deflate_files
)

set_tests_properties(deflate_props deflate_read PROPERTIES
FIXTURES_REQUIRED deflate_files
REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5
)

# --- test properties

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_tests_properties(${test_names} PROPERTIES
TIMEOUT 30
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
  set_tests_properties(${test_names} PROPERTIES
  ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
  )
endif()
