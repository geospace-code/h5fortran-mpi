set_property(DIRECTORY PROPERTY LABELS nompi)

function(nompi_test name)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)

if(${name} STREQUAL "string_read")
  add_test(NAME ${name} COMMAND test_${name} ${string_file})
elseif(${name} STREQUAL "attributes_read")
  add_test(NAME ${name} COMMAND test_${name} ${attr_file})
else()
  add_test(NAME ${name} COMMAND test_${name})
endif()

endfunction(nompi_test)


set(nompi_tests array attributes cast deflate_props deflate_read deflate_write destructor
exist fill groups layout
shape string string_read write
)

foreach(t IN LISTS nompi_tests)
  nompi_test(${t})
endforeach()

# --- test dependencies

set_property(TEST write PROPERTY FIXTURES_SETUP test_files)

set_property(TEST layout shape PROPERTY FIXTURES_REQUIRED test_files)
set_property(TEST layout shape PROPERTY REQUIRED_FILES "${CMAKE_CURRENT_BINARY_DIR}/test_write.h5;${CMAKE_CURRENT_BINARY_DIR}/test_layout.h5")

set_property(TEST deflate_write PROPERTY FIXTURES_SETUP deflate_files)

set_property(TEST deflate_props deflate_read PROPERTY FIXTURES_REQUIRED deflate_files)
set_property(TEST deflate_props deflate_read PROPERTY REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5)

set_tests_properties(string_read PROPERTIES
FIXTURES_REQUIRED h5str
DISABLED $<NOT:$<BOOL:${h5py_ok}>>
)

# --- test properties

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_property(TEST ${test_names} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
set_property(TEST ${test_names} PROPERTY
ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
)
endif()
