set_property(DIRECTORY PROPERTY LABELS nompi)

function(nompi_test names)

foreach(name IN LISTS names)

add_executable(test_${name} test_${name}.f90)
target_link_libraries(test_${name} PRIVATE h5fortran::h5fortran)

if(${name} STREQUAL "string_read")
  add_test(NAME ${name} COMMAND test_${name} ${string_file})
elseif(${name} STREQUAL "attributes_read")
  add_test(NAME ${name} COMMAND test_${name} ${attr_file})
else()
  add_test(NAME ${name} COMMAND test_${name})
endif()

endforeach()

endfunction(nompi_test)


set(nompi_tests array attributes cast deflate_props deflate_read deflate_write destructor
exist fill groups layout
shape string string_read write
)

nompi_test("${nompi_tests}")

# --- test dependencies

set_property(TEST write PROPERTY FIXTURES_SETUP test_files)

set_property(TEST layout shape PROPERTY FIXTURES_REQUIRED test_files)
set_property(TEST layout shape PROPERTY REQUIRED_FILES "${CMAKE_CURRENT_BINARY_DIR}/test_write.h5;${CMAKE_CURRENT_BINARY_DIR}/test_layout.h5")

set_property(TEST deflate_write PROPERTY FIXTURES_SETUP deflate_files)

set_property(TEST deflate_props deflate_read PROPERTY FIXTURES_REQUIRED deflate_files)
set_property(TEST deflate_props deflate_read PROPERTY REQUIRED_FILES ${CMAKE_CURRENT_BINARY_DIR}/deflate1.h5)

# --- test properties

get_property(test_names DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY TESTS)

set_property(TEST ${test_names} PROPERTY TIMEOUT 30)
set_property(TEST ${test_names} PROPERTY WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# --- Windows shared DLLs
if(WIN32 AND CMAKE_VERSION VERSION_GREATER_EQUAL 3.22)
set_property(TEST ${test_names} PROPERTY
ENVIRONMENT_MODIFICATION "PATH=path_list_append:${ZLIB_INCLUDE_DIRS}/../bin;PATH=path_list_append:${ZLIB_INCLUDE_DIR}/../bin"
)
endif()

# --- Python h5py
if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.12)
  set(Python_FIND_VIRTUALENV STANDARD)
  # use order in PATH to pick Python interpreter
  # this avoids unexpected old system Python getting used IF conda environment has been activated
  find_package(Python COMPONENTS Interpreter)
endif()
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

set_property(TEST string_read PROPERTY FIXTURES_REQUIRED h5str)
set_property(TEST string_read PROPERTY REQUIRED_FILES ${string_file})

# --- String
add_test(NAME PythonString
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_string.py ${string_file}
)

set_property(TEST PythonString PROPERTY FIXTURES_SETUP h5str)

set_property(TEST string_read PythonString PROPERTY DISABLED $<NOT:$<BOOL:${h5py_ok}>>)
