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


cmake_path(SET string_file ${CMAKE_CURRENT_BINARY_DIR}/test_string_py.h5)

add_test(NAME PythonString
COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test_string.py ${string_file}
)

set_property(TEST PythonString PROPERTY FIXTURES_SETUP h5str)
set_property(TEST PythonString PROPERTY DISABLED $<NOT:$<BOOL:${h5py_ok}>>)

# --- test subdirs

add_subdirectory(nompi)

if(hdf5_parallel)
  add_subdirectory(mpi)
endif()

if(ENABLE_COVERAGE)
  setup_target_for_coverage_gcovr_html(
  NAME coverage
  EXECUTABLE ${CMAKE_CTEST_COMMAND}
  )
endif()
