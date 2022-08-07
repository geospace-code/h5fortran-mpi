add_library(cpu_count OBJECT cpu_count.cpp)
target_compile_features(cpu_count PRIVATE cxx_std_11)
target_compile_definitions(cpu_count PRIVATE $<$<BOOL:${MSVC}>:_CRT_SECURE_NO_WARNINGS>)
target_include_directories(cpu_count PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# --- Python h5py
if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.12)

find_package(Python COMPONENTS Interpreter)
if(NOT DEFINED h5py_ok)
  execute_process(COMMAND ${Python_EXECUTABLE} -c "import h5py"
  RESULT_VARIABLE h5py_code
  ERROR_VARIABLE err
  TIMEOUT 10
  )
  if(h5py_code EQUAL 0)
    set(h5py_ok true CACHE BOOL "h5py OK")
  else()
    set(h5py_ok false CACHE BOOL "h5py not OK")
    message(VERBOSE "${err}")
  endif()
endif()

endif()

set(string_file ${CMAKE_CURRENT_BINARY_DIR}/test_string_py.h5)

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

if(coverage)
  setup_target_for_coverage_gcovr_html(
  NAME coverage
  EXECUTABLE ${CMAKE_CTEST_COMMAND}
  )
endif()
