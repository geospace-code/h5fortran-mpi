cmake_minimum_required(VERSION 3.20)

project(ref
LANGUAGES C Fortran
)

enable_testing()

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/../cmake/Modules)

find_package(MPI COMPONENTS Fortran REQUIRED)
find_package(HDF5 COMPONENTS parallel Fortran REQUIRED)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

add_executable(write_char write_char.f90)
target_link_libraries(write_char PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
add_test(NAME WriteChar
COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 $<TARGET_FILE:write_char>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# old standalone example, not useful for production.
add_executable(write_coll write_collective.f90)
target_link_libraries(write_coll PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
add_test(NAME WriteCollective
COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 $<TARGET_FILE:write_coll>
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
