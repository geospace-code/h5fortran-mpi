cmake_minimum_required(VERSION 3.20)

project(ref
LANGUAGES C Fortran
)

cmake_path(SET CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/Modules)

# --- auto-ignore build directory
if(NOT EXISTS ${PROJECT_BINARY_DIR}/.gitignore)
  file(WRITE ${PROJECT_BINARY_DIR}/.gitignore "*")
endif()

# old standalone example, not useful for production.
add_executable(simple simple.f90)
target_link_libraries(simple PRIVATE HDF5::HDF5 MPI::MPI_Fortran)
