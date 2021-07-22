# provides CMake imported target HWLOC::HWLOC_Fortran
include(ExternalProject)

find_package(HWLOC-Fortran CONFIG)

if(HWLOC-Fortran_FOUND)
  return()
endif()

if(NOT HWLOC-Fortran_ROOT)
  if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(HWLOC-Fortran_ROOT ${PROJECT_BINARY_DIR} CACHE PATH "default ROOT")
  else()
    set(HWLOC-Fortran_ROOT ${CMAKE_INSTALL_PREFIX})
  endif()
endif()

set(HWLOC-Fortran_LIBRARIES ${HWLOC-Fortran_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}hwloc_ifc${CMAKE_STATIC_LIBRARY_SUFFIX})

ExternalProject_Add(HWLOC-Fortran
GIT_REPOSITORY "https://github.com/scivision/hwloc-fortran.git"
GIT_TAG "v1.0.2"
CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${HWLOC-Fortran_ROOT} -DBUILD_SHARED_LIBS:BOOL=false -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING:BOOL=false
BUILD_BYPRODUCTS ${HWLOC-Fortran_LIBRARIES}
INACTIVITY_TIMEOUT 15
CONFIGURE_HANDLED_BY_BUILD ON
)

file(MAKE_DIRECTORY ${HWLOC-Fortran_ROOT}/include)

add_library(HWLOC::HWLOC_Fortran INTERFACE IMPORTED)
target_link_libraries(HWLOC::HWLOC_Fortran INTERFACE "${HWLOC-Fortran_LIBRARIES}")
target_include_directories(HWLOC::HWLOC_Fortran INTERFACE ${HWLOC-Fortran_ROOT}/include)

find_package(HWLOC)
if(HWLOC_FOUND)
  target_link_libraries(HWLOC::HWLOC_Fortran INTERFACE HWLOC::HWLOC)
endif()

add_dependencies(HWLOC::HWLOC_Fortran HWLOC-Fortran)
