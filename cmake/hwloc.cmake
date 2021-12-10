# provides CMake imported target HWLOCfortran::hwloc_ifc
include(ExternalProject)

find_package(HWLOCfortran CONFIG QUIET)

if(HWLOCfortran_FOUND)
  message(STATUS "HWLOCfortran found: ${HWLOCfortran_DIR}")
  return()
endif()

if(NOT HWLOCfortran_ROOT)
  set(HWLOCfortran_ROOT ${CMAKE_INSTALL_PREFIX})
endif()

if(BUILD_SHARED_LIBS)
  set(HWLOCfortran_LIBRARIES ${HWLOCfortran_ROOT}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}hwloc_ifc${CMAKE_SHARED_LIBRARY_SUFFIX})
else()
  set(HWLOCfortran_LIBRARIES ${HWLOCfortran_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}hwloc_ifc${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(HWLOCfortran_args
--install-prefix=${HWLOCfortran_ROOT}
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DBUILD_TESTING:BOOL=false
)

ExternalProject_Add(HWLOCfortran
GIT_REPOSITORY ${HWLOCfortran_git}
GIT_TAG ${HWLOCfortran_tag}
CMAKE_ARGS ${HWLOCfortran_args}
BUILD_BYPRODUCTS ${HWLOCfortran_LIBRARIES}
INACTIVITY_TIMEOUT 15
CONFIGURE_HANDLED_BY_BUILD ON
)

file(MAKE_DIRECTORY ${HWLOCfortran_ROOT}/include)

add_library(HWLOCfortran::hwloc_ifc INTERFACE IMPORTED)
target_link_libraries(HWLOCfortran::hwloc_ifc INTERFACE "${HWLOCfortran_LIBRARIES}")
target_include_directories(HWLOCfortran::hwloc_ifc INTERFACE ${HWLOCfortran_ROOT}/include)

add_dependencies(HWLOCfortran::hwloc_ifc HWLOCfortran)
