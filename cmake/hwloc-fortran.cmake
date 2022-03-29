# provides CMake imported target HWLOCfortran::hwloc_ifc
include(ExternalProject)


include(${CMAKE_CURRENT_LIST_DIR}/hwloc.cmake)

# --- HWLOC-Fortran

if(BUILD_SHARED_LIBS)
  set(HWLOCfortran_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}hwloc_ifc${CMAKE_SHARED_LIBRARY_SUFFIX})
else()
  set(HWLOCfortran_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}hwloc_ifc${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(HWLOCfortran_args
-DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX}
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
-DCMAKE_BUILD_TYPE=Release
-DBUILD_TESTING:BOOL=false
)

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)
string(JSON HWLOCfortran_git GET ${_libj} HWLOCfortran git)
string(JSON HWLOCfortran_tag GET ${_libj} HWLOCfortran tag)

ExternalProject_Add(HWLOCfortran
GIT_REPOSITORY ${HWLOCfortran_git}
GIT_TAG ${HWLOCfortran_tag}
CMAKE_ARGS ${HWLOCfortran_args}
BUILD_BYPRODUCTS ${HWLOCfortran_LIBRARIES}
INACTIVITY_TIMEOUT 15
CONFIGURE_HANDLED_BY_BUILD ON
DEPENDS HWLOC::HWLOC
)

file(MAKE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/include)

add_library(HWLOCfortran::hwloc_ifc INTERFACE IMPORTED)
target_link_libraries(HWLOCfortran::hwloc_ifc INTERFACE "${HWLOCfortran_LIBRARIES}" HWLOC::HWLOC)
target_include_directories(HWLOCfortran::hwloc_ifc INTERFACE ${CMAKE_INSTALL_PREFIX}/include)

add_dependencies(HWLOCfortran::hwloc_ifc HWLOCfortran)
