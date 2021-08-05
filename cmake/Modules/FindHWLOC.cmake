# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindHWLOC
-------
Michael Hirsch, Ph.D.

Finds the hwloc library, required by OpenMPI and also useful by itself.
https://www.open-mpi.org/projects/hwloc/

Imported Targets
^^^^^^^^^^^^^^^^

HWLOC::HWLOC

Result Variables
^^^^^^^^^^^^^^^^

HWLOC_LIBRARIES
  libraries to be linked

HWLOC_INCLUDE_DIRS
  dirs to be included

#]=======================================================================]
include(CheckSymbolExists)

find_package(PkgConfig)
pkg_search_module(pc_hwloc hwloc)

# NOTE: the "lib*" are for Windows Intel compiler.
# CMake won't look for lib prefix automatically.

find_library(HWLOC_LIBRARY
  NAMES hwloc libhwloc
  NAMES_PER_DIR
  HINTS ${pc_hwloc_LIBRARY_DIRS} ${pc_hwloc_LIBDIR})

find_path(HWLOC_INCLUDE_DIR
  NAMES hwloc.h
  HINTS ${pc_hwloc_INCLUDE_DIRS})

if(HWLOC_LIBRARY AND HWLOC_INCLUDE_DIR)
  set(CMAKE_REQUIRED_INCLUDES ${HWLOC_INCLUDE_DIR})
  set(CMAKE_REQUIRED_LIBRARIES ${HWLOC_LIBRARY})
  check_symbol_exists(hwloc_topology_load hwloc.h HWLOC_TOPO_LOAD)
endif()

if(HWLOC_TOPO_LOAD)
  find_file(hwloc_conf
    NAMES config.h
    HINTS ${HWLOC_INCLUDE_DIR} ${pc_hwloc_INCLUDE_DIRS}
    PATH_SUFFIXES hwloc/autogen
    DOC "HWLOC configuration header"
  )

  if(hwloc_conf)
    file(STRINGS ${hwloc_conf} _def
    REGEX "^[ \t]*#[ \t]*define[ \t]+HWLOC_VERSION[ \t]+" )
    if("${_def}" MATCHES "HWLOC_VERSION[ \t]+\"([0-9]+\\.[0-9]+\\.[0-9]+)?\"")
      set(HWLOC_VERSION "${CMAKE_MATCH_1}")
    endif()
  endif()
endif()

set(CMAKE_REQUIRED_INCLUDES)
set(CMAKE_REQUIRED_LIBRARIES)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(HWLOC
    REQUIRED_VARS HWLOC_LIBRARY HWLOC_INCLUDE_DIR HWLOC_TOPO_LOAD
    VERSION_VAR HWLOC_VERSION)

if(HWLOC_FOUND)
set(HWLOC_LIBRARIES ${HWLOC_LIBRARY})
set(HWLOC_INCLUDE_DIRS ${HWLOC_INCLUDE_DIR})

if(NOT TARGET HWLOC::HWLOC)
  add_library(HWLOC::HWLOC INTERFACE IMPORTED)
  set_target_properties(HWLOC::HWLOC PROPERTIES
                        INTERFACE_LINK_LIBRARIES "${HWLOC_LIBRARY}"
                        INTERFACE_INCLUDE_DIRECTORIES "${HWLOC_INCLUDE_DIR}"
                      )
endif()
endif(HWLOC_FOUND)

mark_as_advanced(HWLOC_INCLUDE_DIR HWLOC_LIBRARY HWLOC_TOPO_LOAD)
