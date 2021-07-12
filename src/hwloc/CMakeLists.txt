include(CheckIncludeFile)

find_package(HWLOC)

if(HWLOC_FOUND)
  # separate C object lib to avoid compiler warnings from C <-> Fortran add_compile_options etc.
  add_library(hwloc_c OBJECT get_cpu_hwloc.c)
  target_include_directories(hwloc_c INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
  target_link_libraries(hwloc_c PRIVATE HWLOC::HWLOC)

  # not OBJECT to avoid link issues
  add_library(hwloc_ifc hwloc_ifc.f90)
  target_link_libraries(hwloc_ifc PRIVATE hwloc_c)
else()
  check_include_file(unistd.h HAVE_UNISTD_H)
  if(HAVE_UNISTD_H)
    check_symbol_exists(_SC_NPROCESSORS_ONLN unistd.h HAVE_GNU_COUNT)
  endif()

  if(HAVE_GNU_COUNT)
    # https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/sysconf.3.html
    target_sources(hwloc_c PRIVATE get_cpu_sysconf.c)
  elseif(WIN32)
    target_sources(hwloc_c PRIVATE get_cpu_win.c)
  else()
    # this should be almost never encountered
    message(STATUS "did not find hwloc or sysconf. -np Ncpu option will be needed.")
    target_sources(hwloc_c PRIVATE dummy_cpu.c)
  endif()
endif(HWLOC_FOUND)

set_targ_props(hwloc_ifc)
