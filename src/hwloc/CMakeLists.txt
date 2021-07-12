# separate C object lib to avoid compiler warnings from C <-> Fortran add_compile_options etc.
add_library(hwloc_c OBJECT get_cpu_hwloc.c)
target_include_directories(hwloc_c INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(hwloc_c PRIVATE HWLOC::HWLOC)

# not OBJECT to avoid link issues
add_library(hwloc_ifc hwloc_ifc.f90)
set_target_properties(hwloc_ifc PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
target_include_directories(hwloc_ifc INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)
target_link_libraries(hwloc_ifc PRIVATE hwloc_c)
