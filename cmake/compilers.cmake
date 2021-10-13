if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(
    $<$<CONFIG:Debug>:-Werror=array-bounds>
    $<$<CONFIG:Release>:-fno-backtrace>
  )
endif()
