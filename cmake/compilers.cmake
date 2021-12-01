if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(
    "$<$<CONFIG:Debug,RelWithDebInfo>:-Werror=array-bounds,-fcheck=all>"
    $<$<CONFIG:Release>:-fno-backtrace>
  )
endif()
