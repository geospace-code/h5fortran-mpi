if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)

add_compile_options(
"$<$<COMPILE_LANGUAGE:Fortran>:-mtune=native;-Wall;-fimplicit-none>"
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug,RelWithDebInfo>>:-Wextra;-fcheck=all;-Werror=array-bounds>"
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-fno-backtrace>"
)
#"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RelWithDebInfo>>:-Wno-maybe-uninitialized>"

elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")

add_compile_options(
$<IF:$<BOOL:${WIN32}>,/QxHost,-xHost>
"$<$<COMPILE_LANGUAGE:Fortran>:-warn;-heap-arrays>"
"$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug,RelWithDebInfo>>:-traceback;-check;-debug>"
)

endif()
