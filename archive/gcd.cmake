function(gcd x y)

math(EXPR z "${x} % ${y}")
while(NOT z EQUAL 0)
  set(x ${y})
  set(y ${z})
  math(EXPR z "${x} % ${y}")
endwhile()

set(z ${y} PARENT_SCOPE)

endfunction(gcd)


function(max_gcd L M outvar)
# M: number of CPU
# L: grid length
# Range: [1, M]

set(q 1)
foreach(i RANGE 0 ${M})
  math(EXPR W "${M} - ${i}")
  gcd(${W} ${L})
  # message(STATUS "W ${W} L ${L} z ${z}  q ${q}")
  if(z GREATER q)
    set(q ${z})
  endif()
  if(W LESS q)
    break()
  endif()
endforeach()

set(${outvar} ${q} PARENT_SCOPE)

endfunction(max_gcd)
