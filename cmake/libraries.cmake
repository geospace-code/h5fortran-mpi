set(_names HWLOCfortran)

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)

foreach(n ${_names})
  foreach(t git tag)
    string(JSON ${n}_${t} GET ${_libj} ${n} ${t})
  endforeach()
endforeach()
