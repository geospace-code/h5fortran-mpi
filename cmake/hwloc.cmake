include(ExternalProject)

set(hwloc_external true CACHE BOOL "autobuild HWLOC")

if(HWLOC_VERSION VERSION_LESS 2.6)
  set(HWLOC_VERSION 2.7.0)
endif()

# need to be sure _ROOT isn't empty, DEFINED is not enough
if(NOT HWLOC_ROOT)
  set(HWLOC_ROOT ${CMAKE_INSTALL_PREFIX})
endif()

set(hwloc_cmake_args
--install-prefix=${HWLOC_ROOT}
-DCMAKE_BUILD_TYPE=Release
-DHWLOC_ENABLE_TESTING:BOOL=off
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
)

if(BUILD_SHARED_LIBS)
  set(HWLOC_LIBRARIES ${HWLOC_ROOT}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}hwloc${CMAKE_SHARED_LIBRARY_SUFFIX})
  set(hwloc_args --disable-static --enable-shared)
else()
  set(HWLOC_LIBRARIES ${HWLOC_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}hwloc${CMAKE_STATIC_LIBRARY_SUFFIX})
  set(hwloc_args --disable-shared --enable-static)
endif()
set(HWLOC_INCLUDE_DIRS ${HWLOC_ROOT}/include)


# --- read JSON meta

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json _libj)
string(JSON hwloc_url GET ${_libj} hwloc ${HWLOC_VERSION} url)
string(JSON hwloc_sha256 GET ${_libj} hwloc ${HWLOC_VERSION} sha256)

if(WIN32)
  ExternalProject_Add(HWLOC
  URL ${hwloc_url}
  URL_HASH SHA256=${hwloc_sha256}
  CMAKE_GENERATOR ${EXTPROJ_GENERATOR}
  SOURCE_SUBDIR contrib/windows-cmake
  CMAKE_ARGS ${hwloc_cmake_args}
  BUILD_BYPRODUCTS ${HWLOC_LIBRARIES}
  CONFIGURE_HANDLED_BY_BUILD ON
  INACTIVITY_TIMEOUT 15
  )
else()
  find_program(MAKE_EXECUTABLE
  NAMES gmake make
  NAMES_PER_DIR
  DOC "GNU Make"
  REQUIRED)

  find_package(LibXml2)
  if(NOT LibXml2_FOUND)
    list(APPEND hwloc_args --disable-libxml2)
  endif()

  ExternalProject_Add(HWLOC
  URL ${hwloc_url}
  URL_HASH SHA256=${hwloc_sha256}
  BUILD_BYPRODUCTS ${HWLOC_LIBRARIES}
  CONFIGURE_HANDLED_BY_BUILD ON
  INACTIVITY_TIMEOUT 15
  CONFIGURE_COMMAND ${PROJECT_BINARY_DIR}/HWLOC-prefix/src/HWLOC/configure --prefix=${HWLOC_ROOT} ${hwloc_args}
  BUILD_COMMAND ${MAKE_EXECUTABLE} -j
  INSTALL_COMMAND ${MAKE_EXECUTABLE} install
  TEST_COMMAND ""
  )
endif()

file(MAKE_DIRECTORY ${HWLOC_INCLUDE_DIRS})
# avoid race condition

# this GLOBAL is required to be visible via other project's FetchContent
add_library(HWLOC::HWLOC INTERFACE IMPORTED GLOBAL)
target_include_directories(HWLOC::HWLOC INTERFACE "${HWLOC_INCLUDE_DIRS}")
target_link_libraries(HWLOC::HWLOC INTERFACE "${HWLOC_LIBRARIES}")
if(APPLE)
  target_link_libraries(HWLOC::HWLOC INTERFACE "-framework Foundation" "-framework IOKit" "-framework OpenCL")
endif()
if(LibXml2_FOUND)
  target_link_libraries(HWLOC::HWLOC INTERFACE LibXml2::LibXml2)
endif()

add_dependencies(HWLOC::HWLOC HWLOC)
