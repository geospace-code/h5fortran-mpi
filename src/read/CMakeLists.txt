file(READ reader_template.f90 reader_template)

configure_file(reader.in.f90 reader.f90 @ONLY)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS "reader_template.f90")

target_sources(h5mpi PRIVATE
read.f90
${CMAKE_CURRENT_BINARY_DIR}/reader.f90
)
