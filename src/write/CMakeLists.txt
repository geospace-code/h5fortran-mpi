file(READ writer_template.f90 writer_template)
configure_file(writer.in.f90 writer.f90 @ONLY)
set_directory_properties(PROPERTIES CMAKE_CONFIGURE_DEPENDS
"writer_template.f90")

target_sources(h5mpi PRIVATE
write.f90
${CMAKE_CURRENT_BINARY_DIR}/writer.f90
)
