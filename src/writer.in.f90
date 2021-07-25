submodule (h5mpi:write) writer

implicit none (type, external)

contains

module procedure ph5write2d_r32

integer :: ierr
integer(HID_T) :: filespace, memspace, dset_id, plist_id

call hdf_create(self, dname, H5T_NATIVE_REAL, shape(A, HSIZE_T), dims_file, filespace, memspace, dset_id, plist_id)

@writer_template@

end procedure ph5write2d_r32


module procedure ph5write3d_r32

integer :: ierr
integer(HID_T) :: filespace, memspace, dset_id, plist_id

call hdf_create(self, dname, H5T_NATIVE_REAL, shape(A, HSIZE_T), dims_file, filespace, memspace, dset_id, plist_id)

@writer_template@

end procedure ph5write3d_r32

end submodule writer
