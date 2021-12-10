integer :: ierr
integer(HID_T) :: filespace, memspace, dset_id, plist_id

call hdf_create(self, dname, dtype=H5T_NATIVE_DOUBLE, dims=shape(A, HSIZE_T), dims_file=dims_file, &
  filespace=filespace, memspace=memspace, dset_id=dset_id, plist_id=plist_id)

call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, A, dims_file, ierr, &
  file_space_id=filespace, &
  mem_space_id=memspace, &
  xfer_prp=plist_id)
if (ierr/=0) error stop "h5dwrite: " // dname // " " // self%filename

call hdf_wrapup(filespace, memspace, dset_id, plist_id)
