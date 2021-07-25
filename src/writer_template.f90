!> Write dataset
call h5dwrite_f(dset_id, H5T_NATIVE_REAL, A, dims_file, ierr, &
  file_space_id=filespace, mem_space_id=memspace, xfer_prp = plist_id)
if (ierr/=0) error stop "h5dwrite: " // dname // " " // self%filename

call hdf_wrapup(filespace, memspace, dset_id, plist_id)
