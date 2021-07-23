!> create dataspace
call h5screate_simple_f(size(dims_file), dims_file, filespace, ierr)
if (ierr/=0) error stop "h5screate_simple: " // dname // " " // self%filename
call h5screate_simple_f(size(dims_mem), dims_mem, memspace, ierr)

!> collective: create dataset
call h5dcreate_f(self%file_id, dname, H5T_NATIVE_REAL, filespace, dset_id, ierr)
if (ierr/=0) error stop "h5dcreate: " // dname // " " // self%filename
call h5sclose_f(filespace, ierr)

!> Select hyperslab in the file.
CALL h5dget_space_f(dset_id, filespace, ierr)
CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, &
  start=offset, &
  count=cnt, hdferr=ierr, &
  stride=stride, &
  block=blk)
if (ierr/=0) error stop "h5sselect_hyperslab: " // dname // " " // self%filename

if(self%use_mpi) then
  !> Create property list for collective dataset write
  call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
  call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)

  ! For independent write use
  ! call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)
else
  plist_id = H5P_DEFAULT_F
endif



!> collective: Write dataset
call h5dwrite_f(dset_id, H5T_NATIVE_REAL, A, dims_file, ierr, &
  file_space_id=filespace, mem_space_id=memspace, xfer_prp = plist_id)
if (ierr/=0) error stop "h5dwrite: " // dname // " " // self%filename

!> wind down
call h5sclose_f(filespace, ierr)
call h5dclose_f(dset_id, ierr)
call h5pclose_f(plist_id, ierr)
