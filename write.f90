submodule (mpi_h5write) write_smod

use mpi, only : mpi_comm_rank

implicit none (type, external)

contains

module procedure ph5write2d_r32

integer :: ierr, mpi_id

integer(HSIZE_T), dimension(rank(A)) :: cnt, stride, blk, offset, dims_mem
integer(HID_T) :: dset_id, filespace, memspace, plist_id

dims_mem = shape(A)

! create dataspace
call h5screate_simple_f(size(dims_file), dims_file, filespace, ierr)
call h5screate_simple_f(size(dims_mem), dims_mem, memspace, ierr)

! collective: create dataset
call h5dcreate_f(self%file_id, dname, H5T_NATIVE_REAL, filespace, dset_id, ierr)
call h5sclose_f(filespace, ierr)

! Each process defines dataset in memory and writes it to the hyperslab
! in the file.
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

!> chunk choices are arbitrary, but must be the same on all processes
!> only chunking along first dim
cnt = [integer(hsize_t) :: dims_mem(1), 1]
offset = [integer(hsize_t) ::mpi_id*cnt(1), 0]
stride = [integer(hsize_t) :: 1, 1]
blk = [integer(hsize_t) :: 1, dims_file(2)]

! Select hyperslab in the file.

CALL h5dget_space_f(dset_id, filespace, ierr)
CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, &
  start=offset, &
  count=cnt, hdferr=ierr, &
  stride=stride, &
  block=blk)
if (ierr/=0) error stop "h5sselect_hyperslab"

! Create property list for collective dataset write
call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)

! For independent write use
! call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)

! collective: Write dataset
call h5dwrite_f(dset_id, H5T_NATIVE_REAL, A, dims_file, ierr, &
  file_space_id=filespace, mem_space_id=memspace, xfer_prp = plist_id)
if (ierr/=0) error stop "h5dwrite"

! wind down
call h5sclose_f(filespace, ierr)
call h5dclose_f(dset_id, ierr)
call h5pclose_f(plist_id, ierr)

end procedure ph5write2d_r32


module procedure ph5write3d_r32

integer :: ierr, mpi_id

integer(HSIZE_T), dimension(rank(A)) :: cnt, stride, blk, offset, dims_mem
integer(HID_T) :: dset_id, filespace, memspace, plist_id

dims_mem = shape(A)

! create dataspace
call h5screate_simple_f(size(dims_file), dims_file, filespace, ierr)
call h5screate_simple_f(size(dims_mem), dims_mem, memspace, ierr)

! collective: create dataset
call h5dcreate_f(self%file_id, dname, H5T_NATIVE_REAL, filespace, dset_id, ierr)
call h5sclose_f(filespace, ierr)

! Each process defines dataset in memory and writes it to the hyperslab
! in the file.
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

!> chunk choices are arbitrary, but must be the same on all processes
!> only chunking along first dim
cnt = [integer(hsize_t) :: dims_mem(1), 1, 1]
offset = [integer(hsize_t) ::mpi_id*cnt(1), 0, 0]
stride = [integer(hsize_t) :: 1, 1, 1]
blk = [integer(hsize_t) :: 1, dims_file(2), dims_file(3)]

! Select hyperslab in the file.

CALL h5dget_space_f(dset_id, filespace, ierr)
CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, &
  start=offset, &
  count=cnt, hdferr=ierr, &
  stride=stride, &
  block=blk)
if (ierr/=0) error stop "h5sselect_hyperslab"

! Create property list for collective dataset write
call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)

! For independent write use
! call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)

! collective: Write dataset
call h5dwrite_f(dset_id, H5T_NATIVE_REAL, A, dims_file, ierr, &
  file_space_id=filespace, mem_space_id=memspace, xfer_prp = plist_id)
if (ierr/=0) error stop "h5dwrite"

! wind down
call h5sclose_f(filespace, ierr)
call h5dclose_f(dset_id, ierr)
call h5pclose_f(plist_id, ierr)

end procedure ph5write3d_r32

end submodule write_smod
