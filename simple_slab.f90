program simple
!! a more optimal case is to use hyperslabs with each worker
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use mpi
use hdf5

implicit none

character(:), allocatable :: fname, dname

integer(HID_T) :: file_id, dset_id, filespace, memspace, plist_id

real, allocatable :: data(:,:)

integer(HSIZE_T) :: dims_file(rank(data)), dims_mem(rank(data))

integer :: ierr, i, j, k, lx1, lx2, dx1

INTEGER :: comm, info
INTEGER :: Nmpi, mpi_id

INTEGER(HSIZE_T), DIMENSION(2) :: cnt, stride, blk, offset

comm = MPI_COMM_WORLD
info = MPI_INFO_NULL

call mpi_init(ierr)
call mpi_comm_size(comm, Nmpi, ierr)
call mpi_comm_rank(comm, mpi_id, ierr)

fname = "out.h5"
dname = "/x"

!! dummy problem
lx1 = 16
lx2 = 4

if (Nmpi > lx1) error stop "too many MPI workers"
if (modulo(lx1, Nmpi) /= 0) error stop "number of MPI workers must evenly divide problem size."

!! 1-D decompose in rows (neglect ghost cells)
dx1 = lx1 / Nmpi

allocate(data(dx1, lx2))
data = mpi_id

dims_mem = [dx1, lx2]
dims_file = [lx1, lx2]

call h5open_f(ierr)
if(ierr/=0) error stop "h5open"

! collective: setup for MPI access
call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
call h5pset_fapl_mpio_f(plist_id, comm, info, ierr)

! collective: create file
call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, ierr, access_prp = plist_id)
call h5pclose_f(plist_id, ierr)

call h5screate_simple_f(size(dims_file), dims_file, filespace, ierr)
call h5screate_simple_f(size(dims_mem), dims_mem, memspace, ierr)

! collective: create dataset
call h5dcreate_f(file_id, dname, H5T_NATIVE_REAL, filespace, dset_id, ierr)
call h5sclose_f(filespace, ierr)

! Each process defines dataset in memory and writes it to the hyperslab
! in the file.
!
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

! Create property list for collective dataset write
call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)

! For independent write use
! call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)

! collective: Write dataset
call h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, dims_file, ierr, &
  file_space_id=filespace, mem_space_id=memspace, xfer_prp = plist_id)
if (ierr/=0) error stop "h5dwrite"

! wind down
deallocate(data)
call h5sclose_f(filespace, ierr)
call h5dclose_f(dset_id, ierr)
call h5pclose_f(plist_id, ierr)
call h5fclose_f(file_id, ierr)
call h5close_f(ierr)

call mpi_finalize(ierr)

end program
