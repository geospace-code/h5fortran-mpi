program simple
!! this example has undesirable effect that all workers must have copy of data.
!! we "broadcast" our simulated data here implicitly
!! a more optimal case is to use hyperslabs with each worker

use mpi
use hdf5

implicit none

character(:), allocatable :: fname, dname

integer(HID_T) :: file_id, dset_id, filespace, plist_id

real, allocatable :: data(:,:)

integer(HSIZE_T) :: ddims(rank(data))

integer :: ierr, i, j, k

INTEGER :: comm, info
INTEGER :: Nmpi, mpi_id

comm = MPI_COMM_WORLD
info = MPI_INFO_NULL

call mpi_init(ierr)
call mpi_comm_size(comm, Nmpi, ierr)
call mpi_comm_rank(comm, mpi_id, ierr)

fname = "out.h5"
dname = "/x"

allocate(data(10, 8))

! fake data
do i = 1,Nmpi
  j = (i-1) * size(data, 1) / Nmpi + 1
  k = i * size(data, 1) / Nmpi
  !if (i-1 == mpi_id)
  data(j:k, :) = i
enddo

ddims = shape(data)

call h5open_f(ierr)
if(ierr/=0) error stop "h5open"

! collective: setup for MPI access
call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
call h5pset_fapl_mpio_f(plist_id, comm, info, ierr)

! collective: create file
call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, ierr, access_prp = plist_id)
call h5pclose_f(plist_id, ierr)
if(ierr/=0) error stop "h5pclose"

call h5screate_simple_f(size(ddims), ddims, filespace, ierr)

! collective: create dataset
call h5dcreate_f(file_id, dname, H5T_NATIVE_REAL, filespace, dset_id, ierr)

! Create property list for collective dataset write
call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, ierr)
call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, ierr)

! For independent write use
! call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, ierr)

! collective: Write dataset
call h5dwrite_f(dset_id, H5T_NATIVE_REAL, data, ddims, ierr, xfer_prp = plist_id)
if (ierr/=0) error stop "h5dwrite"

! wind down
deallocate(data)
call h5sclose_f(filespace, ierr)
call h5dclose_f(dset_id, ierr)
call h5pclose_f(plist_id, ierr)
call h5fclose_f(file_id, ierr)
call h5close_f(ierr)

call mpi_finalize(ierr)
if (ierr/=0) error stop "mpi_finalize"

print *,"OK: write collective"

end program
