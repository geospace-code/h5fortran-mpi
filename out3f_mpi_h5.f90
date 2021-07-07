module original
!! originally by Paul Inchin
use, intrinsic :: iso_fortran_env, only : real64
use mpi
use hdf5

implicit none (type, external)

type param

integer :: x1, x2, x3, Ns

INTEGER(HSIZE_T) :: adims(1) = [25] ! Attribute dimension
INTEGER :: arank = 1 ! Attribute rank

end type param

integer :: mpi_id, mpi_size
integer, parameter :: info = mpi_info_null

contains


subroutine out3f(P, q, fname, dname)

! Routine to save compressed chunked output
! Note current filter is based on GZIP compression
! Old indexing for output is preserved

character(*), intent(in) :: fname, dname
type(param), intent(in) :: P
real(real64), intent(in) :: q(:,:,:,:)

integer :: ierr
integer ::  mstart

integer(hid_t) :: memspace, memd

!! dataset name for specific rank
character(mpi_max_processor_name) :: hostname

!! data dataset dimensions
integer :: i,j,k,l,m,idd

integer(hsize_t) :: cdims(rank(q)) ! chunks data dimensions

!! TODO: add our auto-chunking for 1-16 MB or so per recent recommendations
!! and experimental tuning
cdims = [P%x1 / 8, P%x2 / 16, P%x3 / 16, 1]

call mpi_comm_size(MPI_COMM_WORLD, mpi_size, ierr)
call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr/=0) error stop "mpi_comm"

if(mpi_id==0) print *, "number of MPI images: ", mpi_size
print *, "MPI image online: #",mpi_id

! initialize HDF5 fortran interface
call h5open_f(ierr)

! id 0 creates hdf5 data layout and write attributes
if (mpi_id == 0) call creator(P, fname, dname, int(shape(q), kind=hsize_t), cdims)

! sync all images
call mpi_barrier(MPI_COMM_WORLD, ierr)

call write_file(P,q, fname, dname)


end subroutine out3f


subroutine creator(P, fname, dname, dimsf, cdims)

type(param), intent(in) :: P
character(*), intent(in) :: dname, fname
integer(hsize_t), intent(in) :: dimsf(:), cdims(:)

integer :: ierr

integer(hid_t) :: file_id       ! file identifier
integer(hid_t) :: dcpl          ! property list identifier
integer(hid_t) :: dataset_id    ! dataset identifier
integer(hid_t) :: dataspace_id  ! dataspace identifier

character(3) :: c

! create the hdf5 file
call h5fcreate_f(fname, h5f_acc_trunc_f, file_id, ierr)

! create the dataspace for the dataset
! dimsf: size of every dimension
call h5screate_simple_f(size(dimsf), dimsf, dataspace_id, ierr)

! create properties variable for the data
call h5pcreate_f(h5p_dataset_create_f, dcpl, ierr)

! attribute the chunk size
! dcpl: link this property variable with chunk size
! 4: dimension of data
! cdims: dimensions of chunk in every direction
call h5pset_chunk_f(dcpl, size(dimsf), cdims, ierr)

! attribute the compression type (GZIP compression)
call h5pset_deflate_f(dcpl, 6, ierr)

! Uncomment below and comment previous line to use SZIP compression

!szip_options_mask = H5_SZIP_NN_OM_F
!szip_pixels_per_block = 16
!CALL H5Pset_szip_f(dcpl, szip_options_mask, szip_pixels_per_block, ierr)

! attribute time of allocation of space for data in datasets
! h5d_alloc_time_early_f - allocate all space when the dataset is created
call h5pset_alloc_time_f(dcpl, h5d_alloc_time_early_f, ierr)

! create dataset for this processor (based on id)
! dcpl_id: dataset creation property list
call h5dcreate_f(file_id, dname, h5t_native_double, &
                  dataspace_id, dataset_id, ierr, dcpl_id=dcpl)
if(ierr/=0) error stop "creator:h5dcreate"

call write_attr(P, dataset_id)

call h5dclose_f(dataset_id, ierr)
call h5sclose_f(dataspace_id, ierr)
call h5pclose_f(dcpl, ierr)
call h5fclose_f(file_id, ierr)

end subroutine creator


subroutine write_attr(P, dataset_id)

type(param), intent(in) :: P
integer(hid_t), intent(in) :: dataset_id

INTEGER(HSIZE_T) :: data_dims(1)
real(real64) :: attr_datacur(25)

INTEGER(HID_T) :: atype_id ! Attribute dataspace identifier
INTEGER(HID_T) :: aspace_id ! Attribute dataspace identifier
INTEGER(HID_T) :: attr_id ! Attribute identifier

integer :: ierr

! create datatype for the attribute
! Copy existing datatype
! H5T_NATIVE_CHARACTER - copy this datatype
! atype_id - copy datatype to this variable
call h5tcopy_f(h5t_native_double, atype_id, ierr)

! create scalar dataspace for the attribute
call h5screate_simple_f(P%arank, P%adims, aspace_id, ierr)

! attr_datacur(1) = P%ngrids_out
! attr_datacur(2) = P%nDim
! attr_datacur(3) = P%t
! attr_datacur(4) = P%meqn
! attr_datacur(5) = 1.
! attr_datacur(6) = mtotal(1) ! Full mx (all processors)
! attr_datacur(7) = mtotal(2) ! Full my
! attr_datacur(8) = mtotal(3) ! Full mz
! attr_datacur(9) = 0.
! attr_datacur(10) = P%xlower ! lower boundary x
! attr_datacur(11) = P%ylower ! lower boundary y
! attr_datacur(12) = P%zlower ! lower boundary z
! attr_datacur(13) = 0
! attr_datacur(14) = P%xlower+mtotal(1)*P%dx ! upper boundary x
! attr_datacur(15) = P%ylower+mtotal(2)*P%dy ! upper boundary y
! attr_datacur(16) = P%zlower+mtotal(3)*P%dz ! upper boundary z
! attr_datacur(17) = 0.
! attr_datacur(18) = P%dx
! attr_datacur(19) = P%dy
! attr_datacur(20) = P%dz
! attr_datacur(21) = 0.
! Next variable are not included for HDF4 version but needed for slicing in HDF5
! attr_datacur(22) = lx
! attr_datacur(23) = ly
! attr_datacur(24) = lz

call h5acreate_f(dataset_id,"Parameters",atype_id,aspace_id,attr_id, ierr)

data_dims(1) = 25
call h5awrite_f(attr_id, atype_id, attr_datacur, data_dims, ierr)

! close attribute
call h5aclose_f(attr_id, ierr)

! close access to the dataspace for attribute
call h5sclose_f(aspace_id, ierr)

call h5tclose_f(atype_id, ierr)

end subroutine write_attr


subroutine write_file(P,q, fname, dname)

type(param), intent(in) :: P
real(real64), intent(in) :: q(:,:,:,:)
character(*), intent(in) :: fname, dname

integer(hid_t) :: plist_id      ! property list identifier
integer(hid_t) :: file_id       ! file identifier
integer(hid_t) :: dataset_id    ! dataset identifier
integer(hid_t) :: filespace

integer :: i,np, ierr

character(len=3) :: c

! setup file access property variable with parallel i/o access
call h5pcreate_f(h5p_file_access_f, plist_id, ierr)
call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, info, ierr)

! open hdf5 file
! plist_id: file access property list
call h5fopen_f(fname, h5f_acc_rdwr_f, file_id, ierr, plist_id)

! close the property list
call h5pclose_f(plist_id, ierr)

! create properties variable
! h5p_dataset_xfer_f: property for raw data transfer
call h5pcreate_f(h5p_dataset_xfer_f, plist_id, ierr)
! set collective mpio model
! h5fd_mpio_collective_f: collective is usually faster
call h5pset_dxpl_mpio_f(plist_id, h5fd_mpio_collective_f, ierr)
if (ierr /= 0) error stop "h5pset_dxpl_mpio"

! Parallel compression requires collective writing

! open dataset (each processor opens its own dataset)
call h5dopen_f(file_id, dname, dataset_id, ierr)
if (ierr /= 0) error stop "h5dopen_f"

call h5dget_space_f(dataset_id,filespace,ierr)

if (mpi_id /= 0) call h5sselect_none_f(filespace, ierr)

! write data to dataset
! data: data by itself
! xfer_prp = plist_id: data transfer property variable
call h5dwrite_f(dataset_id, h5t_native_double, &
  q(:, :, :, :), int(shape(q), kind=hsize_t), ierr, &
  file_space_id = filespace, xfer_prp = plist_id)

call h5dclose_f(dataset_id,ierr)

call h5sclose_f(filespace, ierr)

call h5pclose_f(plist_id, ierr)
call h5fclose_f(file_id, ierr)

!	 deallocate(data)

! close fortran interface
call h5close_f(ierr)

end subroutine write_file

end module original
