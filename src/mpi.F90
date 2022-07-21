!! procedures that require HDF5-MPI

submodule (h5fortran) mpi_smod

#ifdef h5fortran_HAVE_PARALLEL
use hdf5, only : h5pcreate_f, h5pset_dxpl_mpio_f, h5pset_fapl_mpio_f, &
H5FD_MPIO_COLLECTIVE_F, &
H5P_DATASET_XFER_F
#endif

use hdf5, only : H5P_FILE_ACCESS_F

implicit none (type, external)

contains


module procedure mpi_collective

integer :: ier

xfer_id = H5P_DEFAULT_F

if (.not. use_mpi) return

#ifdef h5fortran_HAVE_PARALLEL

!! Create property list for collective dataset operations
call H5Pcreate_f(H5P_DATASET_XFER_F, xfer_id, ier)
if (ier/=0) error stop "ERROR:h5fortran:h5pcreate dataset xfer: " // dname

call H5Pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ier)
if (ier/=0) error stop "ERROR:h5fortran:h5pset_dxpl_mpio collective: " // dname

! For independent dataset operations
! call H5Pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_INDEPENDENT_F, ier)

#endif

end procedure mpi_collective


module procedure mpi_opener

integer :: ier

fapl = H5P_DEFAULT_F

if (.not. use_mpi) return

#ifdef h5fortran_HAVE_PARALLEL

call mpi_comm_rank(mpi_h5comm, mpi_id, ier)
if(ier /= 0) error stop "ERROR:h5fortran:opener: could not get MPI ID"

!! collective: setup for MPI access
call H5Pcreate_f(H5P_FILE_ACCESS_F, fapl, ier)
if(ier /= 0) error stop "ERROR:h5fortran:opener:h5pcreate could not collective open property for " // filename

call H5Pset_fapl_mpio_f(fapl, mpi_h5comm, mpi_h5info, ier)
if(ier /= 0) error stop "ERROR:h5fortran:opener:h5pset_fapl_mpio could not collective open file for " // filename

#endif

end procedure mpi_opener

end submodule mpi_smod
