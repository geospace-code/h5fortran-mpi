# verify HDF5 was configured for parallel compression
# needs a run test as sometimes libhdf5.settings says filter collective
# is enabled when it actually fails at runtime (Windows Intel oneAPI)

include(CheckSourceRuns)


function(hdf5_compression_flag)

if(DEFINED CACHE{hdf5_parallel_compression})
  return()
endif()

cmake_path(GET HDF5_C_LIBRARY PARENT_PATH HDF5_LIBRARY_DIR)
cmake_path(GET HDF5_LIBRARY_DIR PARENT_PATH HDF5_DIR)

message(CHECK_START "Checking if HDF5 configured for parallel compression")

if(HDF5_VERSION VERSION_LESS 1.10.2)
  # https://www.hdfgroup.org/2018/04/why-should-i-care-about-the-hdf5-1-10-2-release/
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression: HDF5 < 1.10.2")
  message(CHECK_FAIL "NO - HDF5 version ${HDF5_VERSION} < 1.10.2")
  return()
endif()

if(MPI_VERSION VERSION_LESS 3)
  message(CHECK_FAIL "NO - MPI version ${MPI_VERSION} < 3")
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression: MPI < 3")
  return()
endif()

find_file(hdf5_settings_file
NAMES libhdf5_openmpi.settings libhdf5_mpich.settings libhdf5.settings
HINTS ${HDF5_LIBRARY_DIR} ${HDF5_DIR}
PATH_SUFFIXES lib hdf5/openmpi hdf5/mpich share/hdf5-mpi share/hdf5 share
NO_DEFAULT_PATH
REQUIRED
)

# self/general: lib share
# Ubuntu: hdf5/openmpi hdf5/mpich
# Homebrew: share/hdf5-mpi share/hdf5

file(READ ${hdf5_settings_file} hdf5_settings)
string(REGEX MATCH "Parallel Filtered Dataset Writes:[ ]*([a-zA-Z]+)" hdf5_parallel_compression_match ${hdf5_settings})
if(${CMAKE_MATCH_1})

  set(CMAKE_REQUIRED_LIBRARIES HDF5::HDF5 MPI::MPI_Fortran MPI::MPI_C)

  set(src
  [=[
  program test_fortran_mpi

use, intrinsic :: iso_fortran_env, only : real32
use hdf5
use mpi

implicit none

integer :: ierr, mpi_id
integer(HID_T) :: fapl, dcpl, xfer_id, fid, dset_id, filespace
integer(HSIZE_T) :: chunk_size(2) = [2, 2]

real(real32), allocatable :: A(:,:)

call mpi_init(ierr)
if (ierr/=0) error stop "mpi_init"

call mpi_comm_rank(MPI_COMM_WORLD, mpi_id, ierr)
if (ierr/=0) error stop "mpi_comm_rank"

call h5open_f(ierr)
if (ierr/=0) error stop "h5open"

call h5pcreate_f(H5P_FILE_ACCESS_F, fapl, ierr)
if (ierr/=0) error stop "h5pcreate_f"
call h5pset_fapl_mpio_f(fapl, MPI_COMM_WORLD, MPI_INFO_NULL, ierr)
if (ierr/=0) error stop "h5pset_fapl_mpio"

call h5fcreate_f('junk.h5', H5F_ACC_TRUNC_F, fid, ierr, access_prp=fapl)
if (ierr/=0) error stop "h5fcreate"
call h5pclose_f(fapl, ierr)

call h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, ierr)
call h5pset_dxpl_mpio_f(xfer_id, H5FD_MPIO_COLLECTIVE_F, ierr)
if (ierr/=0) error stop "h5pset_dxpl_mpio"

if (mpi_id == 0) then
  allocate(A(10, 10))

  call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl, ierr)
  if (ierr/=0) error stop "h5pcreate"

  call h5pset_chunk_f(dcpl, rank(A), chunk_size, ierr)
  if (ierr/=0) error stop "h5pset_chunk"

  call h5pset_deflate_f(dcpl, 1, ierr)
  if (ierr/=0) error stop "h5pset_deflate"
else
  call h5sselect_none_f(filespace, ierr)
endif

call h5screate_simple_f(rank(A), shape(A, HSIZE_T), space_id=filespace, hdferr=ierr)
if (ierr/=0) error stop "h5screate_simple"

!> create dataset
call h5dcreate_f(fid, "/test", H5T_NATIVE_REAL, space_id=filespace, dset_id=dset_id, hdferr=ierr, dcpl_id=dcpl)
if (ierr/=0) error stop "h5dcreate"

call h5dwrite_f(dset_id, H5T_NATIVE_REAL, &
A, shape(A, HSIZE_T), ierr, &
file_space_id = filespace, xfer_prp = xfer_id)
if (ierr/=0) error stop "h5dwrite"

call h5dclose_f(dset_id,ierr)
call h5pclose_f(xfer_id, ierr)
call h5pclose_f(dcpl, ierr)
call h5sclose_f(filespace, ierr)
call h5fclose_f(fid, ierr)
if (ierr/=0) error stop "h5fclose"

call h5close_f(ierr)

call mpi_finalize(ierr)

end program
]=])

  check_source_runs(Fortran ${src} hdf5_parallel_compression_run)

  if(hdf5_parallel_compression_run)
    message(CHECK_PASS "${CMAKE_MATCH_1}")
    set(hdf5_parallel_compression .true. CACHE STRING "HDF5-MPI configured for parallel compression")
  else()
    message(CHECK_FAIL "HDF5-MPI run check failed")
    set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression")
  endif()
else()
  message(CHECK_FAIL "NO")
  set(hdf5_parallel_compression .false. CACHE STRING "HDF5-MPI does not have parallel compression")
endif()

endfunction()
