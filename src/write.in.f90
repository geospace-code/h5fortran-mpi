submodule (mpi_h5write) write_smod

use mpi, only : mpi_comm_rank

implicit none (type, external)

contains

module procedure ph5write2d_r32

integer :: ierr, mpi_id

integer(HSIZE_T), dimension(rank(A)) :: cnt, stride, blk, offset, dims_mem
integer(HID_T) :: dset_id, filespace, memspace, plist_id

dims_mem = shape(A)

! Each process defines dataset in memory and writes it to the hyperslab in the file.
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

!> chunk choices are arbitrary, but must be the same on all processes
!> only chunking along first dim
cnt(1) = dims_mem(1)
cnt(2:) = 1
offset(1) = mpi_id*cnt(1)
offset(2:) = 0
stride = 1
blk(1) = 1
blk(2:) = dims_file(2:)

@writer_template@

end procedure ph5write2d_r32


module procedure ph5write3d_r32

integer :: ierr, mpi_id

integer(HSIZE_T), dimension(rank(A)) :: cnt, stride, blk, offset, dims_mem
integer(HID_T) :: dset_id, filespace, memspace, plist_id

dims_mem = shape(A)
! Each process defines dataset in memory and writes it to the hyperslab in the file.
call mpi_comm_rank(mpi_h5comm, mpi_id, ierr)

!> chunk choices are arbitrary, but must be the same on all processes
!> only chunking along first dim
cnt(1) = dims_mem(1)
cnt(2:) = 1
offset(1) = mpi_id*cnt(1)
offset(2:) = 0
stride = 1
blk(1) = 1
blk(2:) = dims_file(2:)

@writer_template@

end procedure ph5write3d_r32

end submodule write_smod
