program simple
!! use hyperslabs with each worker
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real64, stderr=>error_unit
use hdf5
use h5mpi, only : hdf5_file
use partition, only : get_simsize
use cli, only : get_cli
use perf, only : print_timing

implicit none

type(hdf5_file) :: h5

real, allocatable :: A2(:,:), A3(:,:,:)

character(1000) :: argv, outfn

integer :: ierr, lx1, lx2, lx3, i
integer :: Nrun

integer(int64) :: tic, toc, tmin

integer(HSIZE_T) :: dims_full(rank(A3))

call get_simsize(lx1, lx2, lx3)

print '(a,1x,i0,1x,i0,1x,i0)', 'Serial: lx1, lx2, lx3 =', lx1, lx2, lx3

dims_full = [lx1, lx2, lx3]

!> output HDF5 file to write
Nrun = 1
outfn = ""

do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, outfn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  end select
end do

if(len_trim(outfn) == 0) error stop "please specify -o filename to write"


allocate(A2(lx1, lx2), A3(lx1, lx2, lx3))
!> dummy data
A2 = 1
A3 = 1

!> benchmark loop
tmin = huge(0_int64)
main : do i = 1, Nrun
  call system_clock(count=tic)
  call h5%open(trim(outfn), action="w", mpi=.false., comp_lvl=3)

  call h5%write("/A2", A2, dims_full(:2))
  call h5%write("/A3", A3, dims_full)

  call h5%close()

  call system_clock(count=toc)
  tmin = min(tmin, toc-tic)

end do main

!> RESULTS

call print_timing(storage_size(A3), int(dims_full), tmin, real(h5%filesize()))

end program
