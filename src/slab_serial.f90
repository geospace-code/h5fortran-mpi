program simple
!! use hyperslabs with each worker
!! https://support.hdfgroup.org/ftp/HDF5/examples/parallel/hyperslab_by_row.f90

use, intrinsic :: iso_fortran_env, only : int64, real32, real64, stderr=>error_unit
use hdf5, only : HSIZE_T
use h5mpi, only : hdf5_file
use partition, only : get_simsize
use cli, only : get_cli
use perf, only : print_timing
use kernel, only : gaussian2d

implicit none

type(hdf5_file) :: h5

real(real32), allocatable :: S2(:,:), S3(:,:,:)
real(real64), allocatable :: D2(:,:), D3(:,:,:)

character(1000) :: argv, outfn

integer :: ierr, lx1, lx2, lx3, i, real_bits, comp_lvl
integer :: Nrun

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)

integer(HSIZE_T) :: dims_full(3)

call get_simsize(lx1, lx2, lx3)

print '(a,1x,i0,1x,i0,1x,i0)', 'Serial: lx1, lx2, lx3 =', lx1, lx2, lx3

dims_full = [lx1, lx2, lx3]

!> output HDF5 file to write
Nrun = 1
outfn = ""
real_bits = 32
comp_lvl = 0

do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, outfn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  case("-realbits")
    call get_cli(i, argv, real_bits)
  case ("-comp")
    call get_cli(i, argv, comp_lvl)
  case("-d")
    debug = .true.
  end select
end do

if(len_trim(outfn) == 0) error stop "please specify -o filename to write"

allocate(t_elapsed(Nrun))
if(real_bits == 32) then
  allocate(S2(lx1, lx2), S3(lx1, lx2, lx3))
  S2(1:lx1, 1:lx2) = gaussian2d(lx1, lx2, 1.)
  call random_number(S3)
  S3(1:lx1, 1:lx2, 1:lx3) = 0.1*S3 + spread(gaussian2d(lx1, lx2, 1.), 3, lx3)
elseif(real_bits==64) then
  allocate(D2(lx1, lx2), D3(lx1, lx2, lx3))
  S2(1:lx1, 1:lx2) = gaussian2d(lx1, lx2, 1.)
  call random_number(S3)
  S3(1:lx1, 1:lx2, 1:lx3) = 0.1*S3 + spread(gaussian2d(lx1, lx2, 1.), 3, lx3)
else
  error stop "unknown real_bits: expect 32 or 64"
endif

!! benchmark loop
!! due to filesystem caching, minimum time isn't appropriate
!! better to use mean/median/variance etc.

main : do i = 1, Nrun

  call system_clock(count=tic)
  call h5%open(trim(outfn), action="w", mpi=.false., comp_lvl=comp_lvl, debug=debug)

  if(real_bits == 32) then
    call h5%write("/A2", S2, dims_full(:2))
    call h5%write("/A3", S3, dims_full)
  elseif(real_bits == 64) then
    call h5%write("/A2", D2, dims_full(:2))
    call h5%write("/A3", D3, dims_full)
  endif

  call h5%close()

  call system_clock(count=toc)
  t_elapsed(i) = toc-tic

end do main

!> RESULTS

call print_timing(real_bits, int(dims_full), t_elapsed, real(h5%filesize()))

end program
