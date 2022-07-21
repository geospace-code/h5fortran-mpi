program read_slab_serial
!! no MPI / parallel at all

use, intrinsic :: iso_fortran_env, only : int64, real32, real64, stderr=>error_unit

use h5fortran, only : hdf5_file, HSIZE_T

use cli, only : get_cli
use perf, only : print_timing

implicit none

type(hdf5_file) :: h5

real(real32), allocatable :: S3(:,:,:)
real(real64), allocatable :: D3(:,:,:)
character(1000) :: argv

!> default parameters
integer :: real_bits = 32, Nrun = 1
character(1000) :: h5fn = ""
integer(HSIZE_T) :: lx1 = -1, lx2 = -1, lx3 = -1

integer :: ierr, i

logical :: debug = .false.

integer(int64) :: tic, toc
integer(int64), allocatable :: t_elapsed(:)

integer(HSIZE_T), allocatable :: dims_full(:)


do i = 1, command_argument_count()
  call get_command_argument(i, argv, status=ierr)
  if(ierr/=0) error stop "unknown argument: " // argv

  select case(argv)
  case("-o")
    call get_cli(i, argv, h5fn)
  case("-Nrun")
    call get_cli(i, argv, Nrun)
  case("-realbits")
    call get_cli(i, argv, real_bits)
  case("-d", "-debug")
    debug = .true.
  end select
end do

if(len_trim(h5fn) == 0) error stop "please specify -o HDF5 filename"

!> get simsize
call h5%open(trim(h5fn), action="r", mpi=.false., debug=debug)
call h5%shape("/A3", dims_full)
call h5%close()

print '(a,1x,3i5)', 'Serial: lx1, lx2, lx3 =', dims_full
lx1 = dims_full(1)
lx2 = dims_full(2)
lx3 = dims_full(3)

!> read arrays
allocate(t_elapsed(Nrun))
if(real_bits == 32) then
  allocate(S3(lx1, lx2, lx3))
elseif(real_bits==64) then
  allocate(D3(lx1, lx2, lx3))
else
  error stop "unknown real_bits: expect 32 or 64"
endif

!! benchmark loop
!! due to filesystem caching, minimum time isn't appropriate
!! better to use mean/median/variance etc.

main : do i = 1, Nrun

  call system_clock(count=tic)
  call h5%open(trim(h5fn), action="r", mpi=.false., debug=debug)

  if(real_bits == 32) then
    call h5%read("/A3", S3)
  elseif(real_bits == 64) then
    call h5%read("/A3", D3)
  endif

  call h5%close()

  call system_clock(count=toc)
  t_elapsed(i) = toc-tic

end do main

!> RESULTS

call h5%open(trim(h5fn), action="r", mpi=.false.)
call print_timing(1, h5%comp_lvl, real_bits, int(dims_full), t_elapsed, h5%filesize(), debug, &
trim(h5fn) // ".read_stat.h5")
call h5%close()

end program
