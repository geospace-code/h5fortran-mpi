program frontend

use, intrinsic :: iso_fortran_env, only: compiler_version
use hwloc_ifc, only : get_cpu_count
use partition, only : max_gcd, get_simsize
use cli, only : get_cli

implicit none (type, external)

integer :: lid, lx1, lx2, lx3, Ncpu, ierr, Nrun, i, argc, comp_lvl
character(2000) :: buf, exe, mpiexec, outfn
character(:), allocatable :: cmd
logical :: exists

argc = command_argument_count()

!> defaults
comp_lvl = 0
Ncpu = 0
Nrun = 1
mpiexec = ""
exe = ""
outfn = ""

do i = 4, argc
  call get_command_argument(i, buf, status=ierr)
  if(ierr/=0) exit

  select case(buf)
  case("-Nrun")
    call get_cli(i, buf, Nrun)
  case("-o")
    call get_cli(i, buf, outfn)
  case("-np")
    call get_cli(i, buf, Ncpu)
  case("-exe")
    call get_cli(i, buf, exe)
  case ("-comp")
    call get_cli(i, buf, comp_lvl)
  case ("-compiler")
    print '(A)', compiler_version()
    stop
  case("-mpiexec")
    call get_cli(i, buf, mpiexec)
    inquire(file=mpiexec, exist=exists)
    if(.not. exists) error stop trim(mpiexec) // " is not a file."
  end select
end do

call get_simsize(lx1, lx2, lx3)

if(len_trim(mpiexec)==0) mpiexec = "mpiexec"
if(len_trim(outfn)==0) error stop "must specify -o <output file>"

if(len_trim(exe)==0) error stop "please specify MPI program to run with option:  -exe myprog.exe"
inquire(file=exe, exist=exists)
if(.not. exists) error stop trim(exe) // " is not a file."

if(Ncpu < 1) Ncpu = get_cpu_count()

lid = max_gcd(lx1, Ncpu)

print '(A,I0)', 'MPI images: ', lid

!> run MPI-based executable
!> need to quote executables in case they have spaces in the path.
!> don't quote "exe" as this makes the CLI invalid syntax--don't have spaces in the exe path.
write(buf, '(A1,A,A1,1X,A2,1X,I0,1X,A,1X,I0,1X,I0,1X,I0,1X,A5,1X,I0,1x,a5,1x,I1,1x,a2,1x,a)') &
  char(34), trim(mpiexec), char(34), '-n', lid, &
  trim(exe), &
  lx1,lx2,lx3, &
  "-Nrun", Nrun, &
  "-comp", comp_lvl, &
  "-o", trim(outfn)

!! quotes are for mpiexec path with spaces
cmd = trim(buf)
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop trim(exe) // ' run failure'

end program
