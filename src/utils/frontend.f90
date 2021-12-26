program frontend

use, intrinsic :: iso_fortran_env, only: compiler_version
use hwloc_ifc, only : get_cpu_count
use partition, only : max_gcd
use cli, only : get_cli, get_simsize

implicit none (type, external)

integer :: lid, lx1, lx2, lx3, Ncpu, ierr, Nrun, i, comp_lvl
character(2000) :: buf, exe, mpiexec, outfn, refh5fn
character(:), allocatable :: cmd, args
logical :: exists

!> defaults
comp_lvl = 0
Ncpu = -1
Nrun = 1
lx1 = -1
lx2 = -1
lx3 = -1
lid = -1
mpiexec = "mpiexec"
exe = ""
outfn = ""
refh5fn = ""

do i = 1, command_argument_count()
  call get_command_argument(i, buf, status=ierr)
  if(ierr/=0) exit

  select case(buf)
  case("-lx")
    call get_simsize(lx1, lx2, lx3)
  case("-Nrun")
    call get_cli(i, buf, Nrun)
  case("-o")
    call get_cli(i, buf, outfn)
  case("-ref")
    call get_cli(i, buf, refh5fn)
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

if (any([lx1, lx2, lx3] < 1)) error stop "use '-lx lx1 lx2 lx3' to set the simulation size"

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
write(buf, '(A1,A,A1,1X,A2,1X,I0,1X,A,1X,A3,1X,I0,1X,I0,1X,I0,1X,A5,1X,I0,1x,a5,1x,I1,1x,a2,1x,a,1x,a4)') &
  char(34), trim(mpiexec), char(34), '-n', lid, &
  trim(exe), &
  "-lx", lx1,lx2,lx3, &
  "-Nrun", Nrun, &
  "-comp", comp_lvl, &
  "-o", trim(outfn)

if (len_trim(refh5fn) > 0) then
  args = " -ref " // trim(refh5fn)
else
  args = ""
endif

!! quotes are for mpiexec path with spaces
cmd = trim(buf) // args
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop trim(exe) // ' run failure'

end program
