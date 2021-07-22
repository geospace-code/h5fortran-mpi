program frontend

use hwloc_ifc, only : get_cpu_count
use partition, only : max_gcd, get_simsize
use cli, only : get_cli

implicit none (type, external)

integer :: lid, lx1, lx2, lx3, Ncpu, ierr, u, Nrun, i, argc
character(1000) :: buf, exe, mpiexec
character(:), allocatable :: cmd, sizefn
logical :: exists

argc = command_argument_count()

call get_simsize(lx1, lx2, lx3)

!> defaults
Ncpu = 0
Nrun = 1

do i = 4, argc
  call get_command_argument(i, buf, status=ierr)
  if(ierr/=0) exit

  select case(buf)
    case("-Nrun")
    call get_cli(i, buf, Nrun)
  case("-np")
    call get_cli(i, buf, Ncpu)
  case("-exe")
    call get_cli(i, buf, exe)
  case("-mpiexec")
    call get_cli(i, buf, mpiexec)
    inquire(file=mpiexec, exist=exists)
    if(.not. exists) error stop trim(mpiexec) // " is not a file."
  end select
end do

if(len_trim(mpiexec)==0) mpiexec = "mpiexec"

if(len_trim(exe)==0) error stop "please specify MPI program to run with option:  -exe myprog.exe"
inquire(file=exe, exist=exists)
if(.not. exists) error stop trim(exe) // " is not a file."

if(Ncpu < 1) Ncpu = get_cpu_count()

lid = max_gcd(lx1, Ncpu)

print '(A,I0)', 'MPI images: ', lid

!> run MPI-based executable
!> need to quote executables in case they have spaces in the path.
!> don't quote "exe" as this makes the CLI invalid syntax--don't have spaces in the exe path.
write(buf, '(A1,A,A1,1X,A2,1X,I0,1X,A,1X,I0,1X,I0,1X,I0,1X,A5,1X,I0)') &
  char(34), trim(mpiexec), char(34), '-n', lid, &
  trim(exe), &
  lx1,lx2,lx3, &
  "-Nrun", Nrun

!! quotes are for mpiexec path with spaces
cmd = trim(buf)
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop trim(exe) // ' run failure'

end program
