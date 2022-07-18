program test_runner

use, intrinsic :: iso_fortran_env, only: compiler_version
use, intrinsic :: iso_c_binding, only : C_INT
use partition, only : max_gcd
use cli, only : get_cli

implicit none (type, external)

interface
integer(c_int) function cpu_count() bind(c, name="cpu_count")
import c_int
end function cpu_count
end interface


integer :: lid, lx2, Ncpu, ierr, i, comp_lvl
character(2000) :: buf, exe, mpiexec
character(:), allocatable :: cmd, args
logical :: exists, debug

!> defaults
debug = .false.
comp_lvl = 0
Ncpu = -1
lx2 = -1
lid = -1
mpiexec = "mpiexec"
exe = ""

do i = 1, command_argument_count()
  call get_command_argument(i, buf, status=ierr)
  if(ierr/=0) exit

  select case(buf)
  case("-lx")
    call get_cli(i, buf, lx2)
  case("-np")
    call get_cli(i, buf, Ncpu)
  case("-exe")
    call get_cli(i, buf, exe)
  case ("-comp")
    call get_cli(i, buf, comp_lvl)
  case("-d", "-debug")
    debug = .true.
  case ("-compiler")
    print '(A)', compiler_version()
    stop
  case("-mpiexec")
    call get_cli(i, buf, mpiexec)
    inquire(file=mpiexec, exist=exists)
    if(.not. exists) error stop trim(mpiexec) // " is not a file."
  end select
end do

if (lx2 < 1) error stop "use '-lx lx2' to set the simulation size"

if(len_trim(exe)==0) error stop "please specify MPI program to run with option:  -exe myprog.exe"
inquire(file=exe, exist=exists)
if(.not. exists) error stop trim(exe) // " is not a file."

if(Ncpu < 1) Ncpu = cpu_count()
lid = max_gcd(lx2, Ncpu)

print '(A,I0)', 'MPI images: ', lid

!> run MPI-based executable
!> need to quote executables in case they have spaces in the path.
!> don't quote "exe" as this makes the CLI invalid syntax--don't have spaces in the exe path.
write(buf, '(A1,A,A1,1X,A2,1X,I0,1X,A,1X,A3,1X,I0,1X,A5,1X,I0)') &
  char(34), trim(mpiexec), char(34), '-n', lid, &
  trim(exe), &
  "-lx", lx2, &
  "-comp", comp_lvl

args = ""
if (debug) args = args // " -debug"

!! quotes are for mpiexec path with spaces
cmd = trim(buf) // args
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop trim(exe) // ' run failure'

end program
