program frontend

use hwloc_ifc, only : get_cpu_count
use partition, only : max_gcd

implicit none (type, external)

integer :: lid, lx1, lx2, lx3, Ncpu, ierr, u
character(1000) :: buf
character(:), allocatable :: cmd, exe, mpiexec, sizefn
logical :: exists

sizefn = "simsize.txt"
inquire(file=sizefn, exist=exists)
if(.not.exists) error stop sizefn // ' not found'

!> defaults
Ncpu = 0
exe = ""
mpiexec = "mpiexec"

do u = 1,command_argument_count()
  call get_command_argument(u, buf, status=ierr)
  if(ierr/=0) exit
  select case(buf)
  case("-np")
    call get_command_argument(u+1, buf, status=ierr)
    if(ierr /= 0 .or. buf(1:1) == "-") error stop "-np needs an argument"
    read(buf, '(I6)') Ncpu
  case("-exe")
    buf = ""
    call get_command_argument(u+1, buf, status=ierr)
    if(ierr /= 0 .or. buf(1:1) == "-") error stop "-exe needs an argument"
    exe = trim(buf)
  case("-mpiexec")
    buf = ""
    call get_command_argument(u+1, buf, status=ierr)
    if(ierr /= 0 .or. buf(1:1) == "-") error stop "-mpiexec needs an argument"
    mpiexec = trim(buf)
    inquire(file=mpiexec, exist=exists)
    if(.not. exists) error stop mpiexec // " is not a file."
  end select
end do

if(len_trim(exe)==0) error stop "please specify MPI program to run with option:  -exe myprog.exe"
inquire(file=exe, exist=exists)
if(.not. exists) error stop exe // " is not a file."

if(Ncpu < 1) Ncpu = get_cpu_count()

! dummy problem
open(newunit=u, file="simsize.txt", action="read", status='old', iostat=ierr)
read(u, *) lx1, lx2, lx3
close(u)

lid = max_gcd(lx1, Ncpu)

print '(A,I0)', 'MPI images: ', lid

!> run MPI-based executable
write(buf, '(A1,A,A1,1X,A2,1X,I0,1X,A)') char(34),mpiexec,char(34), '-n', lid, exe

!! quotes are for mpiexec path with spaces
cmd = trim(buf)
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop exe // ' run failure'

end program
