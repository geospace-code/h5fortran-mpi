program frontend

use hwloc_ifc, only : get_cpu_count
use partition, only : max_gcd

implicit none (type, external)

integer :: lid, lx1, lx2, lx3, Ncpu, ierr, u
character(1000) :: buf
character(:), allocatable :: cmd, exe, extra, sizefn
logical :: exists

sizefn = "simsize.txt"

inquire(file=sizefn, exist=exists)
if(.not.exists) error stop sizefn // ' not found'

call get_command_argument(1, buf, status=ierr)
if(ierr/=0) error stop "please input MPI-based program to run"
exe = trim(buf)
inquire(file=exe, exist=exists)
if(.not. exists) error stop exe // " is not a file."

extra = ""
call get_command_argument(2, buf, status=ierr)
if(ierr==0) extra = trim(buf)

Ncpu = get_cpu_count()

! dummy problem
open(newunit=u, file="simsize.txt", action="read", status='old', iostat=ierr)
read(u, *) lx1, lx2, lx3
close(u)

lid = max_gcd(lx1, Ncpu)

print '(A,I0)', 'MPI images: ', lid

!> run MPI-based executable
write(buf, '(A,1X,I0,1X,A,1X,A)') 'mpiexec -n', lid, exe, extra

!! quotes are for mpiexec path with spaces
cmd = trim(buf)
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop exe // ' run failure'

end program
