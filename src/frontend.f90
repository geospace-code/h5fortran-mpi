program frontend

use hwloc_ifc, only : get_cpu_count
use partition, only : max_mpi

implicit none (type, external)

integer :: lid, lx2, lx3, Ncpu, ierr
character(1000) :: buf
character(:), allocatable :: cmd, exe
logical :: exists

call get_command_argument(1, buf, status=ierr)
if(ierr/=0) error stop "please input MPI-based program to run"
exe = trim(buf)
inquire(file=exe, exist=exists)
if(.not. exists) error stop exe // " is not a file."

Ncpu = get_cpu_count()

lid = max_mpi(lx2, lx3, Ncpu)

print '(A,I0)', 'MPI images: ', lid

!> run MPI-based executable
write(buf, '(A1,A,A,I0,1X,A,1X,A,1X,A)') 'mpiexec -n ', lid, exe

!! quotes are for mpiexec path with spaces
cmd = trim(buf)
print *, cmd
call execute_command_line(cmd, exitstat=ierr)

if (ierr/=0) error stop exe // ' run failure'

end program
