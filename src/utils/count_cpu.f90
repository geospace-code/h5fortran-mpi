program tell_cpu_count
!! tell how many CPUs would be used for this problem
use hwloc_ifc, only : get_cpu_count
use partition, only : max_gcd, get_simsize
use cli, only : get_cli

implicit none (type, external)

character(6) :: argv
integer :: i, Ncpu, lid, argc, lx1, lx2, lx3

Ncpu = 0

argc = command_argument_count()

do i = 4, argc
  call get_command_argument(i, argv, status=i)
  if(i /= 0) exit

  select case(argv)
  case("-np")
    call get_cli(i, argv, Ncpu)
  end select
end do

if (Ncpu == 0) Ncpu = get_cpu_count()

call get_simsize(lx1, lx2, lx3)

lid = max_gcd(lx1, Ncpu)

print '(I0)', lid

end program
