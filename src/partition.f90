module partition

implicit none (type, external)

contains

integer function gcd(a, b)

integer, intent(in) :: a, b
integer :: x,y,z

if (a < 1 .or. b < 1) error stop "autogrid:gcd positive integers only"

x = a
y = b
z = modulo(x, y)
do while (z /= 0)
  x = y
  y = z
  z = modulo(x, y)
end do
gcd = y

end function gcd


integer function max_mpi(lx2, lx3, max_cpu)
!! goal is to find the highest x2 + x3 to maximum CPU core count
integer, intent(in) :: lx2, lx3, max_cpu

!> divide by 2 to ensure MPI partition has at least 2 per axis--for 2D and 3D
if (lx3 == 1) then
  max_mpi = max_gcd(lx2/2, max_cpu)
elseif (lx2 == 1) then
  max_mpi = max_gcd(lx3/2, max_cpu)
else
  max_mpi = max_gcd2(lx2/2, lx3/2, max_cpu)
end if

end function max_mpi


integer function max_gcd(L, M)
!! find the Greatest Common Factor to evenly partition the simulation grid
!! Output range is [M, 1]

integer, intent(in) :: L,M
integer :: i

if (M < 1) error stop "max_gcd: CPU count must be at least one"

max_gcd = 1
do i = M, 2, -1
  max_gcd = max(gcd(L, i), max_gcd)
  if (i < max_gcd) exit
end do

end function max_gcd


integer function max_gcd2(lx2, lx3, M)
!! find the Greatest Common Factor to evenly partition the simulation grid
!! Output range is [M, 1]
!!
!!    1. find factors of each dimension
!!    2. choose partition that yields highest CPU count usage

integer, intent(in) :: lx2, lx3, M
integer :: f2, f3, i,j, t2, t3

if (M < 1) error stop "max_gcd2: CPU count must be at least one"

max_gcd2 = 1
t2 = 1
t3 = huge(0)
x2 : do i = M,2,-1
  x3 : do j = M,2,-1
    f2 = max_gcd(lx2, i)
    f3 = max_gcd(lx3, j)
    if (M < f2 * f3) cycle x3 !< too big for CPU count
    if (f2 * f3 < max_gcd2) exit x3 !< too small for max CPU count
    if (lx2 / i == 1) cycle x2
    if (lx3 / j == 1) cycle x3
    if (abs(f2-f3) > abs(t2-t3)) cycle x3

    t2 = f2
    t3 = f3
    ! print *, "lid2, lid3: ", f2, f3
    max_gcd2 = f2 * f3
  end do x3
  if (i*M < max_gcd2) exit x2
end do x2

! print *, "final: lid2, lid3: ", t2, t3

end function max_gcd2


end module partition
