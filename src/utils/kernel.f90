module kernel

implicit none (type, external)

private
public :: phantom, gaussian2d

contains


pure function phantom(M, N, gensig)

real :: phantom(M, N)
integer, intent(in) :: M, N
real, intent(in) :: gensig

integer :: i, j

if (gensig >= 0) then
  phantom = gaussian2d(M, N, gensig)
else
  do j = 1,N
    do i = 1,M
      phantom(i, j) = j + (i-1)*N
    end do
  end do
endif

end function phantom


pure function gaussian2d(M, N, sigma)

real :: gaussian2d(M, N)
integer, intent(in) :: M, N
real, intent(in) :: sigma

integer :: x, y
real :: xx, yy

do y = 1, M
  do x = 1, N
    xx = x - real(N) / 2 - 0.5
    yy = y - real(M) / 2 - 0.5
    gaussian2d(y,x) = 2.0*exp(-0.5 * (xx**2 + yy**2) / sigma**2)
  end do
end do

end function gaussian2d

end module kernel
