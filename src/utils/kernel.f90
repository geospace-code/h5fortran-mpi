module kernel

implicit none (type, external)

contains


function gaussian2d(M, N, sigma)

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
