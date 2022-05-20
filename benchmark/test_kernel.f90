program test_kernel

use kernel, only : gaussian2d

implicit none (type, external)

character(4) :: argv
integer :: x, y, i
real, allocatable :: A(:,:)

call get_command_argument(1, argv)
read(argv,'(i4)') x
call get_command_argument(2, argv)
read(argv,'(i4)') y

allocate(A(y,x))

A = gaussian2d(x,y, 1.4)

do i = 1, y
  print *, A(i,:) * 100
end do

end program
