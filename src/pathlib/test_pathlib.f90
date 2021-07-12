program pathlib_test

use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
use pathlib, only : mkdir, expanduser, is_absolute, directory_exists

implicit none (type, external)

call test_expanduser_absolute()

call test_directory_exists()


contains


subroutine test_directory_exists()

integer :: i

if(.not.(directory_exists('.'))) error stop "did not detect '.' as directory"

open(newunit=i, file='test-pathlib.h5', status='replace')
close(i)
if((directory_exists('test-pathlib.h5'))) error stop "detected file as directory"
call unlink('test-pathlib.h5')

print *," OK: pathlib: directory_exists"
end subroutine test_directory_exists


subroutine test_expanduser_absolute()

character(:), allocatable:: fn
character(16) :: fn2

fn = expanduser("~")
if (fn(1:1) == "/") then
  if (.not.is_absolute("/")) error stop "is_absolute('/') on Unix should be true"
  if (is_absolute("c:/")) error stop "is_absolute('c:/') on Unix should be false"
else
  if (.not.is_absolute("J:/")) error stop "is_absolute('J:/') on Windows should be true"
  if (.not.is_absolute("j:/")) error stop "is_absolute('j:/') on Windows should be true"
  if (is_absolute("/")) error stop "is_absolute('/') on Windows should be false"
endif

print *, "OK: pathlib: expanduser,is_absolute"
end subroutine test_expanduser_absolute


subroutine unlink(path)
character(*), intent(in) :: path
integer :: i
logical :: e

inquire(file=path, exist=e)
if (.not.e) return

open(newunit=i, file=path, status='old')
close(i, status='delete')
end subroutine unlink

end program
