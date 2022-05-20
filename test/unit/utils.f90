module test_utils

implicit none (type, external)

contains

subroutine unlink(filename)

character(*), intent(in) :: filename
integer :: u

open(newunit=u, file=filename, status='unknown')
close(u, status='delete')


end subroutine unlink


end module test_utils
