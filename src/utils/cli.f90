module cli

implicit none (type, external)

contains

subroutine get_cli(i, name, value)
!! handles command line arguments like "-N 35" "-o myfile.txt"
integer, intent(in) :: i
character(*), intent(in) :: name
class(*), intent(inout) :: value

character(1000) :: buf
integer :: ierr

if (command_argument_count() < i+1) error stop "not enough command line arguments"

call get_command_argument(i+1, buf, status=ierr)
if(ierr /= 0 .or. buf(1:1) == "-" .or. len_trim(buf) == 0) error stop trim(name) // " needs an argument"

select type (value)
type is (character(*))
  if(len_trim(buf) == 0) error stop trim(name) // " needs an argument (character)"
  value = trim(buf)
type is (integer)
  read(buf, "(I6)") value
class default
  error stop "unknown argument type for " // trim(name)
end select

end subroutine get_cli


end module cli
