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
if(ierr /= 0) error stop "could not get argument for " // trim(name)
if(len_trim(buf) == 0) error stop trim(name) // " needs an argument"

select type (value)
type is (character(*))
  value = trim(buf)
type is (integer)
  read(buf, "(i6)") value
type is (real)
  read(buf, "(f8.3)") value
class default
  error stop "unknown argument type for " // trim(name)
end select

end subroutine get_cli


subroutine get_simsize(lx1, lx2, lx3, Nmpi)

integer, intent(out) :: lx1, lx2, lx3
integer, intent(in), optional :: Nmpi

integer :: i, argc
character(9) :: buf

lx1 = -1
lx2 = -1
lx3 = -1

argc = command_argument_count()
if(argc < 4) error stop "must input at least: -lx lx1 lx2 lx3"

do i = 1,argc-3

call get_command_argument(i, buf)
select case (buf)
  case ("-lx")
    call get_command_argument(i+1, buf)
    read(buf, '(I9)') lx1
    call get_command_argument(i+2, buf)
    read(buf, '(I9)') lx2
    call get_command_argument(i+3, buf)
    read(buf, '(I9)') lx3
end select

end do

if (any([lx1, lx2] < 1)) error stop "must input -lx lx1 lx2 lx3"

if(.not.present(Nmpi)) return

!> MPI sanity check
if (Nmpi > lx1) error stop "too many MPI workers"
if (modulo(lx1, Nmpi) /= 0) error stop "number of MPI workers must evenly divide problem size."

end subroutine get_simsize


end module cli
