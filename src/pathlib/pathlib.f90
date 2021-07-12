module pathlib

use, intrinsic:: iso_fortran_env, only: stderr=>error_unit

implicit none (type, external)
private
public :: mkdir, copyfile, expanduser, home, get_suffix, &
  filesep_windows, filesep_unix, &
  directory_exists, assert_directory_exists, assert_file_exists,&
  is_absolute, parent, file_name

interface  ! pathlib_{unix,windows}.f90
module subroutine copyfile(source, dest)
character(*), intent(in) :: source, dest
end subroutine copyfile

module subroutine mkdir(path)
character(*), intent(in) :: path
end subroutine mkdir

module logical function is_absolute(path)
character(*), intent(in) :: path
end function is_absolute

end interface

interface !< pathlib_{intel,gcc}.f90
module logical function directory_exists(path) result(exists)
character(*), intent(in) :: path
end function directory_exists
end interface


contains

pure function get_suffix(filename)
!! extracts path suffix, including the final "." dot
character(*), intent(in) :: filename
character(:), allocatable :: get_suffix

get_suffix = filename(index(filename, '.', back=.true.) : len_trim(filename))

end function get_suffix


pure function parent(path)
!! returns parent directory of path
character(*), intent(in) :: path
character(:), allocatable :: parent

character(len_trim(path)) :: work
integer :: i

work = filesep_unix(path)

i = index(work, "/", back=.true.)
parent = work(1:i-1)

end function parent


pure function file_name(path)
!! returns file name without path
character(*), intent(in) :: path
character(:), allocatable :: file_name
character(len_trim(path)) :: work
integer :: i

work = filesep_unix(path)

i = index(work, "/", back=.true.)
file_name = work(i+1:len(work))

end function file_name


subroutine assert_directory_exists(path)
!! throw error if directory does not exist
character(*), intent(in) :: path

if (.not. directory_exists(path)) error stop 'directory does not exist ' // path

end subroutine assert_directory_exists


subroutine assert_file_exists(path)
!! throw error if file does not exist

character(*), intent(in) :: path
logical :: exists

inquire(file=expanduser(path), exist=exists)

if (.not. exists) error stop 'file does not exist ' // path

end subroutine assert_file_exists


pure function filesep_windows(path) result(swapped)
!! '/' => '\' for Windows systems

character(*), intent(in) :: path
character(len_trim(path)) :: swapped
integer :: i

swapped = path
i = index(swapped, '/')
do while (i > 0)
  swapped(i:i) = char(92)
  i = index(swapped, '/')
end do

end function filesep_windows


pure function filesep_unix(path) result(swapped)
!! '\' => '/'

character(*), intent(in) :: path
character(len_trim(path)) :: swapped
integer :: i

swapped = path
i = index(swapped, char(92))
do while (i > 0)
  swapped(i:i) = '/'
  i = index(swapped, char(92))
end do

end function filesep_unix


function expanduser(in) result (out)
!! resolve home directory as Fortran does not understand tilde
!! works for Linux, Mac, Windows, etc.
character(*), intent(in) :: in
character(:), allocatable :: out, homedir

out = filesep_unix(in)

if (len_trim(out) < 1 .or. out(1:1) /= '~') then
  !! nothing to expand
  out = trim(adjustl(out))
  return
endif

homedir = home()
if (len_trim(homedir) == 0) then
  !! could not determine the home directory
  out = trim(adjustl(out))
  return
endif

if (len_trim(out) < 3) then
  !! ~ or ~/
  out = homedir
else
  !! ~/...
  out = homedir // trim(adjustl(out(3:)))
endif

end function expanduser


function home()
!! returns home directory, or empty string if not found
!!
!! https://en.wikipedia.org/wiki/Home_directory#Default_home_directory_per_operating_system

character(:), allocatable :: home
character(256) :: buf
integer :: L, istat

call get_environment_variable("HOME", buf, length=L, status=istat)
if (L==0 .or. istat /= 0) then
  call get_environment_variable("USERPROFILE", buf, length=L, status=istat)
endif

if (L==0 .or. istat /= 0) then
  home = ""
else
  home = trim(buf) // '/'
endif

end function home

end module pathlib
