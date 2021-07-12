module perf

use, intrinsic :: iso_fortran_env, only : real64, int64, stderr=>error_unit

implicit none (type, external)

contains

real(real64) function sysclock2ms(t)
!! Convert a number of clock ticks, as returned by system_clock called
!! with integer(int64) arguments, to milliseconds

integer(int64), intent(in) :: t
integer(int64) :: rate
real(real64) ::  r

call system_clock(count_rate=rate)
r = 1000._real64 / rate
sysclock2ms = t * r

end function sysclock2ms

end module perf
