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


subroutine print_timing(bits, dims, tmin)
integer, intent(in) :: bits, dims(:)
integer(int64), intent(in) :: tmin

real(real64) :: file_Mbytes, t_ms, Mbytes_sec

file_Mbytes = real((bits / 8 * product(dims)) + &
                  (bits / 8 * product(dims(:2))), real64) / 1024 / 1024

print '(A,F8.1)', "data size: (megabytes)", file_Mbytes

t_ms = sysclock2ms(tmin)
Mbytes_sec = file_Mbytes/(t_ms/1000)

print "(A,F10.1,A,F10.1,A)", "time =", t_ms, " ms/run ", Mbytes_sec, " Mbytes/sec"

if(Mbytes_sec < 10) write(stderr,'(A)') "WARNING: write speed seems unusally slow"
if(file_Mbytes < 1) write(stderr, '(A)') "WARNING: benchmark may lose accuracy with small files in general"

end subroutine

end module perf
