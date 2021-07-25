module perf

use, intrinsic :: iso_fortran_env, only : int64, stderr=>error_unit

implicit none (type, external)

contains

real function sysclock2ms(t)
!! Convert a number of clock ticks, as returned by system_clock called
!! with integer(int64) arguments, to milliseconds

integer(int64), intent(in) :: t
integer(int64) :: rate
real ::  r

call system_clock(count_rate=rate)
r = 1000. / rate
sysclock2ms = t * r

end function sysclock2ms


subroutine print_timing(bits, dims, tmin, file_bytes)
integer, intent(in) :: bits, dims(:)
integer(int64), intent(in) :: tmin
real, intent(in) :: file_bytes

integer :: bytes
real :: data_MB, file_MB, t_ms, data_MBsec, file_MBsec

bytes = bits/8

data_MB = bytes * real(product(dims) + product(dims(:2))) / 1024 / 1024
file_MB = file_bytes / 1024 / 1024

print '(A,F8.1)', "data size: (megabytes)", data_MB
print '(A,F8.1)', "file size: (megabytes)", file_MB

t_ms = sysclock2ms(tmin)
data_MBsec = data_MB/(t_ms/1000)
file_MBsec = file_MB/(t_ms/1000)

print "(A,F10.1,A,F10.1,a,F10.1,a)", "time =", t_ms, " ms/run ", data_MBsec, " data MB/sec. ", file_MBsec, " data MB/sec."

if(file_MBsec < 10) write(stderr,'(A)') "WARNING: write speed seems unusally slow"
if(file_MB < 1) write(stderr, '(A)') "WARNING: benchmark may lose accuracy with small files in general"

end subroutine

end module perf
