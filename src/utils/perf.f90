module perf

use, intrinsic :: iso_fortran_env, only : int64, stderr=>error_unit, stdout=>output_unit

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

print '(a14,2x,a9,2x,a9)', 'speed (MB/sec)','data (MB)','file (MB)'

bytes = bits/8
data_MB = bytes * real(product(dims) + product(dims(:2))) / 1024 / 1024

t_ms = sysclock2ms(tmin)
data_MBsec = data_MB/(t_ms/1000)

file_MB = file_bytes / 1024 / 1024
file_MBsec = file_MB/(t_ms/1000)
print '(f14.3,1x,f10.3,1x,f10.3)', data_MBsec, data_MB, file_MB


if(data_MBsec < 10) write(stderr,'(a)') "WARNING: write speed seems unusually slow."
if(data_MB < 1) write(stderr, '(a)') "WARNING: benchmark loses accuracy with small files."

end subroutine

end module perf
