module perf

use, intrinsic :: iso_fortran_env, only : int64, stderr=>error_unit, stdout=>output_unit

implicit none (type, external)

contains

impure elemental real function sysclock2ms(t)
!! Convert a number of clock ticks, as returned by system_clock called
!! with integer(int64) arguments, to milliseconds

integer(int64), intent(in) :: t
integer(int64) :: rate
real ::  r

call system_clock(count_rate=rate)
r = 1000. / rate
sysclock2ms = t * r

end function sysclock2ms


subroutine print_timing(bits, dims, t_elapsed, file_bytes)
!! print summary of results
integer, intent(in) :: bits, dims(:)
integer(int64), intent(in) :: t_elapsed(:)
real, intent(in) :: file_bytes

integer :: bytes,N
real :: data_MB, file_MB, mean_MBsec, var_MBsec, std_MBsec, file_MBsec
real, dimension(size(t_elapsed)) :: t_ms, data_MBsec
real :: t_ms_mean

N = size(t_ms)

t_ms = sysclock2ms(t_elapsed)

print '(a19,2x,a23,2x,a9,2x,a9)', 'mean write (MB/sec)', 'stdev write (MB/sec)', 'data (MB)','file (MB)'

bytes = bits/8
data_MB = bytes * real(product(dims) + product(dims(:2))) / 1024 / 1024

data_MBsec = data_MB/(t_ms/1000)

mean_MBsec = sum(data_MBsec) / N
var_MBsec = sum((data_MBsec - mean_MBsec)**2) / (N - 1)
std_MBsec = sqrt(var_MBsec)

file_MB = file_bytes / 1024 / 1024
file_MBsec = file_MB/(t_ms_mean/1000)
print '(f19.3,1x,f23.3,1x,f10.3,1x,f10.3)', mean_MBsec, std_MBsec, data_MB, file_MB

print *, "TRACE: t_ms", t_ms, data_MBsec

if(mean_MBsec < 10) write(stderr,'(a)') "WARNING: write speed seems unusually slow."
if(data_MB < 1) write(stderr, '(a)') "WARNING: benchmark loses accuracy with small files."

end subroutine

end module perf
