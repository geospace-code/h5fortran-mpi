module perf

use, intrinsic :: iso_fortran_env, only : int64, stderr=>error_unit, stdout=>output_unit
use median_mod, only : median

use h5mpi, only : hdf5_file
use hdf5, only : HSIZE_T

implicit none (type, external)

contains

impure elemental real function sysclock2ms(t)
!! Convert a number of clock ticks, as returned by system_clock called
!! with integer(int64) arguments, to milliseconds

integer(int64), intent(in) :: t
integer(int64) :: rate
real ::  r

call system_clock(count_rate=rate)
r = 1000. / real(rate)
sysclock2ms = real(t) * r

end function sysclock2ms


subroutine print_timing(bits, dims, t_elapsed, file_bytes, outfn)
!! print summary of results
integer, intent(in) :: bits, dims(:)
integer(int64), intent(in) :: t_elapsed(:)
real, intent(in) :: file_bytes
character(*), intent(in), optional :: outfn

integer :: bytes,N
real :: data_MB, file_MB, mean_MBsec, var_MBsec, std_MBsec, median_MBsec
real, dimension(size(t_elapsed)) :: t_ms, data_MBsec

type(hdf5_file) :: f

N = size(t_ms)

t_ms = sysclock2ms(t_elapsed)

print '(a15,1x,a15,2x,a9,2x,a9)', 'median (MB/sec)', 'stdev (MB/sec)', 'data (MB)','file (MB)'

bytes = bits/8
data_MB = bytes * real(product(dims) + product(dims(:2))) / 1024 / 1024

data_MBsec = data_MB/(t_ms/1000)

mean_MBsec = sum(data_MBsec) / N
var_MBsec = sum((data_MBsec - mean_MBsec)**2) / (N - 1)
std_MBsec = sqrt(var_MBsec)
median_MBsec = median(data_MBsec)

file_MB = file_bytes / 1024 / 1024
print '(f15.1,1x,f15.3,1x,f9.1,1x,f9.1)', median_MBsec, std_MBsec, data_MB, file_MB

if(mean_MBsec < 10) write(stderr,'(a)') "WARNING: write speed seems unusually slow."
if(data_MB < 5) write(stderr, '(a)') "WARNING: benchmark loses accuracy with small files."

if(.not.present(outfn)) return

call f%open(outfn, action="w", mpi=.false.)

call f%write("t_ms", t_ms, shape(t_ms, HSIZE_T))
call f%write("MBsec", data_MBsec, shape(data_MBsec, HSIZE_T))
call f%write("mean_MBsec", mean_MBsec)
call f%write("var_MBsec", var_MBsec)
call f%write("std_MBsec", std_MBsec)
call f%write("median_MBsec", median_MBsec)
call f%write("data_MB", data_MB)
call f%write("file_MB", file_MB)

call f%close()

end subroutine

end module perf
