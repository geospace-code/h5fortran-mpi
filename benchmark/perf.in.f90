module perf

use, intrinsic :: iso_fortran_env, only : int64, stderr=>error_unit, stdout=>output_unit, compiler_version
use mpi, only : mpi_get_version, MPI_Get_library_version

use h5fortran, only : hdf5_file, hdf5version, HSIZE_T

use median_mod, only : median

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


subroutine print_timing(Ncpu, comp_lvl, bits, dims, t_elapsed, file_bytes, debug, statfn)
!! print summary of results
integer, intent(in) :: Ncpu, comp_lvl, bits, dims(:)
integer(int64), intent(in) :: t_elapsed(:)
integer(HSIZE_T), intent(in) :: file_bytes
logical, intent(in) :: debug
character(*), intent(in), optional :: statfn

integer :: bytes, N, ierr, mpi_api_version(2), L
character(256) :: mpi_lib_version
real :: mean_MBsec, var_MBsec, std_MBsec, median_MBsec, file_MB, data_MB
real, dimension(size(t_elapsed)) :: t_ms, data_MBsec

type(hdf5_file) :: f

N = size(t_ms)

t_ms = sysclock2ms(t_elapsed)

print '(a15,1x,a15,2x,a9,2x,a9)', 'median (MB/sec)', 'stdev (MB/sec)', 'data (MB)','file (MB)'

bytes = bits/8
data_MB = real(bytes) / 1024 / 1024 * (product(dims) + product(dims(:2)))

data_MBsec = data_MB / (t_ms/1000)

mean_MBsec = sum(data_MBsec) / N
if (N > 1) then
  var_MBsec = sum((data_MBsec - mean_MBsec)**2) / (N - 1)
else
  var_MBsec = 0
endif
std_MBsec = sqrt(var_MBsec)
median_MBsec = median(data_MBsec)

file_MB = real(file_bytes) / 1024 / 1024

print '(f15.1,1x,f15.1,1x,f9.1,1x,f9.1)', median_MBsec, std_MBsec, data_MB, file_MB

if(.not.present(statfn)) return

call f%open(statfn, action="w", mpi=.false.)

call f%write("/t_ms", t_ms)
call f%write("/MBsec", data_MBsec)
call f%write("/mean_MBsec", mean_MBsec)
call f%write("/stdev_MBsec", std_MBsec)
call f%write("/median_MBsec", median_MBsec)
call f%write("/data_MB", data_MB)
call f%write("/file_MB", file_MB)
call f%write("/compiler", compiler_version())
call f%write("/Ncpu", Ncpu)
call f%write("/comp_lvl", comp_lvl)
call f%write("/os", @runner_os@)
call f%write("/hdf5version", hdf5version())

call mpi_get_version(mpi_api_version(1), mpi_api_version(2), ierr)
if(ierr /= 0) error stop "h5fortran:perf: mpi_get_version failed"
call f%write("/mpi_api_version", mpi_api_version)

call MPI_Get_library_version(mpi_lib_version, L, ierr)
if(ierr /= 0) error stop "h5fortran:perf: MPI_Get_library_version failed"
call f%write("/mpi_lib_version", mpi_lib_version)

call f%close()

print '(a)', "saved stats to " // statfn

if(debug) return

if(data_MB < 10) write(stderr, '(a)') "benchmark inaccurate with small files < 10 MB"
if(median_MBsec > 50000) error stop "unrealistically high median MB/sec > 50,000"
if(median_MBsec < 10) error stop "unusually low median MB/sec < 10"

end subroutine

end module perf
