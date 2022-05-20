module median_mod
!! based on comp.lang.fortran 2019-02-25T5:43:31 Thomas Koening
!! https://groups.google.com/g/comp.lang.fortran/c/NhkygjteSS0/m/0GzWn6qlBwAJ
!! uses C standard library quicksort Qsort

use, intrinsic :: iso_c_binding

implicit none (type, external)

private
public :: median, qsort


interface
subroutine qsort_c(A, elem_count, elem_size, compare) bind(C, name="qsort")
import c_ptr, c_size_t, c_funptr
type(c_ptr), value :: A
integer(c_size_t), value :: elem_count
integer(c_size_t), value :: elem_size
type(c_funptr), value :: compare
end subroutine qsort_c
end interface


interface qsort
  procedure qsort_r32, qsort_r64
end interface qsort


contains


subroutine qsort_r32(a)
real(c_float), intent(inout), target :: a(:)
call qsort_c(c_loc(a(1)), size(A, kind=c_size_t), c_sizeof(a(1)), c_funloc(compare_r32))
end subroutine qsort_r32

subroutine qsort_r64(a)
real(c_double), intent(inout), target :: a(:)
call qsort_c(c_loc(a(1)), size(A, kind=c_size_t), c_sizeof(a(1)), c_funloc(compare_r64))
end subroutine qsort_r64


pure integer(c_int) function compare_r32 (a,b) bind(C)
real(c_float), intent(in) :: a, b

if (a > b) then
compare_r32 = 1
else if (a == b) then
compare_r32 = 0
else
compare_r32 = -1
end if

end function compare_r32


pure integer(c_int) function compare_r64 (a,b) bind(C)
real(c_double), intent(in) :: a, b

if (a > b) then
compare_r64 = 1
else if (a == b) then
compare_r64 = 0
else
compare_r64 = -1
end if

end function compare_r64


real function median(A)
real, intent(in) :: A(:)

integer :: L
real :: W(size(A))

W = A
call Qsort(W)

L = size(A)
if ( mod(L, 2) == 0 ) then
  median = (W(L / 2 + 1) + W(L / 2)) / 2
else
  median = W(L / 2 + 1)
end if

end function median

end module median_mod
