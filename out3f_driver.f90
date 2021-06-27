program test

use, intrinsic :: iso_fortran_env, only : real64
use original, only : out3f, param

implicit none (type, external)

type(param) :: P

real(real64), allocatable :: q(:,:,:,:)

P%ngrids_out = 1
P%nDim = 3
P%drank = 4

allocate(q(P%meqn, 1-P%mbc:P%maxmx+P%mbc, 1-P%mbc:P%maxmy+P%mbc, 1-P%mbc:P%maxmz+P%mbc))

call out3f(P,q)

end program
