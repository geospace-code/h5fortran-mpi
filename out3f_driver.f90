program test

use, intrinsic :: iso_fortran_env, only : real64

use original, only : out3f, param
use mpi

implicit none (type, external)

external :: mpi_finalize

integer :: ierr

type(param) :: P

real(real64), allocatable :: q(:,:,:,:)

character(:), allocatable :: fname, dname

call mpi_init(ierr)

P%x1 = 96
P%x2 = 1024
P%x3 = 256
P%Ns = 7

allocate(q(P%x1, P%x2, P%x3, P%Ns))

fname = "out.h5"
dname = "/pid"

call out3f(P, q, fname, dname)

call mpi_finalize(ierr)

end program
