# h5fortran-MPI API

This document provides a listing of h5fortran-mpi `public` scoped user-facing procedures and methods with a summary of their parameters.

All examples assume:

```fortran
use h5mpi, only: hdf5_file, HSIZE_T, HID_T

type(hdf5_file) :: h
```

Query HDF5 library version:

```fortran
use h5mpi, only : hdf5version

print *, hdf5version()
```

## Open / close HDF5 file reference

More than one HDF5 file can be open in a program, by declaring unique file handle (variable) like:

```fortran
type(hdf5_file) :: h1, h2, h3
```

```fortran
call h%open(filename, action, mpi, comp_lvl)
!! Opens hdf5 file

character(*), intent(in) :: filename
character(*), intent(in), optional :: action  !< 'r', 'r+', 'w', 'rw'  (default 'r')
logical, intent(in) :: mpi  !< .true.: use HDF5-MPI   .false.: use serial HDF5
integer, intent(in), optional      :: comp_lvl  !< 0: no compression. 1-9: ZLIB compression, higher is more compressior
```

```fortran
call h%close(close_hdf5_interface)
!! This must be called on each open file to flush buffers to disk
!! data loss can occur if program terminates before this procedure
!!
!! close_hdf5_interface is when you know you have exactly one HDF5 file in your
!! application, if true it closes ALL files, even those invoked directly from HDF5.

logical, intent(in), optional :: close_hdf5_interface
```

To avoid memory leaks or corrupted files, always "close" files before STOPping the Fortran program.

## Flush data to disk while file is open

```fortran
call h%flush()
```

## Disk variable (dataset) inquiry

To allocate variables before reading data, inquire about dataset characteristics with these procedures.

```fortran
rank = h%ndim(dataset_name)

character(*), intent(in) :: dataset_name
```

Get disk dataset shape (1D vector)

```fortran
call h%shape(dataset_name, dims)

character(*), intent(in) :: dataset_name
integer(HSIZE_T), intent(out), allocatable :: dims(:)
```

Dataset "dname" data class (i.e. integer, float, string, ...)

```fortran
integer :: class
!! H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F
class = h%class(dname)
character(*), intent(in) :: dname
```

Dataset "dname" datatype

```fortran
integer(HID_T) :: dtype
!! H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE
dtype = h%dtype(dname)
character(*), intent(in) :: dname
```

Does dataset "dname" exist in this HDF5 file?

```fortran
tf = h%exist(dname)

character(*), intent(in) :: dname
```

Does attribute "attr" exist for object "obj" in this file?

```fortran
tf = h%exist_attr(obj, attr)

character(*), intent(in) :: obj, attr
```

Is dataset "dname" contiguous on disk?

```fortran
tf = h%is_contig(dname)

character(*), intent(in) :: dname
```

Is dataset compact (< 64K)

```fortran
tf = h%is_compact(dname)
!! is dataset compact layout
character(*), intent(in) :: dname
```

Is dataset chunked?

```fortran
tf = h%is_chunked(dname)
!! is dataset chunked
character(*), intent(in) :: dname
```

Is this an HDF5 file?

```fortran
use h5mpi, only: is_hdf5

tf = is_hdf5('myfile.txt')  !< probably false
tf = is_hdf5('myfile.h5')  !< true if a valid HDF5 file
```

These are more advanced inquiries into the memory layout of the dataset, for advanced users:

```fortran
Layout = h%layout(dname)
!! integer :: H5D_CONTIGUOUS_F, H5D_CHUNKED_F, H5D_VIRTUAL_F, H5D_COMPACT_F
character(*), intent(in) :: dname
```

```fortran
call h%chunks(dname, chunk_size)

character(*), intent(in) :: dname
integer, intent(out) :: chunk_size(:)
```

## create dataset softlink

One of the key features of HDF5 is the ability to create dataset softlinks within an HDF5 file:

```fortran
call h%softlink(target, link)
character(*), intent(in) :: target, &  !< target path to link dataset
                            link  !< soft link path to create
```

## file write operations

Write data from memory to disk HDF5 dataset:
When file has been opened for MPI collective read via: `%open(..., mpi=.true.)` the data is distributed
via MPI to the workers.
If overall dataset dimensions "dset_dims" is present, data is collectively gathered from the workers as per HDF5-MPI docs.
Otherwise, h5fortran-mpi assumes that root has all the data to be written and ignores the workers.

```fortran
call h%write(dname, value, dset_dims, istart, iend, stride, chunk_size, compact)
!! write 0d..7d dataset
character(*), intent(in) :: dname
class(*), intent(in) :: value(..)  !< array to write
integer, intent(in), dimension(rank(value)), optional :: dset_dims
integer, intent(in), optional, dimension(rank(value)) :: istart, iend, stride !< array slicing for hyperslab
integer, intent(in), optional :: chunk_size(rank(value))  !< override auto-chunking
logical, intent(in), optional :: compact  !< faster I/O for sub-64 kB datasets
```

Write dataset attribute (e.g. units or instrument)

```fortran
call h%writeattr(dname, attr, attrval)

character(*), intent(in) :: dname, attr  !< dataset name, attribute name
class(*), intent(in) :: attrval(:)  !< character, real, integer
```

## file read operations

Read data from disk to memory:
When file has been opened for MPI collective read via: `%open(..., mpi=.true.)` the data is distributed
via MPI to the workers.
For example, if no slicing is specified, the whole dataset is read by root and broadcast to the workers.
If slicing is specified, the data is read and distributed among the workers as per HDF5-MPI docs.

```fortran
call h%read(dname, value, istart, iend)
character(*), intent(in)         :: dname
class(*), intent(inout) :: value(..)  !< read array to this ALLOCATED variable of rank 0d..7d
integer, intent(in), optional, dimension(rank(value)) :: istart, iend !< array slicing
```

Read dataset attribute into memory:

```fortran
call h%readattr(dname, attr, attrval)
character(*), intent(in) :: dname, attr  !< dataset name, attribute name
class(*), intent(inout) :: attrval(:)  !< character scalar; real vector, integer vector
```

## delete attribute

```fortran
call h%delete_attr(dname, attr)
```
