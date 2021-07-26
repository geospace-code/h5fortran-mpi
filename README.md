# hdf5-benchmark

benchmarking speed of HDF5 writes from MPI parallel workers

## Build HDF5 Parallel library

Most systems default to the serial HDF5 API, which lacks the HDF5 parallel MPI layer.
The CMakeLists.txt in this project detects this and errors before building.

To build the HDF5 parallel library, regardless of what OS or compiler you're using, use the script from our h5fortran repo:

```sh
git clone https://github.com/geospace-code/h5fortran
cd h5fortran/scripts
cmake -B build -DCMAKE_INSTALL_PREFIX=~/libs_par
cmake --build build
```

that will build and install HDF5 under ~/lib_par (or other directory of your choice).

---

Alternatively, some Linux distros do have a parallel HDF5 package:

* Ubuntu: `apt install libhdf5-openmpi-dev`
* CentOS: `yum install hdf5-openmpi-devel`

## Compressed parallel HDF5

Compression is useful in general to save significant disk space and speed up write/read, but requires HDF5 >= 1.10.2 and MPI-3.

### Windows

At least through HDF5 1.10.7 and 1.21.1, parallel HDF5 writes *with compression* are not supported on Windows, with GCC or Intel compiler.
This is despite the Intel compiler having MPI-3 API.
This appears to be an undocumented limitation on Windows of HDF5.

Windows users may instead use Windows Subsystem for Linux and install `libhdf5-openmpi-dev` in Ubuntu.
Or simply use uncompressed parallel HDF5, but compression is important enough that WSL may be the best general solution for Windows users.

## Build this project

Once you have a HDF5 parallel library, build this project like:

```sh
cmake -B build -DHDF5_ROOT=~/libs_par
cmake --build build
```

The test program "build/slab" writes a file "out.h5" that contains arrays "A2" and "A3" which are 2D and 3D respectively.
The workers partition the writing by rows.
