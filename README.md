# h5fortran-mpi

[![DOI](https://zenodo.org/badge/377901005.svg)](https://zenodo.org/badge/latestdoi/377901005)

[![ci](https://github.com/geospace-code/h5fortran-mpi/actions/workflows/ci.yml/badge.svg)](https://github.com/geospace-code/h5fortran-mpi/actions/workflows/ci.yml)
[![ci](https://github.com/geospace-code/h5fortran-mpi/actions/workflows/ci_build.yml/badge.svg)](https://github.com/geospace-code/h5fortran-mpi/actions/workflows/ci_build.yml)
[![intel-oneapi](https://github.com/geospace-code/h5fortran-mpi/actions/workflows/intel-oneapi.yml/badge.svg)](https://github.com/geospace-code/h5fortran-mpi/actions/workflows/intel-oneapi.yml)

Easy to use object-oriented Fortran [parallel HDF5-MPI](https://portal.hdfgroup.org/display/HDF5/Parallel+HDF5) interface.
The [h5fortran-mpi API](./API.md) can be used with or with MPI.
A very similar NetCDF4 interface is
[nc4fortran](https://github.com/geospace-code/nc4fortran).

Many computer systems default to the serial HDF5 API, which lacks the HDF5 parallel MPI layer.
The scripts/CMakeLists.txt can build the HDF5-MPI stack if needed.
To use HDF5-MPI features, the computer must have a working MPI library installed already (e.g. OpenMPI, MPICH, Intel MPI, MS-MPI).

Some OS have an installable parallel HDF5 package:

* Ubuntu: `apt install libhdf5-mpi-dev`
* CentOS: `yum install hdf5-openmpi-devel`
* MacOS Homebrew: `brew install hdf5-mpi`
* MacOS MacPorts: `port install hdf5 +fortran +mpich`

While HDF5 1.10.2 is the oldest working HDF5 version, in general for bugfixes and performance HDF5 &ge; 1.10.5 is [recommended](https://portal.hdfgroup.org/display/knowledge/OpenMPI+Build+Issues).
For highest performance with parallel compressed writes consider HDF5 &ge; 1.12.2.

## Compressed parallel HDF5

Compression is useful in general to save significant disk space and speed up write/read.
HDF5-MPI file compression requires HDF5 >= 1.10.2 and MPI-3.
As noted above, HDF5 >= 1.10.5 is recommended for stability and performance.

### Windows limitations

Microsoft Windows does not currently support native HDF5 parallel file compression.
Windows Subsystem for Linux can be used for HDF5 parallel file compression.
Native Windows users can read HDF5 compressed files but without using MPI.

Native Windows MPI options are currently limited to MS-MPI and Intel MPI.
Currently Windows MS-MPI is MPI-2.
A quirk with Intel oneAPI on Windows despite having MPI-3 is that with HDF5 1.10.x and at least through HDF5 1.12.1 the collective filtered parallel compression file I/O does not work.
We test for this in CMake and set the compile flags appropriately.

Windows users that need file compression may use Windows Subsystem for Linux (e.g. Ubuntu) and install `libhdf5-mpi-dev`.

## Build this project

Build this project like:

```sh
cmake -B build
cmake --build build
```

If you have previously built / installed a parallel HDF5 library, refer to it (saving build time) like:

```sh
cmake -B build -DHDF5_ROOT=~/lib_par
cmake --build build
```

To build without MPI (serial HDF5 file operations only):

```sh
cmake -B build -Dhdf5_parallel=off
```

Cray computers may use the CMake toolchain file to work with Intel or GCC backend.

```sh
cmake --toolchain cray.cmake -B build
```

---

Fortran Package Manager (FPM) users build like:

```sh
fpm build --flag -Dh5fortran_HAVE_PARALLEL
# omitting this flag builds the serial API only

fpm test
```

## Notes

To build and install the HDF5 parallel library use the script:

```sh
cmake -B build_hdf5 -S scripts --install-prefix=$HOME/lib_par

cmake --build build_hdf5
```

that will build and install HDF5 under ~/lib_par (or other directory of your choice).
