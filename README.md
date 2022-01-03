# hdf5-benchmark

[![ci](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci.yml/badge.svg)](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci.yml)
[![ci_macos](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci_macos.yml/badge.svg)](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci_macos.yml)
[![intel-oneapi](https://github.com/scivision/hdf5-benchmark/actions/workflows/intel-oneapi.yml/badge.svg)](https://github.com/scivision/hdf5-benchmark/actions/workflows/intel-oneapi.yml)

benchmarking speed of HDF5 writes from MPI parallel workers.
Once the program is built as in the following sections, run benchmarks in the [scripts](./scripts) directory like:

```sh
python bench_slab.py
```

Many computer systems default to the serial HDF5 API, which lacks the HDF5 parallel MPI layer.
The CMakeLists.txt in this project detects this and **automatically builds HDF5-MPI if needed**.
The system must have a working MPI library installed already (e.g. OpenMPI, MPICH, Intel MPI, MS-MPI).

Some OS have an installable parallel HDF5 package:

* Ubuntu: `apt install libhdf5-mpi-dev`
* CentOS: `yum install hdf5-openmpi-devel`
* MacOS Homebrew: `brew install hdf5-mpi`
* MacOS MacPorts: `port install hdf5 +fortran +mpich`

While HDF5 1.10.2 is the oldest working HDF5 version, and the CI includes HDF5 1.10.4, in general for bugfixes and performance HDF5 >= 1.10.5 is [recommended](https://portal.hdfgroup.org/display/knowledge/OpenMPI+Build+Issues).

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

## Notes

To build and install the HDF5 parallel library use the script from our h5fortran repo:

```sh
git clone https://github.com/geospace-code/h5fortran
cd h5fortran/scripts

cmake -B build --install-prefix=$HOME/lib_par -Dhdf5_parallel=on

cmake --build build
```

that will build and install HDF5 under ~/lib_par (or other directory of your choice).
