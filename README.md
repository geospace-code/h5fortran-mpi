# hdf5-benchmark

[![ci](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci.yml/badge.svg)](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci.yml)
[![ci_macos](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci_macos.yml/badge.svg)](https://github.com/scivision/hdf5-benchmark/actions/workflows/ci_macos.yml)
[![intel-oneapi](https://github.com/scivision/hdf5-benchmark/actions/workflows/intel-oneapi.yml/badge.svg)](https://github.com/scivision/hdf5-benchmark/actions/workflows/intel-oneapi.yml)

benchmarking speed of HDF5 writes from MPI parallel workers.
Once the program is built as in the following sections, run benchmarks in the [scripts](./scripts) directory like:

```sh
python bench_slab.py
```

Most systems default to the serial HDF5 API, which lacks the HDF5 parallel MPI layer.
The CMakeLists.txt in this project detects this and automatically builds HDF5-MPI if needed.
The system must have a working MPI library installed already.
Some OS have a parallel HDF5 package:

* Ubuntu: `apt install libhdf5-mpi-dev`
* CentOS: `yum install hdf5-openmpi-devel`
* MacOS: `brew install hdf5-mpi`

## Compressed parallel HDF5

Compression is useful in general to save significant disk space and speed up write/read.
HDF5-MPI file compression requires HDF5 >= 1.10.2 and MPI-3.
Currently Windows MS-MPI is MPI-2.
Intel oneAPI, including on Windows has MPI-3.
Thus Windows users can use Intel oneAPI with compression, or GCC+MS-MPI without compression.

Windows users that need file compression may use Windows Subsystem for Linux (e.g. Ubuntu) and install `libhdf5-mpi-dev`.

## Build this project

Once you have a HDF5 parallel library, build this project like:

```sh
cmake -B build -DHDF5_ROOT=~/lib_par
cmake --build build
```

The test program "build/slab" writes a file "out.h5" that contains arrays "A2" and "A3" which are 2D and 3D respectively.
The workers partition the writing by rows.

## Notes

To build and install the HDF5 parallel library use the script from our h5fortran repo:

```sh
git clone https://github.com/geospace-code/h5fortran
cd h5fortran/scripts

cmake -B build -DCMAKE_INSTALL_PREFIX=~/lib_par -Dhdf5_parallel=on

cmake --build build
```

that will build and install HDF5 under ~/lib_par (or other directory of your choice).
