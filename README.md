# hdf5-benchmark

benchmarking speed of HDF5 writes from MPI parallel workers

## Build HDF5 Parallel library

Most systems default to the serial HDF5 API, which lacks the HDF5 parallel MPI layer.
The CMakeLists.txt in this project detects this and errors before building.

To build the HDF5 parallel library, regardless of what OS or compiler you're using, use the script from our h5fortran repo:

```sh
git clone https://github.com/geospace-code/h5fortran
cd h5fortran/scripts
cmake -B build -DCMAKE_INSTALL_PREFIX=~/libs_parallel
cmake --build build
```

that will build and install HDF5 under ~/lib_parallel (or other directory of your choice).

## Build HDF5 benchmarks (this project)

Once you have a HDF5 parallel library, build this project like:

```sh
cmake -B build -DHDF5_ROOT=~/libs_parallel
cmake --build build
```

TODO: add benchmark executables
