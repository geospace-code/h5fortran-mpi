#!/usr/bin/env python3

import time
import subprocess
import shutil
import argparse
from pathlib import Path

R = Path(__file__).parent.resolve()

p = argparse.ArgumentParser(description="Benchmark slab")
p.add_argument(
    "-B", "--binary_dir", help="Path to the binary directory", default=R / "../build"
)
p.add_argument(
    "-n", help="total size of slab", type=int, nargs=3, default=[10000, 4, 8]
)
p.add_argument("-Nrun", help="number of test runs", type=int, default=10)
P = p.parse_args()

bin_dir = P.binary_dir
if not bin_dir.is_dir():
    raise NotADirectoryError(
        f"{bin_dir} is not a directory. Please build HDF5 benchmarks with cmake as per Readme."
    )

mpiexec = shutil.which("mpiexec")
if not mpiexec:
    raise FileNotFoundError("mpiexec not found")

Nrun_opt = ["-Nrun", str(P.Nrun)]

print(f"Slab size: {P.n}")
# %% Serial (no MPI at all)
serial_exe = shutil.which("slab_serial", path=bin_dir)
if not serial_exe:
    raise FileNotFoundError(f"slab_serial not found in {bin_dir}")
args = list(map(str, P.n)) + ["-o", "out.h5"] + Nrun_opt

tic = time.monotonic()
subprocess.check_call([serial_exe] + args, timeout=600)
toc = time.monotonic() - tic
print(f"serial slab: {toc:.3f} seconds")
# %% Runner frontend uses HWLOC to compute physical core count
runner_exe = shutil.which("runner", path=bin_dir)
if not runner_exe:
    raise FileNotFoundError(f"runner not found in {bin_dir}")
# %% MPI transfer to root (inefficient relative to HDF5-MPI)
mpi_serial_exe = shutil.which("slab_mpi_serial", path=bin_dir)
if not mpi_serial_exe:
    raise FileNotFoundError(f"slab_mpi_serial not found in {bin_dir}")
args = list(map(str, P.n)) + ["-exe", mpi_serial_exe, "-o", "out.h5"] + Nrun_opt

tic = time.monotonic()
subprocess.check_call([runner_exe] + args, timeout=600)
toc = time.monotonic() - tic
print(f"MPI transfer slab: {toc:.3f} seconds")
# %% HDF5-MPI layer (most efficient general I/O approach for parallel computation)
hdf5_mpi_exe = shutil.which("slab_mpi", path=bin_dir)
if not hdf5_mpi_exe:
    raise FileNotFoundError(f"slab_mpi not found in {bin_dir}")
args = list(map(str, P.n)) + ["-exe", hdf5_mpi_exe, "-o", "out.h5"] + Nrun_opt
tic = time.monotonic()
subprocess.check_call([runner_exe] + args, timeout=600)
toc = time.monotonic() - tic
print(f"HDF5-MPI slab: {toc:.3f} seconds")
