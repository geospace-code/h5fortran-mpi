#!/usr/bin/env python3

from __future__ import annotations
import typing as T
from pprint import pprint
import time
import subprocess
import shutil
import argparse
from pathlib import Path

R = Path(__file__).parent.resolve()

TIMEOUT = 600


def cli() -> dict[str, T.Any]:
    p = argparse.ArgumentParser(description="Benchmark slab")
    p.add_argument(
        "-B",
        "--binary_dir",
        help="Path to the binary directory",
        default=R / "../build",
    )
    p.add_argument(
        "-n", help="total size of slab", type=int, nargs=3, default=[10000, 4, 8]
    )
    p.add_argument("-Nrun", help="number of test runs", type=int, default=10)
    P = p.parse_args()

    bin_dir = Path(P.binary_dir).resolve()
    if not bin_dir.is_dir():
        raise NotADirectoryError(
            f"{bin_dir} is not a directory. Please build HDF5 benchmarks with cmake as per Readme."
        )

    print(f"Slab size: {P.n}")

    params = {
        "bin_dir": bin_dir,
        "Nrun": P.Nrun,
        "lx": P.n,
    }

    return params


def serial_runner(
    exe_name: str, bin_dir: Path, Nrun: int, lx: tuple[int, int, int]
) -> float:
    """
    Serial without MPI
    """

    exe = shutil.which(exe_name, path=bin_dir)
    if not exe:
        raise FileNotFoundError(f"{exe_name} not found in {bin_dir}")
    args = list(map(str, lx)) + ["-o", "out.h5", "-Nrun", str(Nrun)]

    tic = time.monotonic()
    subprocess.check_call([exe] + args, timeout=TIMEOUT)

    return time.monotonic() - tic


def mpi_runner(
    exe_name: str, bin_dir: Path, Nrun: int, lx: tuple[int, int, int]
) -> float:
    """
    Runner frontend uses HWLOC to compute physical core count
    """

    runner_exe = shutil.which("runner", path=bin_dir)
    if not runner_exe:
        raise FileNotFoundError(f"runner not found in {bin_dir}")

    exe = shutil.which(exe_name, path=bin_dir)
    if not exe:
        raise FileNotFoundError(f"{exe_name} not found in {bin_dir}")
    mpiexec = shutil.which("mpiexec")
    if not mpiexec:
        raise FileNotFoundError("mpiexec not found")
    args = list(map(str, lx)) + [
        "-exe",
        exe,
        "-o",
        "out.h5",
        "-mpiexec",
        mpiexec,
        "-Nrun",
        str(Nrun),
    ]

    tic = time.monotonic()
    subprocess.check_call([runner_exe] + args, timeout=TIMEOUT)

    return time.monotonic() - tic


if __name__ == "__main__":
    t = {}
    P = cli()
    # %% Serial (no MPI at all)
    t["serial"] = serial_runner("slab_serial", P["bin_dir"], P["Nrun"], P["lx"])
    # %% MPI transfer to root (inefficient relative to HDF5-MPI)
    t["mpi_root"] = mpi_runner("slab_mpi_serial", P["bin_dir"], P["Nrun"], P["lx"])
    # %% HDF5-MPI layer (most efficient general I/O approach for parallel computation)
    t["mpi_hdf5"] = mpi_runner("slab_mpi", P["bin_dir"], P["Nrun"], P["lx"])

    pprint(t)
