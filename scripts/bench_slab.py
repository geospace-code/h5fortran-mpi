#!/usr/bin/env python3
"""
3D array float write benchmark

Note: A reasonable minimum file size is necessary to get useful results, say
50-100 MB or more file size.
Too-small files bias results unfairly against fixed MPI overhead, whereas real-world
MPI problems of interest are likely to be larger.
"""

from __future__ import annotations
import time
import subprocess
import shutil
from pathlib import Path

import pandas as pd

from bench_plot import plot_time
from utils import cpu_count, cli


TIMEOUT = 600


def serial_runner(
    exe_name: str,
    bin_dir: Path,
    Nrun: int,
    lx: tuple[int, int, int],
    comp_lvl: int,
    outfn: Path,
    np: int = None,
) -> float:
    """
    Serial without MPI
    """

    exe = shutil.which(exe_name, path=bin_dir)
    if not exe:
        raise FileNotFoundError(f"{exe_name} not found in {bin_dir}")

    args = (
        ["-lx"]
        + list(map(str, lx))
        + [
            "-o",
            str(outfn),
            "-Nrun",
            str(Nrun),
            "-comp",
            str(comp_lvl),
        ]
    )
    if np:
        args += ["-np", str(np)]

    tic = time.monotonic()
    subprocess.check_call([exe] + args, timeout=TIMEOUT)

    return time.monotonic() - tic


def mpi_runner(
    exe_name: str,
    bin_dir: Path,
    Nrun: int,
    lx: tuple[int, int, int],
    comp_lvl: int,
    outfn: Path,
    np: int = None,
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
    args = (
        ["-lx"]
        + list(map(str, lx))
        + [
            "-exe",
            exe,
            "-o",
            str(outfn),
            "-mpiexec",
            mpiexec,
            "-Nrun",
            str(Nrun),
            "-comp",
            str(comp_lvl),
        ]
    )

    if np:
        args += ["-np", str(np)]

    tic = time.monotonic()
    subprocess.check_call([runner_exe] + args, timeout=TIMEOUT)

    return time.monotonic() - tic


if __name__ == "__main__":

    P = cli()

    t = pd.DataFrame(index=[0, 1, 3, 5, 7, 9], columns=["serial", "mpi_root", "mpi_hdf5"])

    for c in t.index:
        tail = f"{P['lx'][0]}_{P['lx'][1]}_{P['lx'][2]}_comp{c}"
        # %% Serial (no MPI at all)
        serialfn = P["data_dir"] / f"serial_{tail}.h5"
        t["serial"][c] = serial_runner(
            "slab_serial",
            P["bin_dir"],
            P["Nrun"],
            P["lx"],
            outfn=serialfn,
            comp_lvl=c,
            np=P["np"],
        )
        if not P["keep"]:
            serialfn.unlink()

        # %% MPI transfer to root (inefficient relative to HDF5-MPI)
        mpirootfn = P["data_dir"] / f"mpi_root_{tail}.h5"
        t["mpi_root"][c] = mpi_runner(
            "slab_mpi_serial",
            P["bin_dir"],
            P["Nrun"],
            P["lx"],
            outfn=mpirootfn,
            comp_lvl=c,
            np=P["np"],
        )
        if not P["keep"]:
            mpirootfn.unlink()

        # %% HDF5-MPI layer (most efficient general I/O approach for parallel computation)
        mpih5fn = P["data_dir"] / f"mpi_hdf5_{tail}.h5"
        t["mpi_hdf5"][c] = mpi_runner(
            "slab_mpi",
            P["bin_dir"],
            P["Nrun"],
            P["lx"],
            outfn=mpih5fn,
            comp_lvl=c,
            np=P["np"],
        )
        if not P["keep"]:
            mpih5fn.unlink()

    runner_exe = shutil.which("runner", path=P["bin_dir"])
    compiler = subprocess.check_output([runner_exe, "-compiler"], text=True)

    fig, ax = plot_time(t)
    Ncpu = cpu_count(P["bin_dir"], P["lx"])
    ax.set_title(f"Slab Benchmark: size: {P['lx']}  Ncpu: {Ncpu}\n{compiler}")
    fig.savefig("slab_time.png", dpi=150)
