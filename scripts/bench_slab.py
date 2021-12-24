#!/usr/bin/env python3
"""
3D array float write benchmark

Note: A reasonable minimum file size is necessary to get useful results, say
50-100 MB or more file size.
Too-small files bias results unfairly against fixed MPI overhead, whereas real-world
MPI problems of interest are likely to be larger.
"""

from __future__ import annotations
import subprocess
import shutil
from pathlib import Path

from utils import cli


TIMEOUT = 600


def serial_runner(
    exe_name: str,
    bin_dir: Path,
    outfn: Path,
    Nrun: int,
    lx: tuple[int, int, int] = None,
    comp_lvl: int = None,
    np: int = None,
):
    """
    Serial without MPI
    """

    exe = shutil.which(exe_name, path=bin_dir)
    if not exe:
        raise FileNotFoundError(f"{exe_name} not found in {bin_dir}")

    args = [
        "-o",
        str(outfn),
        "-Nrun",
        str(Nrun),
    ]

    if lx:
        args += ["-lx"] + list(map(str, lx))
    if comp_lvl:
        args += ["-comp", str(comp_lvl)]
    if np:
        args += ["-np", str(np)]

    cmd = [exe] + args
    print(" ".join(cmd))

    subprocess.check_call(cmd, timeout=TIMEOUT)


def mpi_runner(
    exe_name: str,
    bin_dir: Path,
    outfn: Path,
    Nrun: int,
    lx: tuple[int, int, int] = None,
    comp_lvl: int = None,
    np: int = None,
):
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
    args = [
        "-exe",
        exe,
        "-o",
        str(outfn),
        "-mpiexec",
        mpiexec,
        "-Nrun",
        str(Nrun),
    ]
    if lx:
        args += ["-lx"] + list(map(str, lx))
    if np:
        args += ["-np", str(np)]
    if comp_lvl:
        args += ["-comp", str(comp_lvl)]

    subprocess.check_call([runner_exe] + args, timeout=TIMEOUT)


def slab_bench(
    tests: list[str],
    comp_lvls: list[int],
    keep: bool,
    lx: tuple[int, int, int],
    Nrun: int,
    np: int,
    bin_dir: Path,
    data_dir: Path,
):

    for c in comp_lvls:
        tail = f"{lx[0]}_{lx[1]}_{lx[2]}_comp{c}"

        if "serial" in tests:
            # Serial (no MPI at all)
            serialfn = data_dir / f"serial_{tail}.h5"

            serial_runner(
                "slab_serial_write",
                bin_dir,
                serialfn,
                Nrun,
                lx,
                comp_lvl=c,
                np=np,
            )

            serial_runner(
                "slab_serial_read",
                bin_dir,
                serialfn,
                Nrun,
                np=np,
            )

            if not keep:
                serialfn.unlink()

        if "mpi_root" in tests:
            # MPI transfer to root (inefficient relative to HDF5-MPI)
            mpirootfn = data_dir / f"mpi_root_{tail}.h5"

            mpi_runner(
                "slab_mpi_serial_write",
                bin_dir,
                mpirootfn,
                Nrun,
                lx,
                comp_lvl=c,
                np=np,
            )

            mpi_runner(
                "slab_mpi_serial_read",
                bin_dir,
                mpirootfn,
                Nrun,
                lx,
                np=np,
            )

            if not keep:
                mpirootfn.unlink()

        if "mpi_hdf5" in tests:
            # HDF5-MPI layer (most efficient general I/O approach for parallel computation)
            mpih5fn = data_dir / f"mpi_hdf5_{tail}.h5"

            mpi_runner(
                "slab_mpi_write",
                bin_dir,
                mpih5fn,
                Nrun,
                lx,
                comp_lvl=c,
                np=np,
            )

            mpi_runner(
                "slab_mpi_read",
                bin_dir,
                mpih5fn,
                Nrun,
                lx,
                np=np,
            )

            if not keep:
                mpih5fn.unlink()


if __name__ == "__main__":

    P = cli()

    slab_bench(
        P["tests"], P["comp"], P["keep"], P["lx"], P["Nrun"], P["np"], P["bin_dir"], P["data_dir"]
    )
