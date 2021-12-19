from pathlib import Path
import h5py
import shutil
import subprocess
import platform

import pandas as pd
from matplotlib.figure import Figure

from utils import cpu_count, cli


def plot_datarate(dr: pd.DataFrame):
    """plot benchmarks"""

    fig = Figure()
    ax = fig.gca()

    for c in dr.columns:
        dr[c].plot(ax=ax, label=c)

    ax.set_xlabel("Compression Level")
    ax.set_ylabel("data bandwidth (MB/second)")
    ax.legend(loc="best")

    return fig, ax


def plot_time(t: pd.DataFrame):
    """plot benchmarks"""

    fig = Figure()
    ax = fig.gca()

    for c in t.columns:
        t[c].plot(ax=ax, label=c)

    ax.set_xlabel("Compression Level")
    ax.set_ylabel("Wallclock Time (seconds)")
    ax.legend(loc="best")

    return fig, ax


def title_meta(lx: tuple[int, int, int], bin_dir: Path) -> str:
    """system metadata for plot title"""

    runner_exe = shutil.which("runner", path=bin_dir)
    if not runner_exe:
        raise FileNotFoundError(f"runner not found in {bin_dir}")
    compiler = subprocess.check_output([runner_exe, "-compiler"], text=True).strip()

    Ncpu = cpu_count(P["bin_dir"], P["lx"])

    if platform.system() == "Linux":
        os = platform.system()
    elif platform.system() == "Darwin":
        os = platform.platform(terse=True)
    else:
        os = platform.platform(terse=True)

    ttxt = f"size: {lx}  Ncpu: {Ncpu}\n{compiler} {os}"

    return ttxt


def read_bench(lx: tuple[int, int, int], data_dir: Path) -> pd.DataFrame:
    """read benchmark HDF5 files to pandas dataframe"""

    datarate = pd.DataFrame(index=[0, 1, 3, 5, 7, 9], columns=["serial", "mpi_root", "mpi_hdf5"])

    for c in datarate.index:
        tail = f"{P['lx'][0]}_{P['lx'][1]}_{P['lx'][2]}_comp{c}"
        serialfn = data_dir / f"serial_{tail}.h5.stat.h5"
        with h5py.File(serialfn, "r") as f:
            datarate["serial"][c] = f["/median_MBsec"][()]

        mpirootfn = data_dir / f"mpi_root_{tail}.h5.stat.h5"
        with h5py.File(mpirootfn, "r") as f:
            datarate["mpi_root"][c] = f["/median_MBsec"][()]

        mpih5fn = data_dir / f"mpi_hdf5_{tail}.h5.stat.h5"
        with h5py.File(mpih5fn, "r") as f:
            datarate["mpi_hdf5"][c] = f["/median_MBsec"][()]

    return datarate


if __name__ == "__main__":

    P = cli()

    datarate = read_bench(P["lx"], P["data_dir"])

    fig, ax = plot_datarate(datarate)

    ttxt = title_meta(P["lx"], P["bin_dir"])
    ax.set_title(f"Slab Benchmark: {ttxt}")
    plotfn = P["data_dir"] / f"datarate_{P['lx'][0]}_{P['lx'][1]}_{P['lx'][2]}.png"
    fig.savefig(plotfn, dpi=150)
