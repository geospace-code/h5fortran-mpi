from pathlib import Path
import h5py
import shutil
import subprocess
import platform

import numpy as np
import pandas as pd
from matplotlib.figure import Figure

from utils import cli


def plot_datarate(dr: pd.DataFrame) -> tuple:
    """plot benchmarks"""

    fig = Figure()
    ax = fig.gca()

    for c in dr.columns:
        dr[c].plot(ax=ax, label=c, marker=".")

    ax.set_xlabel("Compression Level")
    ax.set_ylabel("data bandwidth (MB/second)")
    ax.legend(loc="best")

    return fig, ax


def plot_time(t: pd.DataFrame) -> tuple:
    """plot benchmarks"""

    fig = Figure()
    ax = fig.gca()

    for c in t.columns:
        t[c].plot(ax=ax, label=c, marker=".")

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

    Ncpu = subprocess.check_output(
        [runner_exe, "-lx"] + list(map(str, lx)) + ["-tell_cpu"], text=True
    ).strip()

    if platform.system() == "Linux":
        os = platform.system()
    elif platform.system() == "Darwin":
        os = platform.platform(terse=True)
    else:
        os = platform.platform(terse=True)

    ttxt = f"size: {lx}  Ncpu: {Ncpu}\n{compiler} {os}"

    return ttxt


def get_bench(lx: tuple[int, int, int], data_dir: Path) -> dict[str, pd.DataFrame]:
    """get writing benchmark HDF5 files to pandas dataframe"""

    comp_lvls = [0, 1, 3, 5, 7, 9]
    tests = ["serial", "mpi_root", "mpi_hdf5"]

    data = {
        "write_dr": pd.DataFrame(index=comp_lvls, columns=tests),
        "read_dr": pd.DataFrame(index=comp_lvls, columns=tests),
        "write_t": pd.DataFrame(index=comp_lvls, columns=tests),
        "read_t": pd.DataFrame(index=comp_lvls, columns=tests),
    }

    for t in tests:
        for c in comp_lvls:
            tail = f"{lx[0]}_{lx[1]}_{lx[2]}_comp{c}"

            try:
                with h5py.File(data_dir / f"{t}_{tail}.h5.write_stat.h5", "r") as f:
                    data["write_dr"][t][c] = f["/median_MBsec"][()]
                    data["write_t"][t][c] = np.median(f["/t_ms"][:])
            except FileNotFoundError:
                print(f"ERROR: {t} comp_lvl {c}: write benchmark not found")

            try:
                with h5py.File(data_dir / f"{t}_{tail}.h5.read_stat.h5", "r") as f:
                    data["read_dr"][t][c] = f["/median_MBsec"][()]
                    data["read_t"][t][c] = np.median(f["/t_ms"][:])
            except FileNotFoundError:
                print(f"ERROR: {t} comp_lvl {c}: read benchmark not found")

    return data


if __name__ == "__main__":

    P = cli()
    ttxt = title_meta(P["lx"], P["bin_dir"])

    data = get_bench(P["lx"], P["data_dir"])

    tail = f"{P['lx'][0]}_{P['lx'][1]}_{P['lx'][2]}"

    fig, ax = plot_datarate(data["write_dr"])
    ax.set_title(f"WRITE: Slab Benchmark: {ttxt}")
    plotfn = P["data_dir"] / f"write_datarate_{tail}.png"
    print("making", plotfn)
    fig.savefig(plotfn, dpi=150)

    fig, ax = plot_datarate(data["read_dr"])
    ax.set_title(f"READ: Slab Benchmark: {ttxt}")
    plotfn = P["data_dir"] / f"read_datarate_{tail}.png"
    print("making", plotfn)
    fig.savefig(plotfn, dpi=150)

    fig, ax = plot_time(data["write_t"])
    ax.set_title(f"WRITE: Slab Benchmark: {ttxt}")
    plotfn = P["data_dir"] / f"write_times_{tail}.png"
    print("making", plotfn)
    fig.savefig(plotfn, dpi=150)

    fig, ax = plot_time(data["read_t"])
    ax.set_title(f"READ: Slab Benchmark: {ttxt}")
    plotfn = P["data_dir"] / f"read_times_{tail}.png"
    print("making", plotfn)
    fig.savefig(plotfn, dpi=150)
