from __future__ import annotations
from pathlib import Path
import h5py

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
    ax.set_ylabel("Wallclock Time (milliseconds)")
    ax.legend(loc="best")

    return fig, ax


def get_bench(
    lx: tuple[int, int, int], data_dir: Path, tests: list[str], comp_lvls: list[int]
) -> dict[str, pd.DataFrame]:
    """get writing benchmark HDF5 files to pandas dataframe"""

    data = {
        "write_dr": pd.DataFrame(index=comp_lvls, columns=tests),
        "read_dr": pd.DataFrame(index=comp_lvls, columns=tests),
        "write_t": pd.DataFrame(index=comp_lvls, columns=tests),
        "read_t": pd.DataFrame(index=comp_lvls, columns=tests),
    }

    hdf5_vers = None
    mpi_api = None

    for t in tests:
        for c in comp_lvls:
            tail = f"{lx[0]}_{lx[1]}_{lx[2]}_comp{c}"

            h5fn = data_dir / f"{t}_{tail}.h5.write_stat.h5"
            try:
                with h5py.File(h5fn, "r") as f:
                    ca = f["/comp_lvl"][()]
                    data["write_dr"][t][ca] = f["/median_MBsec"][()]
                    data["write_t"][t][ca] = np.median(f["/t_ms"][:])
            except FileNotFoundError:
                print(f"ERROR: {t} comp_lvl {c}: write benchmark {h5fn}")

            h5fn = data_dir / f"{t}_{tail}.h5.read_stat.h5"
            try:
                with h5py.File(h5fn, "r") as f:
                    ca = f["/comp_lvl"][()]
                    data["read_dr"][t][ca] = f["/median_MBsec"][()]
                    data["read_t"][t][ca] = np.median(f["/t_ms"][:])

                    if "Ncpu" not in data:
                        data["Ncpu"] = f["/Ncpu"][()]
                        data["compiler"] = f["/compiler"].asstr()[()]
                        data["os"] = f["/os"].asstr()[()]
                        hdf5_vers = f["/hdf5version"][:]
                        mpi_api = f["/mpi_api_version"][:]
                        data["mpi_lib_version"] = f["/mpi_lib_version"].asstr()[()]
            except FileNotFoundError:
                print(f"ERROR: {t} comp_lvl {c}: read benchmark {h5fn}")

    if hdf5_vers is not None:
        data["hdf5version"] = f"{hdf5_vers[0]}.{hdf5_vers[1]}.{hdf5_vers[2]}"
        data["mpi_api_version"] = f"{mpi_api[0]}.{mpi_api[1]}"

    return data


if __name__ == "__main__":

    P = cli()

    data = get_bench(P["lx"], P["data_dir"], P["tests"], P["comp"])

    tail = f"{P['lx'][0]}_{P['lx'][1]}_{P['lx'][2]}"
    ttxt = (
        f"size: {P['lx']}  Ncpu: {data['Ncpu']}\n{data['compiler']} {data['os']}\n"
        f"HDF5 {data['hdf5version']}  MPI-{data['mpi_api_version']}  {data['mpi_lib_version']}"
    )

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
