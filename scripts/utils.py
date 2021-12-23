from __future__ import annotations
import typing as T
import argparse
from pathlib import Path

R = Path(__file__).parent.resolve()


def cli() -> dict[str, T.Any]:
    p = argparse.ArgumentParser(description="Benchmark slab")
    p.add_argument(
        "-B",
        "--binary_dir",
        help="Path to the binary directory",
        default=R / "../build",
    )
    p.add_argument("-o", "--datadir", help="data directory", default=R / "data")
    p.add_argument("-n", help="total size of slab", type=int, nargs=3, default=[8192, 32, 64])
    p.add_argument("-Nrun", help="number of test runs", type=int, default=5)
    p.add_argument("-np", help="number of MPI processes", type=int)
    p.add_argument("-k", "--keep", help="keep output files", action="store_true")
    p.add_argument(
        "-t", "--tests", help="test names", nargs="+", default=["serial", "mpi_root", "mpi_hdf5"]
    )
    P = p.parse_args()

    bin_dir = Path(P.binary_dir).resolve()
    if not bin_dir.is_dir():
        raise NotADirectoryError(
            f"{bin_dir} is not a directory. Please build HDF5 benchmarks with CMake as per Readme."
        )

    data_dir = Path(P.datadir).resolve()
    data_dir.mkdir(parents=True, exist_ok=True)

    print(f"Slab size: {P.n}")

    params = {
        "bin_dir": bin_dir,
        "data_dir": data_dir,
        "Nrun": P.Nrun,
        "lx": P.n,
        "np": P.np,
        "keep": P.keep,
        "tests": P.tests,
    }

    return params
