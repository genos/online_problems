from functools import partial
from pathlib import Path

import numpy as np
import numpy.typing as npt
from scipy.ndimage import generic_filter

Algorithm = npt.NDArray[np.bool_]
Image = npt.NDArray[np.bool_]


def read(p: Path) -> tuple[Algorithm, Image]:
    a, i = p.read_text().split("\n\n")
    algorithm = np.fromiter((c == "#" for c in a.replace("\n", "")), bool, 512)
    image = np.array([[c == "#" for c in line] for line in i.split("\n")])
    return algorithm, image


def lookup(algorithm: Algorithm, three_by_three: Image) -> int:
    return algorithm[int(three_by_three @ (2 ** np.arange(9))[::-1])]


def solve(n: int, algorithm: Algorithm, image: Image) -> int:
    i = np.pad(image.copy(), 1, mode="constant", constant_values=False)
    for _ in range(n):
        i = generic_filter(
            np.pad(i, 1, "edge"),
            partial(lookup, algorithm),
            size=(3, 3),
            mode="nearest",
        )
    return np.count_nonzero(i)


if __name__ == "__main__":
    for name in ["test", "input"]:
        print(name)
        a, i = read(Path(f"{name}.txt"))
        print(f"Part 1: {solve(2, a, i)}")
        print(f"Part 2: {solve(50, a, i)}")
