import numpy as np
from scipy import ndimage


def read_heightmap(s: str) -> np.ndarray:
    with open(s) as f:
        return np.array([[int(c) for c in line.strip()] for line in f], dtype=float)


def neighbor_shifts(hmap: np.ndarray) -> np.ndarray:
    m, n = hmap.shape
    col = np.atleast_2d(float("inf") + np.zeros(m)).T
    row = np.atleast_2d(float("inf") + np.zeros(n))
    return np.array(
        [
            np.hstack([col, hmap])[:, :-1],
            np.hstack([hmap, col])[:, 1:],
            np.vstack([row, hmap])[:-1, :],
            np.vstack([hmap, row])[1:, :],
        ]
    )


def part_1(hmap: np.ndarray) -> int:
    mask = np.all(hmap < neighbor_shifts(hmap), axis=0)
    return sum(1 + hmap[mask].astype(int))


def part_1_again(hmap: np.ndarray) -> int:
    footprint = ndimage.generate_binary_structure(2, 1)
    min_filter = ndimage.minimum_filter(
        hmap, footprint=footprint, cval=float("inf"), mode="constant"
    )
    mask = (hmap == min_filter) & (hmap != 9)
    return sum(1 + hmap[mask].astype(int))


def part_2(hmap: np.ndarray) -> int:
    labeled, num_labels = ndimage.label(hmap != 9)
    sizes = sorted(np.count_nonzero(labeled == i) for i in range(1, num_labels + 1))
    return np.product(sizes[-3:])


if __name__ == "__main__":
    test = read_heightmap("test.txt")
    print(part_1(test))
    print(part_1_again(test))
    print(part_2(test))
    full = read_heightmap("input.txt")
    print(part_1(full))
    print(part_1_again(full))
    print(part_2(full))
