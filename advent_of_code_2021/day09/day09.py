import numpy as np


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


if __name__ == "__main__":
    test = read_heightmap("test.txt")
    print(part_1(test))
    full = read_heightmap("input.txt")
    print(part_1(full))
