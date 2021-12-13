import itertools as it
from typing import Iterator, Tuple

import numpy as np
import numpy.typing as npt

Grid = npt.NDArray[np.int_]
Mask = npt.NDArray[np.bool_]


def read_grid(s: str) -> Grid:
    with open(s) as f:
        return np.array([[int(c) for c in line.strip()] for line in f])


def neighbors(i: int, j: int) -> Iterator[Tuple[int, int]]:
    for di, dj in it.product([-1, 0, 1], repeat=2):
        if (di, dj) != (0, 0) and 0 <= i + di < 10 and 0 <= j + dj < 10:
            yield i + di, j + dj


def _flash_step(grid: Grid, mask: Mask) -> Tuple[Grid, Mask]:
    g, m = grid.copy(), mask.copy()
    for i, j in np.array(list(zip(*np.nonzero(grid > 9)))):
        m[i, j], g[i, j] = True, 0
        for a, b in neighbors(i, j):
            if not m[a, b]:
                g[a, b] += 1
    return g, m


def flash(grid: Grid) -> Tuple[Grid, int]:
    old, new, mask = grid, *_flash_step(grid, np.zeros_like(grid, dtype=bool))
    while np.any(old != new):
        old, new, mask = new, *_flash_step(new, mask)
    return new, np.sum(new == 0)


def step(grid: Grid) -> Tuple[Grid, int]:
    return flash(grid + 1)


def part_1(grid: Grid) -> int:
    g, flashes = grid, 0
    for _ in range(100):
        g, f = step(g)
        flashes += f
    return flashes


def part_2(grid: Grid) -> int:
    steps = 0
    while not np.all(grid == 0):
        steps, grid = steps + 1, step(grid)[0]
    return steps


if __name__ == "__main__":
    for grid_file in ["test.txt", "input.txt"]:
        grid = read_grid(grid_file)
        print(part_1(grid))
        print(part_2(grid))
