import numpy as np
from numpy.typing import NDArray

Board = NDArray[np.uint64]


def read_board(path: str) -> Board:
    with open(path) as p:
        return np.array([[int(c) for c in line.strip()] for line in p], dtype=np.uint64)


def viz(b: Board, i: int, j: int) -> np.uint64:
    m, n = b.shape
    vs = np.ones(4, dtype=np.uint64)
    for x in range(1, i + 1):
        if b[i - x, j] >= b[i, j]:
            vs[0] = 0
            break
    for x in range(i + 1, m):
        if b[x, j] >= b[i, j]:
            vs[1] = 0
            break
    for y in range(1, j + 1):
        if b[i, j - y] >= b[i, j]:
            vs[2] = 0
            break
    for y in range(j + 1, n):
        if b[i, y] >= b[i, j]:
            vs[3] = 0
            break
    return np.uint64(np.logical_or.reduce(vs))


def part1(b: Board) -> np.uint64:
    return np.sum([viz(b, i, j) for i, j in np.ndindex(b.shape)])


def views(b: Board, i: int, j: int) -> np.uint64:
    m, n = b.shape
    vs = np.zeros(4, dtype=np.uint64)
    for x in range(1, i + 1):
        vs[0] += 1
        if b[i - x, j] >= b[i, j]:
            break
    for x in range(i + 1, m):
        vs[1] += 1
        if b[x, j] >= b[i, j]:
            break
    for y in range(1, j + 1):
        vs[2] += 1
        if b[i, j - y] >= b[i, j]:
            break
    for y in range(j + 1, n):
        vs[3] += 1
        if b[i, y] >= b[i, j]:
            break
    return np.prod(vs)


def part2(b: Board) -> np.uint64:
    return np.max([views(b, i, j) for i, j in np.ndindex(b.shape)])


if __name__ == "__main__":
    board = read_board("input.txt")
    print(part1(board))
    print(part2(board))
