#!/usr/bin/env python
from typing import Any, Callable, List, Tuple

import networkx as nx
import numpy as np
import numpy.typing as npt

Cavern = npt.NDArray[np.int_]


def read_cavern(s: str) -> Cavern:
    with open(s) as f:
        return np.array([[int(c) for c in line.strip()] for line in f])


def neighbors(cavern: Cavern, i: int, j: int) -> List[Tuple[int, int]]:
    return [
        (i + di, j + dj)
        for (di, dj) in [(-1, 0), (1, 0), (0, -1), (0, 1)]
        if 0 <= i + di < cavern.shape[0] and 0 <= j + dj < cavern.shape[1]
    ]


def to_graph(cavern: Cavern) -> nx.Graph:
    g = nx.Graph()
    for (i, j), x in np.ndenumerate(cavern):
        g.add_node((i, j), weight=x)
        for a, b in neighbors(cavern, i, j):
            g.add_node((a, b), weight=cavern[a, b])
            g.add_edge((i, j), (a, b))
    return g


def solve(cavern: Cavern) -> int:
    g = to_graph(cavern)
    wf = lambda _u, v, _e: g.nodes[v]["weight"]
    m, n = cavern.shape
    return nx.dijkstra_path_length(g, (0, 0), (m - 1, n - 1), wf)


def part2_extend(cavern: Cavern) -> Cavern:
    bump = lambda c, i: np.where(c + i < 10, c + i, 1 + ((c + i) % 10))
    row = np.hstack([bump(cavern, i) for i in range(5)])
    return np.vstack([bump(row, i) for i in range(5)])


if __name__ == "__main__":
    for file in ["test.txt", "input.txt"]:
        print(file)
        cavern = read_cavern(file)
        print(f"Part 1: {solve(cavern)}")
        print(f"Part 2: {solve(part2_extend(cavern))}")
