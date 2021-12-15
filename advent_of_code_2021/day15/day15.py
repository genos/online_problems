#!/usr/bin/env python
from typing import Any, Callable, Iterator, Tuple

import networkx as nx
import numpy as np
import numpy.typing as npt

Cavern = npt.NDArray[np.int_]


def read_cavern(s: str) -> Cavern:
    with open(s) as f:
        return np.array([[int(c) for c in line.strip()] for line in f])


def neighbors(cavern: Cavern, i: int, j: int) -> Iterator[Tuple[int, int]]:
    m, n = cavern.shape
    for di, dj in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        if (di, dj) != (0, 0) and 0 <= i + di < m and 0 <= j + dj < n:
            yield i + di, j + dj


def part_1_to_graph(cavern: Cavern) -> nx.Graph:
    g = nx.Graph()
    for (i, j), x in np.ndenumerate(cavern):
        if (i, j) not in g:
            g.add_node((i, j), weight=x)
        for a, b in neighbors(cavern, i, j):
            if (a, b) not in g:
                g.add_node((a, b), weight=cavern[a, b])
            g.add_edge((i, j), (a, b))
    return g


def part_1_end(cavern: Cavern) -> Tuple[int, int]:
    m, n = cavern.shape
    return m - 1, n - 1


def weight_fn(graph: nx.Graph) -> Callable[[Any, Any, Any], int]:
    return lambda _u, v, _e: graph.nodes[v]["weight"]


def solve(
    cavern: Cavern,
    to_graph: Callable[[Cavern], nx.Graph],
    to_end: Callable[[Cavern], Tuple[int, int]],
) -> int:
    g = to_graph(cavern)
    return nx.dijkstra_path_length(g, (0, 0), to_end(cavern), weight_fn(g))


if __name__ == "__main__":
    for file in ["test.txt", "input.txt"]:
        cavern = read_cavern(file)
        print(solve(cavern, part_1_to_graph, part_1_end))
