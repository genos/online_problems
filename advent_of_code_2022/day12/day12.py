from pathlib import Path

import numpy as np
from numpy.typing import NDArray
import networkx as nx  # type: ignore

Chart = NDArray[np.uint8]
Node = tuple[int, int]


def to_height(s: str) -> int:
    return ord(s.replace("S", "a").replace("E", "z")) - ord("a")


def locate(x: str, text: list[str]) -> Node:
    for i, row in enumerate(text):
        for j, y in enumerate(row):
            if y == x:
                return (i, j)
    raise ValueError(f"Can't find {x} in {text}.")


def read_map(p: Path) -> tuple[Chart, nx.DiGraph, Node, Node]:
    text = p.read_text().split()
    start, end = [locate(c, text) for c in "SE"]
    chart = np.array(
        [[to_height(c) for c in line] for line in text], dtype=np.uint8
    )
    g = nx.DiGraph()
    for ((i, j), height) in np.ndenumerate(chart):
        g.add_node((i, j))
        for x, y in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
            if 0 <= x < chart.shape[0] and 0 <= y < chart.shape[1]:
                g.add_edge(
                    (i, j),
                    (x, y),
                    weight=1 if height + 1 >= chart[x, y] else float("inf"),
                )
    return (chart, g, start, end)


def part_1(g: nx.DiGraph, start: Node, end: Node) -> int:
    return nx.shortest_path_length(g, start, end, "weight")


def part_2(g: nx.DiGraph, chart: Chart, end: Node) -> int:
    return min(part_1(g, start, end) for start in zip(*np.where(chart == 0)))  # type: ignore


if __name__ == "__main__":
    chart, g, start, end = read_map(Path("input.txt"))
    print(part_1(g, start, end))
    print(part_2(g, chart, end))
