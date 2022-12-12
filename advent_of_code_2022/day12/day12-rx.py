from pathlib import Path

import numpy as np
from numpy.typing import NDArray
import rustworkx as rx

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


def _int(n: Node, chart: Chart) -> int:
    return n[1] * chart.shape[0] + n[0]


def read_map(p: Path) -> tuple[Chart, rx.PyDiGraph, Node, Node]:
    text = p.read_text().split()
    start, end = [locate(c, text) for c in "SE"]
    chart = np.array([[to_height(c) for c in line] for line in text], dtype=np.uint8)
    g = rx.PyDiGraph()
    g.add_nodes_from([_int((i, j), chart) for i, j in np.ndindex(chart.shape)])
    for ((i, j), height) in np.ndenumerate(chart):
        for x, y in [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]:
            if (
                0 <= x < chart.shape[0]
                and 0 <= y < chart.shape[1]
                and height + 1 >= chart[x, y]
            ):
                g.add_edge(_int((i, j), chart), _int((x, y), chart), 1)
    return (chart, g, start, end)


def part_1(g: rx.PyDiGraph, chart: Chart, start: Node, end: Node) -> int | None:
    shortest = rx.dijkstra_shortest_path_lengths(
        g,
        _int(start, chart),
        lambda w: w,
        goal=_int(end, chart),
    )
    return int(shortest[_int(end, chart)]) if shortest else None


def part_2(g: rx.PyDiGraph, chart: Chart, end: Node) -> int:
    return min(
        filter(None, (part_1(g, chart, s, end) for s in zip(*np.where(chart == 0))))  # type: ignore
    )


if __name__ == "__main__":
    chart, g, start, end = read_map(Path("input.txt"))
    print(part_1(g, chart, start, end))
    print(part_2(g, chart, end))
