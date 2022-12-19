# help from https://github.com/dsagman/advent-2022/blob/main/day15/day15.py
from pathlib import Path
import re

from shapely.geometry import box, LineString, Polygon
from shapely.ops import unary_union


RX = re.compile(r"x=(-?\d+), y=(-?\d+): .* x=(-?\d+), y=(-?\d+)")


def signals_beacons(p: Path) -> list[tuple[int, int, int, int]]:
    return [
        (int(sx), int(sy), int(bx), int(by))
        for (sx, sy, bx, by) in RX.findall(p.read_text())
    ]


def _range(a: int, b: int) -> int:
    return max(a, b) - min(a, b)


def _coverage(sb: list[tuple[int, int, int, int]]) -> list[Polygon]:
    coverage = []
    for sx, sy, bx, by in sb:
        x_range, y_range = _range(sx, bx), _range(sy, by)
        d = x_range + y_range
        coverage.append(
            Polygon([(sx + d, sy), (sx, sy - d), (sx - d, sy), (sx, sy + d)])
        )
    return coverage


def part_1(sb: list[tuple[int, int, int, int]], y_row: int = 2_000_000) -> int:
    coverage = _coverage(sb)
    x_min, x_max = [f(f(p.bounds[0::2]) for p in coverage) for f in (min, max)]
    row = LineString([(x_min, y_row), (x_max, y_row)]).intersection(
        unary_union(coverage)
    )
    beacons_on_row = {(bx, by) for _, _, bx, by in sb if by == y_row}
    return 1 + int(row.length - len(beacons_on_row))


def part_2(sb: list[tuple[int, int, int, int]], limit: int = 4_000_000) -> int:
    coverage = unary_union(_coverage(sb))
    x, y, _, _ = [1 + b for b in box(0, 0, limit, limit).difference(coverage).bounds]
    return int(limit * x + y)


if __name__ == "__main__":
    sb = signals_beacons(Path(__file__).parent / "input.txt")
    for part in [part_1, part_2]:
        print(part(sb))
