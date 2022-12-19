# help from https://github.com/dsagman/advent-2022/blob/main/day15/day15.py
from pathlib import Path
import re

from shapely.geometry import LineString, Polygon
from shapely.ops import clip_by_rect, unary_union


RX = re.compile(r"x=(-?\d+), y=(-?\d+): .* x=(-?\d+), y=(-?\d+)")


def signals_beacons(p: Path) -> list[tuple[int, int, int, int]]:
    return [
        (int(sx), int(sy), int(bx), int(by))
        for (sx, sy, bx, by) in RX.findall(p.read_text())
    ]


def _range(a: int, b: int) -> int:
    return max(a, b) - min(a, b)


def part_1(sbs: list[tuple[int, int, int, int]], y_row: int = 2000000) -> int:
    polygons = []
    for sx, sy, bx, by in sbs:
        x_range, y_range = _range(sx, bx), _range(sy, by)
        d = x_range + y_range
        polygons.append(
            Polygon([(sx + d, sy), (sx, sy - d), (sx - d, sy), (sx, sy + d)])
        )
    x_min, x_max = [f(f(p.bounds[0::2]) for p in polygons) for f in (min, max)]
    row = LineString([(x_min, y_row), (x_max, y_row)]).intersection(
        unary_union(polygons)
    )
    beacons_on_row = {
        (bx, by) for _, _, bx, by in sbs if by == y_row
    }
    return int(row.length - len(beacons_on_row) + 1)
