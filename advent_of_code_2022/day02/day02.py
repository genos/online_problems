from collections.abc import Iterable
from enum import Enum, IntEnum

WDL = IntEnum("WDL", {"W": 6, "D": 3, "L": 0})
RPS = IntEnum("RPS", {"R": 1, "P": 2, "S": 3})
ABC = Enum("ABC", ["A", "B", "C"])
XYZ = Enum("XYZ", ["X", "Y", "Z"])


def read_input(file: str) -> Iterable[tuple[ABC, XYZ]]:
    with open(file) as f:
        for line in f:
            t, m = line.split()
            yield ABC[t], XYZ[m]


def scores(them: RPS, me: RPS) -> WDL:
    return {
        (RPS.R, RPS.R): WDL.D,
        (RPS.R, RPS.P): WDL.W,
        (RPS.R, RPS.S): WDL.L,
        (RPS.P, RPS.R): WDL.L,
        (RPS.P, RPS.P): WDL.D,
        (RPS.P, RPS.S): WDL.W,
        (RPS.S, RPS.R): WDL.W,
        (RPS.S, RPS.P): WDL.L,
        (RPS.S, RPS.S): WDL.D,
    }[them, me]


def part_1(pairs: Iterable[tuple[ABC, XYZ]]) -> int:
    them, me = dict(zip(ABC, RPS)), dict(zip(XYZ, RPS))
    return sum(scores(them[abc], me[xyz]) + me[xyz] for (abc, xyz) in pairs)


def win(them: RPS) -> int:
    return {RPS.R: RPS.P, RPS.P: RPS.S, RPS.S: RPS.R}[them] + WDL.W


def draw(them: RPS) -> int:
    return {RPS.R: RPS.R, RPS.P: RPS.P, RPS.S: RPS.S}[them] + WDL.D


def lose(them: RPS) -> int:
    return {RPS.R: RPS.S, RPS.P: RPS.R, RPS.S: RPS.P}[them] + WDL.L


def part_2(pairs: Iterable[tuple[ABC, XYZ]]) -> int:
    them = dict(zip(ABC, RPS))
    me = {XYZ.X: lose, XYZ.Y: draw, XYZ.Z: win}
    return sum(me[xyz](them[abc]) for abc, xyz in pairs)


if __name__ == "__main__":
    print(part_1(read_input("input.txt")))
    print(part_2(read_input("input.txt")))
