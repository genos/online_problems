# with help from https://topaz.github.io/paste/#XQAAAQBbCgAAAAAAAAA0m0pnuFI8c+qagMoNTEcTIfyUW1xf5cj5UWEFGhQ7Ii8lr4cN5NkhGKvl3n4bZ0aogI13PFfQ1x4u5EQovTXkDFVV8fJvAI0Y3dOKxdxqpCNgsMPQbWmbpulsLc6wiPNSCuB6BhtZJXyyHtbTHOICz1A+KpENLNnFmyb5n8fMUuw1Zz3/GB2nDasX3GkP33bjyEo5J4Nly5CitzkS60IP2tUkTYpxi7WLDgEShCk4gb8RHTRjv5r5IaUivwiDYstRbZLY7HYDOyy0GdaKz2oURXw9xssVNijoPZTxegzFZwJGxXNJgkMagoVjZ5TJLRlSUievK+uF5YHVrGWZpb/VIa9GWcibUeUkTi1Qs/sSEHmC8ZSkpWoWyI498Vn5aVct72gpaXnzCvvdTmH/GEfWDd+7b13bUImV59zclpEzULQxdMNf/aYSsf6WvhNA7E3J+9m3Vcmb74tRtd7WixW0kTeODqX3KjQSQLLZtL0HjrtRt3qXNfbir1+UihiXNUnWmR/CHJnighPYo+6yRakKOaXQweCXNZWt8LQNZ7EKscrqZp1zLvA/M50O40J7ArmHN3Wp3OUhGeAmcRhn54vc0cXynrEZKoDHVk0fTGvJnPovVclYQ791zmMWn5zdMrFrw3FeO7vMZCc3dyPQpCEGcepcvJIkNMEQ0fHlijgmDSvRl6fwwGik6gDzvqLjeC8j6zp4OEo6njwUnHqI0Sw6/IHoWfaI8l7ulqZRB0XrrfXmMNNKmjo+zvjMytqBw0DTlcNpejAMoJVUP+GQZ2JipIhLaM2JWP6dZKYbCLdz79HdoxyKa5LcF5iTjDd6lwQrKy97GBpqmFLiUzv+m6pExi7yF57SPVVbElskVDnOEAclKB2mic43XUUanVksPaDkZguiZ4m8DXzV/618tV6y89TbIHYnx537IOOmd8lsn1cVJyeEV2ibKmRHIOyzVoux/KMZ7qNtkIzCh/0j8jgVm940dhlv721Ji3fLa+EoxS6cQ0gsRCW5hD4bGjy2BnHS4vTxywyKS61H8pJXraSj0OiJLx+PbZqMwwaoiTi//9AQiGE=
from __future__ import annotations
from functools import reduce
from operator import mul
from pathlib import Path
import re
from typing import NamedTuple

import cvxpy as cp
import numpy as np
from numpy.typing import NDArray

CostMatrix = NDArray[np.int_]


class Blueprint(NamedTuple):
    id: int
    costs: CostMatrix

    @classmethod
    def from_line(cls, line: str) -> Blueprint:
        id, *[
            ore_ore,
            clay_ore,
            obsidian_ore,
            obsidian_clay,
            geode_ore,
            geode_obsidian,
        ] = [int(x) for x in re.findall(r"\d+", line)]
        return cls(
            id,
            np.array(
                [
                    [ore_ore, clay_ore, obsidian_ore, geode_ore],
                    [0, 0, obsidian_clay, 0],
                    [0, 0, 0, geode_obsidian],
                    [0, 0, 0, 0],
                ],
            ),
        )

    def max_geodes(self, time: int) -> int:
        materials, robots, builds, constraints = [], [], [], []
        for i in range(time + 1):
            m = cp.Variable(4, integer=True)
            r = cp.Variable(4, integer=True)
            b = cp.Variable(4, boolean=True)
            if i == 0:
                constraints.append(m == 0)  # no minerals
                constraints.append(r[0] == 1)  # one ore robot
                constraints.append(r[1:] == 0)  # no other robots
                constraints.append(b == 0)  # haven't built anything
            else:
                constraints.append(m >= 0)
                constraints.append(r >= 0)
                # add new robots
                constraints.append(r == robots[-1] + builds[-1])
                # can only build one robot
                constraints.append(cp.sum(b) <= 1)
                # can only build if we have materials
                constraints.append(self.costs @ b <= materials[-1])
                # new materials: previous + 1 for each robot - those used for building
                constraints.append(m == (materials[-1] + r - (self.costs @ b)))
            materials.append(m)
            robots.append(r)
            builds.append(b)
        problem = cp.Problem(cp.Maximize(materials[-1][-1]), constraints)
        problem.solve()
        return int(problem.value)


def read_blueprints(file: str) -> list[Blueprint]:
    return [
        Blueprint.from_line(line) for line in Path(file).read_text().split("\n") if line
    ]


def part_1(bps: list[Blueprint]) -> int:
    return sum(b.id * b.max_geodes(24) for b in bps)


def part_2(bs: list[Blueprint]) -> int:
    return reduce(mul, (b.max_geodes(32) for b in bps[:3]), 1)


if __name__ == "__main__":
    bps = read_blueprints("input.txt")
    print(part_1(bps))
    print(part_2(bps))
