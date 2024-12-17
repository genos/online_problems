# help from https://github.com/elklepo/aoc/blob/main/2024/day17/xpl.py
import re
from dataclasses import dataclass

import typer
from pyprojroot import here
from z3 import BitVec, BitVecRef, Optimize, sat


@dataclass
class Part1:
    prog: list[int]
    a: int | BitVecRef
    b: int | BitVecRef
    c: int | BitVecRef
    ip: int = 0
    print_out: bool = True

    def combo(self, w: int) -> int | BitVecRef:
        match w:
            case 4:
                return self.a
            case 5:
                return self.b
            case 6:
                return self.c
            case _:
                return w

    def jnz(self, w: int) -> bool:
        if self.a != 0:  # type: ignore
            self.ip = w
            return True
        return False

    def out(self, w: int, output: list[int] | None = None):
        if output is not None:
            c = self.combo(w) % 8
            assert isinstance(c, int)
            output.append(c)

    def run(self):
        output = []
        while self.ip < len(self.prog):
            w = self.prog[self.ip + 1]
            match self.prog[self.ip]:
                case 0:
                    self.a >>= self.combo(w)
                case 1:
                    self.b ^= w
                case 2:
                    self.b = self.combo(w) % 8
                case 3:
                    if self.jnz(w):
                        self.ip = w
                        continue
                case 4:
                    self.b ^= self.c
                case 5:
                    self.out(w, output)
                case 6:
                    self.b = self.a >> self.combo(w)
                case 7:
                    self.c = self.a >> self.combo(w)
                case _:
                    pass
            self.ip += 2
        if self.print_out:
            print(",".join(str(i) for i in output))


@dataclass
class Part2(Part1):
    print_out: bool = False
    opt: Optimize = Optimize()
    out_ptr: int = 0

    def jnz(self, w: int) -> bool:
        if self.out_ptr == len(self.prog):
            self.opt.add(self.a == 0)
            return False
        return True

    def out(self, w: int, output: list[int] | None = None):
        if self.out_ptr < len(self.prog):
            self.opt.add(self.combo(w) % 8 == self.prog[self.out_ptr])
            self.out_ptr += 1

    def run(self, print_out: bool = False):
        start_a = self.a
        super().run()
        self.opt.minimize(start_a)
        assert self.opt.check() == sat
        print(self.opt.model()[start_a])


cli = typer.Typer()


@cli.command()
def part2():
    with open(here("input.txt")) as f:
        first, second = f.read().split("\n\n")
        a, b, c = (int(r) for r in re.findall(r"\d+", first))
        prog = [int(c) for c in second.split("Program: ")[1].split(",")]
        Part1(prog, a, b, c).run()
        Part2(prog, BitVec("a", 64), b, c).run()
