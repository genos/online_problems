from pathlib import Path

from sympy import solve

Monkeys = dict[str, str | list[str]]


def parse(file: str) -> Monkeys:
    monkeys = {}
    for line in Path(file).read_text().split("\n"):
        if not line:
            continue
        key, rest = line.split(": ", 1)
        value: str | list[str] = rest if rest.isdigit() else rest.split()
        monkeys[key] = value
    return monkeys


def to_expr(monkeys: Monkeys, key: str):
    value = monkeys[key]
    if isinstance(value, str):
        return value
    else:
        x, op, y = value
        return f"({to_expr(monkeys, x)}) {op} ({to_expr(monkeys, y)})"


def part1(monkeys: Monkeys) -> int:
    return int(eval(to_expr(monkeys, "root")))


def part2(monkeys: Monkeys) -> int:
    ms = monkeys.copy()
    ms["root"][1] = "-"  # type: ignore
    ms["humn"] = "humn"
    return solve(to_expr(ms, "root"), "humn")[0]


if __name__ == "__main__":
    monkeys = parse("input.txt")
    for part in [part1, part2]:
        print(part(monkeys))
