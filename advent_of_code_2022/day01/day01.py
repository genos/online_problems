def parse(path: str) -> list[list[int]]:
    with open(path) as p:
        return [[int(x) for x in group.split()] for group in p.read().split("\n\n")]


def part_1(groups: list[list[int]]) -> int:
    return max(sum(group) for group in groups)


def part_2(groups: list[list[int]]) -> int:
    return sum(sorted(sum(group) for group in groups)[-3:])


if __name__ == "__main__":
    groups = parse("./input.txt")
    print(part_1(groups))
    print(part_2(groups))
