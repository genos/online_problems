from string import ascii_letters


def priority(s: str) -> int:
    return 1 + ascii_letters.find(s)


def part_1(file: str) -> int:
    with open(file) as f:
        priorities = 0
        for line in f:
            line = line.strip()
            n = len(line) >> 1
            first, second = set(line[:n]), set(line[n:])
            priorities += priority((first & second).pop())
        return priorities


def part_2(file: str) -> int:
    with open(file) as f:
        return sum(
            priority((set(a) & set(b) & set(c)).pop())
            # https://docs.python.org/3/library/itertools.html#itertools-recipes
            for a, b, c in zip(*([iter(line.strip() for line in f)] * 3), strict=True)
        )


if __name__ == "__main__":
    print(part_1("input.txt"))
    print(part_2("input.txt"))
