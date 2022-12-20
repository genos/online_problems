from collections import deque
from pathlib import Path


def read_seq(p: str, key=1) -> deque[tuple[int, int]]:
    return deque(enumerate(int(x) * key for x in Path(p).read_text().split("\n") if x))


def mix(seq: deque[tuple[int, int]], num_times: int = 1) -> deque[tuple[int, int]]:
    for _ in range(num_times):
        for i in range(len(seq)):
            while seq[0][0] != i:
                seq.rotate(-1)
            order, shift = seq.popleft()
            seq.rotate(-1 * shift)
            seq.appendleft((order, shift))
    return seq


def coordinates(seq: deque[tuple[int, int]]) -> int:
    while seq[0][1] != 0:
        seq.rotate(-1)
    return sum(seq[i % len(seq)][1] for i in [1_000, 2_000, 3_000])


if __name__ == "__main__":
    print(coordinates(mix(read_seq("input.txt"))))
    print(coordinates(mix(read_seq("input.txt", 811589153), 10)))
