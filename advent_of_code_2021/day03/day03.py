import numpy as np
import scipy.stats as st


def read_file(name: str) -> np.ndarray:
    with open(name, "r") as f:
        return np.array([[int(b) for b in line if b in "01"] for line in f])


def to_int(bits: np.ndarray) -> int:
    bs = bits.flatten()
    return bs @ (2 ** np.arange(len(bs)))[::-1]


def part_1(bits: np.ndarray) -> int:
    m = st.mode(bits).mode
    return to_int(m) * to_int(1 - m)


def rating(bits: np.ndarray, oxygen: bool) -> int:
    bs = bits.copy()
    for i in range(bits.shape[0]):
        if len(bs) == 1:
            return to_int(bs)
        mode, count = st.mode(bs)
        m = mode[0, i] if 2 * count[0, i] != bs.shape[1] else 1
        bs = np.array([b for b in bs if b[i] == (m if oxygen else 1 - m)])
    raise ValueError(f"Couldn't reduce {bs}")


def part_2(bits: np.ndarray) -> int:
    return rating(bits, True) * rating(bits, False)


if __name__ == "__main__":
    for name in ["test.txt", "input.txt"]:
        print(name)
        bits = read_file(name)
        print(part_1(bits))
        print(part_2(bits))
