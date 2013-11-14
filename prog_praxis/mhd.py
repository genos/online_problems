Here's an implementation in Python. The problem statement seems a
bit ambiguous to me; it doesn't specify that the pairs should be of
two distinct numbers. Hence the snarky solution is to return zero,
the distance from the first number in your list to itself.
[sourcecode lang='python']
#!/usr/bin/env python3
from itertools import combinations, starmap

def snarky(ns):
    """The minimum distance between arbitrary pairs is zero (there was no
    stipulation that the pairs must be of distinct items; dist(ns[0],
    ns[0]) = 0).
    """
    return 0

def hamming_distance(m, n):
    """Hamming Distance between two natural numbers is the number of ones
    in the binary representation of their xor.
    """
    return sum(map(int, bin(m ^ n)[2:]))

def min_hamming_dist(ns):
    """Minimum Hamming Distant between pairs of two different ns"""
    return min(starmap(hamming_distance, combinations(ns, 2)))

if __name__ == '__main__':
    print(hamming_distance(79, 83))
    ns = [13500, 1935, 9645, 5790]
    print(snarky(ns))
    print(min_hamming_dist(ns))
[/sourcecode]
