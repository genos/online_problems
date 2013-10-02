#!/usr/bin/env python3
def naive_lucas(n, p, q):
    if n <= 0:
        return (0, 2)
    elif n == 1:
        return (1, p)
    else:
        u, v = naive_lucas(n - 1, p, q)
        uu, vv = naive_lucas(n - 2, p, q)
        return (p * u - q * uu, p * v - q * vv)


def lucas_iter(p, q):
    u, v, uu, vv = 1, p, 0, 2
    while True:
        yield (uu, vv)
        u, v, uu, vv = p * u - q * uu, p * v - q * vv, u, v


def lucas_v(n, p, q):
    if n <= 0:
        return 2
    elif n == 1:
        return p
    else:
        k = n >> 1
        if n & 1:
            return lucas_v(k + 1, p, q) * lucas_v(k, p, q) - p * q**k
        else:
            return lucas_v(k, p, q)**2 - 2 * q**k


if __name__ == '__main__':
    p, q = 1, -1
    N = 10
    print([naive_lucas(n, p, q) for n in range(N)])
    li = lucas_iter(p, q)
    print([next(lucas_iter(p, q)) for n in range(N)])
    print([lucas_v(n, p, q) for n in range(N)])
