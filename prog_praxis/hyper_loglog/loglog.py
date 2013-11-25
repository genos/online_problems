
from math import gamma, log
k = 16
   
def alpha(m):
    return pow(gamma(-1 / m) * (pow(2, 1 / m) - 1) / log(2), -m)

def rho(n):
    for i in range(k):
        if (n & (1 << (k - i))) != 0:
            return i
    return k

def h(x):
    return hash(x) % (1 << 32)

def loglog(MS):
    m = 1 << k
    M = [0] * m
    for x in MS:
        i, j = divmod(h(x), m)
        M[j] = max(M[j], rho(i))
    return alpha(m) * m * pow(2, sum(M) / m)

if __name__ == '__main__':
    with open('/usr/share/dict/words') as f:
        MS = [line.strip() for line in f]
    print(loglog(MS))
