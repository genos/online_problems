pi=:p:^:_1         NB. π(n), number of primes < n, same as _1 p:
17=+/p:i.pi 10     NB. checking
+/p:i.pi 2e6       NB. +/p:i._1 p:2e6, hakank.org/j
+/i.&.(p:^:_1) 2e6 NB. inspired by Roger Hui, using "under"
