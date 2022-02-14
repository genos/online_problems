# Using SAT solving for Day 24 of AOC 2021

While I never made it this far myself, my interest in [day
24](https://adventofcode.com/2021/day/24) by an [interesting blog
post](https://medium.com/@predrag.gruevski/compiler-adventures-part-1-no-op-instructions-c084358c7864)
wherein the writer uses it as an example for writing a compiler.

From there, I also headed to the [Reddit solution
thread](https://www.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/)
to see what others had done, and came across some interesting examples of
people using SAT solvers (mainly [Z3](https://github.com/Z3Prover/z3)) to solve the problem.

From there, I've plagiarized and modified two solutions:

- `Main.hs` is a Haskell version done with help from [this post](https://www.reddit.com/r/adventofcode/comments/rnejv5/comment/hpsc57y), while
- `day24.py` is a Python versions with help from [this one](https://www.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/hpshymr/).


I also found [this Racket/Rosette
version](https://gist.github.com/dschoepe/86df14fa695717149dc7a3ab4f1db64f)
pretty enlightening.
