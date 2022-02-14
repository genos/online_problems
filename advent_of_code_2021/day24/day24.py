# with help from https://www.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/hpshymr/
from functools import reduce
from itertools import count

import z3


def solve():
    solver = z3.Optimize()
    zero, one = z3.BitVecVal(0, 64), z3.BitVecVal(1, 64)
    alu = {r: zero for r in "xyzw"}
    inputs, input_counter, var_counter = [], count(), count()
    with open("input.txt") as f:
        for line in f:
            instruction, *places = line.split()
            if instruction == "inp":
                d = z3.BitVec(f"d_{next(input_counter)}", 64)
                solver.add(1 <= d, d <= 9)
                alu[places[0]] = d
                inputs.append(d)
            else:
                a = alu[places[0]]
                b = alu[places[1]] if places[1] in alu else int(places[1])
                c = z3.BitVec(f"v_{next(var_counter)}", 64)
                if instruction == "add":
                    solver.add(c == a + b)
                elif instruction == "mul":
                    solver.add(c == a * b)
                elif instruction == "mod":
                    solver.add(a >= 0, b > 0, c == a % b)
                elif instruction == "div":
                    solver.add(b != 0, c == a / b)
                elif instruction == "eql":
                    solver.add(c == z3.If(a == b, one, zero))
                else:
                    raise ValueError(f"Unknown instruction {instruction[0]}")
                alu[places[0]] = c
    solver.add(alu["z"] == 0)
    num = reduce(lambda acc, digit: 10 * acc + digit, inputs)
    for name, method in [("maximize", solver.maximize), ("minimize", solver.minimize)]:
        print(name)
        solver.push()
        method(num)
        if solver.check():
            model = solver.model()
            print("".join(str(model[d]) for d in inputs))
        else:
            print("SOLVER FAILED")
        solver.pop()


if __name__ == "__main__":
    solve()
