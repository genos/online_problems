import pandas as pd
depths = pd.read_csv("./input.txt", header=None, squeeze=True)
print(sum(depths.diff() > 0))
print(sum(depths.diff(3) > 0))
