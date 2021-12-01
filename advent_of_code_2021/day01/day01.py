import pandas as pd
depths = pd.read_csv("./input.txt", header=None, squeeze=True)
print(sum(depths.diff() > 0))
print(sum(depths.rolling(3).sum().diff() > 0))
