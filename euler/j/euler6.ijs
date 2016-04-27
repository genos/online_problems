ps=:+/@:*:@:>:@:i.   NB. sum of squares (plus of square)
sp=:*:@:(+/@:>:@:i.) NB. square of sum
(ps-sp)100
NB. better, taken from the forum
ps=:[:+/*:
sp=:[:*:+/
(ps-sp)i.101
