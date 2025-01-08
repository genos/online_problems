#!/usr/bin/env bqn
Split←((¬-˜⊢×·+`»⊸>)∘≠⊔⊢) #bqncrate
i←⍉>(•Parsefloat¨' '⊸Split)¨•FLines"input.txt"  #https://panadestein.github.io/blog/aoc24.html
•Show+´|-´˘⍉∧˘i
x‿y←<˘i
Count←⊐˜∘⊣⊏ ≠∘⊣↑ /⁼∘⊐  #https://github.com/dzaima/aoc
•Show+´x×x Count y
