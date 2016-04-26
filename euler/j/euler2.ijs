NB. http://code.jsoftware.com/wiki/Essays/Fibonacci_Sequence
f7=: 3 : 0
    mp=. +/ .*
    {.{: mp/ mp~^:(I.|.#:y) 2 2$0 1 1 1x
)
f=:f7"0&(2&+)&i. NB. list, start with 1,2,.. per spec
4e6<(>./f 33)    NB. 1...
1e6<(>./f 32)    NB. ...0
+/((0=2|])#])f 32
