ip=:1=#@q:                      NB. is prime
bn=:1+(10x^+&1)*666+10^+&3      NB. nth Belphegor number
bp=:ip@bn#]                     NB. ns yielding prime Belphegor numbers
NB. for example:
pb 11 12 13                     NB. prints 13
pb i.14                         NB. prints 0 13
NB. alternatively, all at once:
BP=:(1=#@q:)@(1+(10x^+&1)*666+10^+&3)#]
