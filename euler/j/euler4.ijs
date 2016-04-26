d=:10&#.inv             NB. base 10 digits
ip=:*./&(|.=])          NB. is palindrome
p=:~.,(*/~)(100+i.900x) NB. products of two three digit nums
>./(ip"1(d p))#p
