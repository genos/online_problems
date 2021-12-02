% run with `swipl -g main,halt day01.pl
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

ints([]) --> eos.
ints([X]) --> integer(X).
ints([X|XS]) --> integer(X), "\n", ints(XS).
load_data(XS) :- phrase_from_file(ints(XS), 'input.txt').

drop(_, [], []).
drop(1, [_ | XS], XS).
drop(N, [_ | XS], YS) :- succ(M, N), drop(M, XS, YS).
chop(N, XS, YS) :- reverse(XS, AS), drop(N, AS, BS), reverse(BS, YS).

greater(X, Y, G) :- X > Y -> G #= 1; G #= 0.

part1(XS, Answer) :-
  drop(1, XS, AS),
  chop(1, XS, BS),
  maplist(greater, AS, BS, G),
  foldl(plus, G, 0, Answer).

part2(XS, Answer) :-
  drop(3, XS, AS),
  chop(3, XS, BS),
  maplist(greater, AS, BS, G),
  foldl(plus, G, 0, Answer).

main :-
  load_data(XS),
  part1(XS, A1), format("~d~n", A1),
  part2(XS, A2), format("~d~n", A2).
