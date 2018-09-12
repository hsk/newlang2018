:- module(setmem,[setmem/2]).
:- use_module(genid).

code(mov(A,B),mov(A1,B1)) :- code(A,A1),code(B,B1).
code(ret(A),ret(A1)) :- code(A,A1).
code(add(A,B),add(A1,B1)) :- code(A,A1),code(B,B1).
code(call(A,B),call(A,B1)) :- maplist(code,B,B1).
code(A,Id) :- integer(A),!,genid('.s.',Id),nb_getval(ls,Ls),nb_setval(ls,[mov(A,Id)|Ls]).
code(A,A) :- atom(A).
code(A,A) :- writeln(error(setmem:code(A))),halt.

func((N,A,B),(N,A,Ls2)) :-
  nb_setval(ls,[]),
  maplist(code,B,B2),
  nb_getval(ls,Ls),append(Ls,B2,Ls2).
setmem(E,R) :- maplist(func,E,R),!.
