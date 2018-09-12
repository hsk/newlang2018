:- module(expand,[expand/2]).
:- use_module(genid).

regs(['%edi','%esi','%edx','%ecx','%r8d','%r9d']).
set(L) :- nb_linkval(expand_codes,L).
get(L) :- nb_getval(expand_codes,L).
add(C) :- get(L),set([C|L]).

code(add(A,B),R) :-       genid('.ex.',R),code(A,A1),code(B,B1),add(addl(A1,B1,R)).
code(mov(A,R),R) :-       atom(A),!,add(movl(A,R)).
code(mov(A,R),R) :-       integer(A),!,format(atom(D),'$~w',[A]),add(movl(D,R)).
code(mov(A,R),R) :-       code(A,R1),add(movl(R1,R)).
code(call(A,B),'%eax') :- maplist(code,B,Rs),add(call(A,Rs)).
code(if(A,B,C),null) :-   get(L),set([]),maplist(code,B,_),get(Lb),
                          set([]),maplist(code,C,_),get(Lc),set(L),
                          code(A,Ra),add(ifeq(Ra,'$0',Lb,Lc)).
code(ret(E),R) :-         code(E,R),add(ret(R)).
code(R,R) :-              atom(R).
code(E,_) :-              writeln(error:E),halt.

argv([],_).
argv([A|As],[R|Rs]) :- add(movl(R,A)),argv(As,Rs).
func((N,A,B),(N,L_)) :-
  set([]),regs(Regs),argv(A,Regs),
  maplist(code,B,_),get(L),reverse(L,L_).
expand(P,R) :- maplist(func,P,R),!.
