:- module(genCode,[genCode/2]).
:- use_module(utils).

regs(['%edi','%esi','%edx','%ecx','%r8d','%r9d']).
set(L) :- nb_linkval(gen_codes,L).
get(L) :- nb_getval(gen_codes,L).
add(C) :- get(L),set([C|L]).

code(bin(Op,A,B),R) :-  genid('.ex.',R),code(A,A1),code(B,B1),add(bin(Op,A1,B1,R)).
code(mov(A,R),R) :-     atom(A),!,add(movl(A,R)).
code(mov(A,R),R) :-     integer(A),!,format(atom(D),'$~w',[A]),add(movl(D,R)).
code(mov(A,R),R) :-     code(A,R1),add(movl(R1,R)).
code(call(A,B),R) :-    genid('.ex.',R),maplist(code,B,Rs),add(call(A,Rs)),add(movl('%eax',R)).
code(R,R) :-            atom(R).
code(I,R) :-            integer(I),format(atom(R),'$~w',I).
code(E,_) :-            writeln(error:E),halt.
stmt(if(A,C,D),null) :- genid('.else',Else),genid('.then',Then),
                        code(A,R1),add(bne(R1,Then,Else)),genid('.cont',Cont),
                        add(label(Then)),maplist(stmt,C,_),add(br(Cont)),
                        add(label(Else)),maplist(stmt,D,_),add(br(Cont)),
                        add(label(Cont)).
stmt(ret(E),R) :-       code(E,R),add(ret(R)).
stmt(E,R) :-            code(E,R).
argv([],_).
argv([A|As],[R|Rs])  :- add(movl(R,A)),argv(As,Rs).
func((N,A,B),(N,L_)) :- set([]),regs(Regs),argv(A,Regs),
                        maplist(stmt,B,_),get(L),reverse(L,L_).
genCode(P,R) :- maplist(func,P,R),!.
