:- module(genCode,[genCode/2]).
:- use_module(utils).

regs(['%rdi','%rsi','%rdx','%rcx','%r8','%r9']).

init_bbs(Lbl)  :- nb_linkval(lbl,Lbl),nb_linkval(cs,[]),
                  nb_linkval(bbs,(H1,H1)).
add(C)         :- nb_getval(cs,Cs),
                  (Cs=[L|_],member(L,[call(_,_),br(_),bne(_,_,_),ret(_)])
                  ;nb_linkval(cs,[C|Cs])).
add_label(Lbl) :- nb_getval(lbl,Lb2),nb_getval(cs,Cs),reverse(Cs,Cs_),
                  nb_getval(bbs,(BBs,[(Lb2,Cs_)|H1])),nb_linkval(bbs,(BBs,H1)),
                  nb_linkval(lbl,Lbl),nb_linkval(cs,[]).
get_bbs(BBs)   :- nb_getval(lbl,Lbl),nb_getval(cs,Cs),reverse(Cs,Cs_),
                  nb_getval(bbs,(BBs,[(Lbl,Cs_)])).

code(bin(Op,A,B),R) :-  genid('.ex.',R),code(A,A1),code(B,B1),add(bin(Op,A1,B1,R)).
code(mov(A,R),R) :-     atom(A),!,add(mov(A,R)).
code(mov(A,R),R) :-     integer(A),!,format(atom(D),'$~w',[A]),add(mov(D,R)).
code(mov(A,R),R) :-     code(A,R1),add(mov(R1,R)).
code(call(A,B),R) :-    genid('.ex.',R),maplist(code,B,Rs),add(call(A,Rs,R)).
code(R,R) :-            atom(R),!.
code(I,R) :-            integer(I),!,format(atom(R),'$~w',I).
code(E,_) :-            writeln(error:E),halt.
stmt(if(A,C,D)) :-      genid('.else',Else),genid('.then',Then),
                        code(A,R1),add(bne(R1,Then,Else)),genid('.cont',Cont),
                        add_label(Then),maplist(stmt,C),add(br(Cont)),
                        add_label(Else),maplist(stmt,D),add(br(Cont)),
                        add_label(Cont).
stmt(ret(E)) :-         code(E,R),add(ret(R)).
stmt(E) :-              code(E,_).
argv([],_).
argv([A|As],[R|Rs])  :- add(mov(R,A)),argv(As,Rs).
func((N,A,B),(N,BBs)):- genid('.enter',Enter),init_bbs(Enter),
                        regs(Regs),argv(A,Regs),
                        maplist(stmt,B),get_bbs(BBs).
genCode(P,R) :- maplist(func,P,R),!.
