:- module(genCode,[genCode/2]).
:- use_module(utils).

regs(['%edi','%esi','%edx','%ecx','%r8d','%r9d']).

init_bbs(Lbl)  :- nb_linkval(lbl,Lbl),nb_linkval(cs,(H,H)),
                  nb_linkval(bbs,(H1,H1)).
add(C)         :- nb_getval(cs,(Cs,[C|H])),nb_linkval(cs,(Cs,H)).
add_label(Lbl) :- nb_getval(lbl,Lb2),nb_getval(cs,(Cs,[])),
                  nb_getval(bbs,(BBs,[(Lb2,Cs)|H1])),nb_linkval(bbs,(BBs,H1)),
                  nb_linkval(lbl,Lbl),nb_linkval(cs,(H,H)).
get_bbs(BBs)   :- nb_getval(lbl,Lbl),nb_getval(cs,(Cs,[])),
                  nb_getval(bbs,(BBs,[(Lbl,Cs)])).

code(bin(Op,A,B),R) :-  genid('.ex.',R),code(A,A1),code(B,B1),add(bin(Op,A1,B1,R)).
code(mov(A,R),R) :-     atom(A),!,add(movl(A,R)).
code(mov(A,R),R) :-     integer(A),!,format(atom(D),'$~w',[A]),add(movl(D,R)).
code(mov(A,R),R) :-     code(A,R1),add(movl(R1,R)).
code(call(A,B),R) :-    genid('.ex.',R),maplist(code,B,Rs),add(call(A,Rs)),add(movl('%eax',R)).
code(R,R) :-            atom(R),!.
code(I,R) :-            integer(I),!,format(atom(R),'$~w',I).
code(E,_) :-            writeln(error:E),halt.
stmt(if(A,C,D),null) :- genid('.else',Else),genid('.then',Then),
                        code(A,R1),add(bne(R1,Then,Else)),genid('.cont',Cont),
                        add_label(Then),maplist(stmt,C,_),add(br(Cont)),
                        add_label(Else),maplist(stmt,D,_),add(br(Cont)),
                        add_label(Cont).
stmt(ret(E),R) :-       code(E,R),add(ret(R)).
stmt(E,R) :-            code(E,R).
argv([],_).
argv([A|As],[R|Rs])  :- add(movl(R,A)),argv(As,Rs).
func((N,A,B),(N,BBs)):- genid('.enter',Enter),init_bbs(Enter),
                        regs(Regs),argv(A,Regs),
                        maplist(stmt,B,_),get_bbs(BBs).
genCode(P,R) :- maplist(func,P,R),!.
