:- module(genCode,[genCode/2,resetid/0,genid/2]).
resetid     :- retractall(id(_)),assert(id(0)).
genid(S,A)  :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'.~w~w',[S,C]).

add(C)      :- assert(c(C)).
label(L)    :- commit,assert(l(L)).
commit      :- retract(l(L)),findall(C,retract(c(C)),Cs),assert(bb(L:Cs));!.
commit(BBs) :- add(ret($0)),commit,findall(BB,retract(bb(BB)),BBs).

expr(bin(Op,A,B),R) :-  genid(r,R),expr(A,A1),expr(B,B1),add(bin(Op,A1,B1,R)).
expr(mov(A,R),R) :-     expr(A,R1),add(mov(R1,R)).
expr(call(A,B),R) :-    genid(r,R),maplist(expr,B,Rs),add(call(A,Rs,R)).
expr(R,R) :-            atom(R),!.
expr(I,$I) :-           integer(I),!.
expr(E,_) :-            throw(genCode(expr(E))).
stmt(if(A,C,D)) :-      genid(then,Then),genid(else,Else),
                        expr(A,R1),add(bne(R1,Then,Else)),genid(cont,Cont),
                        label(Then),stmt(C),add(br(Cont)),
                        label(Else),stmt(D),add(br(Cont)),label(Cont).
stmt(while(A,B)) :-     genid(while,While),genid(then,Then),genid(cont,Cont),
                        label(While),expr(A,R1),add(bne(R1,Then,Cont)),
                        label(Then),stmt(B),add(br(While)),label(Cont).
stmt(ret(E)) :-         expr(E,R),add(ret(R)).
stmt(B) :-              is_list(B),!,forall(member(S,B),stmt(S)).
stmt(E) :-              expr(E,_).
func(N:A=B,N:A=BBs) :-  genid(enter,E),label(E),stmt(B),commit(BBs).
genCode(P,R) :-         resetid,dynamic(last/0),maplist(func,P,R),!.
