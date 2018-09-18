:- module(genCode,[genCode/2,resetid/0,genid/2]).
resetid      :- retractall(id(_)),assert(id(0)).
genid(S,A)   :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'.~w~w',[S,C]).
initBBs(Lbl) :- asserta(lbl(Lbl)).
add(C)       :- cs(L),member(L,[br(_),bne(_,_,_),ret(_)]);asserta(cs(C)).
label(Lbl)   :- retract(lbl(Lb2)),assert(lbl(Lbl)),
                findall(C,retract(cs(C)),Cs),reverse(Cs,Cs_),assert(bb(Lb2:Cs_)).
bb(Lbl,F,B)  :- label(Lbl),call(F),add(B).
getBBs(BBs)  :- retract(lbl(Lbl)),findall(C,retract(cs(C)),Cs),reverse(Cs,Cs_),
                findall(BB,retract(bb(BB)),BBs,[Lbl:Cs_]).
expr(bin(Op,A,B),R) :-  genid(r,R),expr(A,A1),expr(B,B1),add(bin(Op,A1,B1,R)).
expr(mov(A,R),R) :-     expr(A,R1),add(mov(R1,R)).
expr(call(A,B),R) :-    genid(r,R),maplist(expr,B,Rs),add(call(A,Rs,R)).
expr(R,R) :-            atom(R),!.
expr(I,$I) :-           integer(I),!.
expr(E,_) :-            throw(genCode(expr(E))).
stmt(if(A,C,D)) :-      genid(then,Then),genid(else,Else),
                        expr(A,R1),add(bne(R1,Then,Else)),genid(cont,Cont),
                        bb(Then,forall(member(S,C),stmt(S)),br(Cont)),
                        bb(Else,forall(member(S,D),stmt(S)),br(Cont)),label(Cont).
stmt(while(A,B)) :-     genid(while,While),genid(then,Then),genid(cont,Cont),
                        bb(While,expr(A,R1),bne(R1,Then,Cont)),
                        bb(Then,forall(member(S,B),stmt(S)),br(While)),label(Cont).
stmt(ret(E)) :-         expr(E,R),add(ret(R)).
stmt(E) :-              expr(E,_).
func(N:A=B,N:A=BBs) :-  genid(enter,E),initBBs(E),forall(member(S,B),stmt(S)),getBBs(BBs).
genCode(P,R) :-         resetid,dynamic(cs/1),maplist(func,P,R),!.
