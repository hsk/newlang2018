:- module(linearScanRegAlloc,[linearScanRegAlloc/2]).
:- use_module([liveness]).
adr(A,A) :- ($_=A;\_=A;ptr(_,_)=A),!.
adr(A,N) :- lookup(A,N),!.
adr(A,N) :- retract(unused(N)),!,asserta(lookup(A,N)).
adr(A,N) :- retract(c(C)),C1 is C+8,assert(c(C1)),N=ptr(\rbp,-C1),asserta(lookup(A,N)).
adrs(A,A1) :- maplist(adr,A,A1).
prms(Ps)   :- regps(Regps),length(Regps,L),
              forall((nth0(I,Ps,P),nth0(I,Regps,R)),assert(lookup(P,R))),
              forall((nth0(I,Ps,P),I>=L,C is (I-L+2)*8),assert(lookup(P,ptr(\rbp,C)))).
getPush(Rs) :- findall(R,(regp2(R),once((live(A),lookup(A,R)))),Rs).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Rs))  :- adrs(B,B1),adr(C,C1),getPush(Rs).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(lineScan(code(C))).
code(C,(Mks,Rms),C_) :- forall(member(A,Rms),retract(live(A))),code(C,C_),
                        forall(member(A,Mks),asserta(live(A))),
                        forall((member(A,Rms),lookup(A,R),\_=R),asserta(unused(R))).
bb(L:BB,(Lives,Alives),L:BB1) :- forall(member(L1,Lives),assert(live(L1))),
                                 maplist(code,BB,Alives,BB1),retractall(live(_)).
func(N:Ps=BBs,BBsAlives,N:[]=[L:[enter(Size,Rs)|Cs]|BBs1]) :-
  forall(reg(R),assert(unused(R))),assert(c(0)),dynamic(lookup/2),prms(Ps),
  maplist(bb,BBs,BBsAlives,[L:Cs|BBs1]),retract(c(Size)),retractall(unused(_)),
  findall(R,(reg2(R),once(lookup(_,R))),Rs),retractall(lookup(_,_)).
linearScanRegAlloc(Fs,Fs_) :- liveness(Fs,Lives),maplist(func,Fs,Lives,Fs_),!.
