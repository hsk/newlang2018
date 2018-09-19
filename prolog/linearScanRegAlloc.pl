:- module(linearScanRegAlloc,[linearScanRegAlloc/2]).
:- use_module([liveness]).
adr(A,A) :- ($_=A;\_=A;ptr(_,_)=A),!.
adr(A,N) :- m(A:N),!.
adr(A,N) :- retract(unused(N)),!,asserta(m(A:N)).
adr(A,N) :- retract(c(C)),C1 is C+8,assert(c(C1)),N=ptr(\rbp,-C1),asserta(m(A:N)).
adrs(A,A1) :- maplist(adr,A,A1).
prms(Ps)   :- regp(Regp),length(Regp,L),
              forall((nth0(I,Ps,P),nth0(I,Regp,R)),assert(m(P:R))),
              forall((nth0(I,Ps,P),I>=L,C is (I-L+2)*8),assert(m(P:ptr(\rbp,C)))).
getPush(Cs) :- setof(R,(rp2(R),live(A),m(A:R)),Cs);Cs=[].
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Cs))  :- getPush(Cs),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(lineScan(code(C))).
code1(Code,(Mks,Rms),Code_) :-
  forall(member(R,Rms),retract(live(R))),code(Code,Code_),
  forall(member(R,Mks),asserta(live(R))),
  forall((member(R,Rms),m(R:U),\_=U),asserta(unused(U))).
bb(L:BB,(Lives,BBAlives),L:BB1) :-
  forall(member(L1,Lives),assert(live(L1))),
  maplist(code1,BB,BBAlives,BB1),retractall(live(_)).
func(N:Ps=BBs,BBsAlives,N:[]=[N1:[enter(Size,Rs)|Cs]|BBs1]) :-
  regs(Regs),forall(member(R,Regs),assert(unused(R))),
  assert(c(0)),retractall(m(_)),prms(Ps),
  maplist(bb,BBs,BBsAlives,[N1:Cs|BBs1]),retract(c(Size)),retractall(unused(_)),
  (setof(R,(r2(R),m(_:R)),Rs);Rs=[]),retractall(m(_)).
linearScanRegAlloc(Fs,Fs_) :- liveness(Fs,Lives),maplist(func,Fs,Lives,Fs_),!.
