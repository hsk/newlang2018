:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).
adr(A,A) :- (\_=A;$_=A;ptr(_,_)=A),!.
adr(A,N) :- m(A:N),!.
adr(A,N) :- retract(c(C)),C1 is C+8,assert(c(C1)),N=ptr(\rbp,-C1),assert(m(A:N)).
adrs(A,A1) :- maplist(adr,A,A1).
getPush(Cs) :- regp2(Rs),findall(R,(live(A),m(A:R),member(R,Rs)),Cs1),list_to_set(Cs1,Cs).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Cs))  :- getPush(Cs),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(regAlloc(code(C))).
code1(Code,(Out,Kill),Code_) :-
  forall(member(K,Kill),retract(live(K))),code(Code,Code_),!,
  forall(member(O,Out),asserta(live(O))).
bb(L:BB,(Lives,Diffs),L:BB1) :-
  forall(member(L1,Lives),assert(live(L1))),
  maplist(code1,BB,Diffs,BB1),retractall(live(_)).
func(N:_=BBs,(M1,Alloc),N:[]=[N1:[enter(Size,Rs)|Cs]|BBs1]) :-
  assert(c(0)),forall(member(V,M1),assert(m(V))),
  maplist(bb,BBs,Alloc,[N1:Cs|BBs1]),retract(c(Size)),
  regs2(Regs2),findall(R,(member(R,Regs2),retract(m(_:R))),Rs),retractall(m(_)).
regAlloc(Fs,Fs_) :- dynamic(live/1),alloc(Fs,Allocs),maplist(func,Fs,Allocs,Fs_),!.
