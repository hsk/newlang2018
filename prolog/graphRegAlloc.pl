:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).
adr(A,A) :- (\_=A;$_=A;ptr(_,_)=A),!.
adr(A,N) :- lookup(A,N),!.
adr(A,N) :- (live(A);live1(A)),retract(c(C)),C1 is C-8,
            assert(c(C1)),N=ptr(\rbp,C1),assert(lookup(A,N)).
adr(_,null).
adrs(A,A1) :- maplist(adr,A,A1).
getPush(Ps) :- setof(R,(regp2(R),live(A),lookup(A,R)),Ps);Ps=[].
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Ps))  :- getPush(Ps),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(regAlloc(code(C))).
code1(Code,(Mks,Rms),Code_) :-
  forall(member(R,Mks),asserta(live1(R))),
  forall(member(R,Rms),retract(live(R))),code(Code,Code_),!,
  forall(member(R,Mks),asserta(live(R))),retractall(live1(R)).
bb(L:BB,(Lives,BBAlives),L:BB1) :-
  forall(member(L1,Lives),assert(live(L1))),
  maplist(code1,BB,BBAlives,BB1),retractall(live(_)).
func(N:_=BBs,(M1,BBsAlives),N:[]=[N1:[enter(-Size,Rs)|Cs]|BBs1]) :-
  assert(c(0)),forall(member(A:R,M1),assert(lookup(A,R))),
  maplist(bb,BBs,BBsAlives,[N1:Cs|BBs1]),retract(c(Size)),
  (setof(R,(reg2(R),lookup(_,R)),Rs);Rs=[]),retractall(lookup(_,_)).
regAlloc(Fs,Fs_) :- dynamic(live/1),alloc(Fs,Allocs),maplist(func,Fs,Allocs,Fs_),!.
