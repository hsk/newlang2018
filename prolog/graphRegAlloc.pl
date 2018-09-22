:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).
adr(A,A) :- (\_=A;$_=A;ptr(_,_)=A),!.
adr(A,N) :- lookup(A,N),!.
adr(A,N) :- (live(A);live1(A)),retract(c(C)),C1 is C-8,
            assert(c(C1)),N=ptr(\rbp,C1),assert(lookup(A,N)).
adr(_,null).
adrs(A,A1) :- maplist(adr,A,A1).
getPush(Ps) :- findall(R,(regp2(R),once((live(A),lookup(A,R)))),Ps).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Ps))  :- getPush(Ps),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(regAlloc(code(C))).
code(C,(Mks,Rms),C_) :- forall(member(R,Mks),asserta(live1(R))),
                        forall(member(R,Rms),retract(live(R))),code(C,C_),!,
                        forall(member(R,Mks),asserta(live(R))),retractall(live1(_)).
bb(L:BB,(Lives,Alives),L:BB1) :- forall(member(L1,Lives),assert(live(L1))),
                                 maplist(code,BB,Alives,BB1),retractall(live(_)).
func(N:_=BBs,(M1,BBsAlives),N:[]=[N1:[enter(-Size,Rs)|Cs]|BBs1]) :-
  assert(c(0)),forall(member(A:R,M1),assert(lookup(A,R))),
  maplist(bb,BBs,BBsAlives,[N1:Cs|BBs1]),retract(c(Size)),
  findall(R,(reg2(R),once(lookup(_,R))),Rs),retractall(lookup(_,_)).
regAlloc(Fs,Fs_) :- dynamic(live/1),alloc(Fs,Allocs),maplist(func,Fs,Allocs,Fs_),!.
