:- module(memAlloc,[memAlloc/2]).
regps([\rdi,\rsi,\rdx,\rcx,\r8,\r9]).
adr(A,A) :- ($_=A;\_=A;ptr(_,_)=A),!.
adr(A,N) :- m(A:N),!.
adr(A,N) :- retract(c(C)),C1 is C+8,assert(c(C1)),N=ptr(\rbp,-C1),asserta(m(A:N)).
adrs(A,A1) :- maplist(adr,A,A1).
prms(Ps,Ps_) :- regps(Regps),length(Regps,L),retractall(m(_)),
                findall(P:R,(nth0(I,Ps,P),I>=L,C is (I-L+2)*8,R=ptr(\rbp,C),assert(m(P:R))),M2),
                findall(P:R,(nth0(I,Ps,P),nth0(I,Regps,R)),M),append(M,M2,M3),
                forall(member(A:_,M),adr(A,_)),maplist(prms1,M3,Ps_).
prms1(A:R,mov(R,V)) :- m(A:V).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,[]))  :- adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(memAlloc(code(C))).
bb(L:BB,L:BB1) :- maplist(code,BB,BB1).
func(N:Ps=BBs,N:Ps_=[N1:[enter(Size,[])|Cs]|BBs1]) :-
  assert(c(0)),prms(Ps,Ps_),maplist(bb,BBs,[N1:Cs|BBs1]),retract(c(Size)).
memAlloc(Fs,Fs_) :- maplist(func,Fs,Fs_),!.
