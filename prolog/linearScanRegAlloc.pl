:- module(linearScanRegAlloc,[linearScanRegAlloc/2]).
:- use_module([liveness]).
adr(A,A) :- ($_=A;\_=A;ptr(_,_)=A),!.
adr(A,N) :- nb_getval(m,M),member(A:N,M),!.
adr(A,N) :- nb_getval(unused,[N|Us]),!,nb_setval(unused,Us),
            nb_getval(m,M),nb_setval(m,[A:N|M]).
adr(A,N) :- nb_getval(size,C),C1 is C+8,nb_setval(size,C1),
            N=ptr(\rbp,-C1),nb_getval(m,M),nb_setval(m,[A:N|M]).
adrs(A,A1) :- maplist(adr,A,A1).
prms(Ps)   :- regp(Regp),length(Regp,L),
              findall(P:R,(nth0(I,Ps,P),nth0(I,Regp,R)),M),
              findall(P:R,(nth0(I,Ps,P),I>=L,C is (I-L+2)*8,R=ptr(\rbp,C)),M2),
              append(M,M2,M3),nb_getval(m,M4),union(M3,M4,M5),nb_setval(m,M5).
getPush(Cs) :- nb_getval(lives,Lives),nb_getval(m,M),regp2(Rs),
               findall(R,(member(A,Lives),member(A:R,M),member(R,Rs)),Cs1),
               list_to_set(Cs1,Cs).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Cs))  :- getPush(Cs),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- throw(lineScan(code(C))).
code1(Code,(Out,Kill),Code_) :-
  nb_getval(lives,Lives),subtract(Lives,Kill,Lives1),nb_setval(lives,Lives1),
  code(Code,Code_),
  union(Lives1,Out,Lives2),nb_setval(lives,Lives2),
  nb_getval(unused,Us1),nb_getval(m,M),
  findall(U,(member(A,Kill),member(A:U,M),\_=U),Us2,Us1),
  nb_setval(unused,Us2).
bb(L:BB,(Lives,Kill),L:BB1) :- nb_setval(lives,Lives),maplist(code1,BB,Kill,BB1).
func(N:Ps=BBs,(_,Kills),N:[]=[N1:[enter(Size,Rs)|Cs]|BBs1]) :-
  regs(Regs),nb_setval(unused,Regs),
  nb_setval(size,0),nb_setval(m,[]),prms(Ps),
  maplist(bb,BBs,Kills,[N1:Cs|BBs1]),nb_getval(size,Size),
  regs2(Regs2),nb_getval(m,M),findall(R,(member(R,Regs2),member(_:R,M)),Rs).
linearScanRegAlloc(Fs,Fs_) :- liveness(Fs,Lives),maplist(func,Fs,Lives,Fs_),!.
