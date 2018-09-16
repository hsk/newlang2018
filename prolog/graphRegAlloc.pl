:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).

adr(A,A) :- re_match('^[%$1-9]',A),!.
adr(A,N) :- nb_getval(m,M),member(A:N,M),!.
adr(A,N) :- nb_getval(c,C),C1 is C-8,nb_setval(c,C1),
            format(atom(N),'~w(%rbp)',[C1]),
            nb_getval(m,M),nb_setval(m,[A:N|M]).
adrs(A,A1) :- maplist(adr,A,A1).
getPush(Cs)  :- nb_getval(lives,Lives),nb_getval(m,M),regp2(Rs),
                findall(R,(member(A,Lives),member(A:R,M),member(R,Rs)),Cs1),
                list_to_set(Cs1,Cs).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Cs))  :- getPush(Cs),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(label(A),label(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(C,_) :- writeln(error:regAlloc;code(C)),halt(-1).
code1(Code,(Out,Kill),Code_) :-
  nb_getval(lives,Lives),subtract(Lives,Kill,Lives1),nb_setval(lives,Lives1),
  code(Code,Code_),!,
  union(Lives1,Out,Lives2),nb_setval(lives,Lives2).
bb((L,BB),(Lives,Kill),(L,BB1)) :-
  nb_setval(lives,Lives),maplist(code1,BB,Kill,BB1).
func((N,_,BBs),(M1,Kills),(N,[],[(N1,[enter(Size,Rs)|Cs])|BBs1])) :-
  nb_setval(c,0),nb_setval(m,M1),
  maplist(bb,BBs,Kills,[(N1,Cs)|BBs1]),nb_getval(c,Size),
  nb_getval(m,M),regs2(Regs2),include([R]>>member(_:R,M),Regs2,Rs).
regAlloc(Fs,Fs_) :- alloc(Fs,Allocs),maplist(func,Fs,Allocs,Fs_).
