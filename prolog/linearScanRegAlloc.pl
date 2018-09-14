:- module(linearScanRegAlloc,[linearScanRegAlloc/2]).
:- use_module([liveness]).

regs(['%r10','%r11','%rbx','%r12','%r13','%r14','%r15']).

adr(A,V) :- atom(A),nb_getval(m,M),member(A:V,M),!.
adr(A,A) :- atom(A),re_match('^[%$1-9]',A),!.
adr(A,N) :- atom(A),nb_getval(unused,[N|Regs]),!,nb_linkval(unused,Regs),
            nb_getval(m,M),nb_linkval(m,[A:N|M]).
adr(A,N) :- atom(A),!,nb_getval(counter,C),C1 is C-8,nb_linkval(counter,C1),
            format(atom(N),'~w(%rbp)',[C1]),
            nb_getval(m,M),nb_linkval(m,[A:N|M]).
adr(A,A).
adrs(A,A1) :- maplist(adr,A,A1).
getPush(Cs) :- nb_getval(lives,Lives),nb_getval(m,M),foldl(getPush1(M),Lives,[],Cs).
getPush1(M,A,Cs,[R|Cs]) :- member(A:R,M),member(R,['%r10','%r11']),\+member(R,Cs).
getPush1(_,_,_,Cs,Cs).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Cs))  :- getPush(Cs),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(label(A),label(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).

kill1(M,A) :- member(A:V,M),re_match('^%',V),!,
  nb_getval(unused,Regs),nb_linkval(unused,[V|Regs]).
kill1(_,_).
code1(Code,(Out,Kill),Code_) :-
  nb_getval(lives,Lives),subtract(Lives,Kill,Lives1),nb_linkval(lives,Lives1),
  code(Code,Code_),
  nb_getval(m,M),maplist(kill1(M),Kill),
  union(Lives1,Out,Lives2),nb_linkval(lives,Lives2).

bb((L,BB),(Lives,Kill),(L,BB1)) :-
  nb_linkval(lives,Lives),maplist(code1,BB,Kill,BB1).
func((N,BBs),(_,Kills),(N,Rs,[(N1,[enter(Size1,RRs)|Cs])|BBs1])) :-
  regs(Regs),nb_linkval(unused,Regs),
  nb_linkval(counter,0),nb_linkval(m,[]),
  maplist(bb,BBs,Kills,[(N1,Cs)|BBs1]),nb_getval(counter,Counter),
  nb_getval(m,M),
  include([R]>>member(_:R,M),['%rbx','%r12','%r13','%r14','%r15'],Rs),
  reverse(Rs,RRs),
  Size is floor((15-Counter)/16)*16,format(atom(Size1),'$~w',[Size]).

linearScanRegAlloc(Fs,Fs_) :- collect(Fs,Lives),maplist(func,Fs,Lives,Fs_).
