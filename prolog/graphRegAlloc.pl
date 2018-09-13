:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).

regs(['%r10','%r11','%r12','%r13','%r14','%r15']).
alloc_func((Live,Kills),(Cs,Kills)) :-
  gen_edges(Live,Edges),neighbors(Edges,Ns),coloring(Ns,Cs).
alloc(Prog,R) :- collect(Prog,Lives),maplist(alloc_func,Lives,R).

adr(A,V) :- atom(A),nb_getval(m,M),member(A:V,M),!.
adr(A,A) :- atom(A),re_match('^[%$]',A),!.
adr(A,N) :- atom(A),nb_getval(cols,Cols),member(A:I,Cols),
            regs(Regs),nth1(I,Regs,N),!,nb_getval(m,M),nb_linkval(m,[A:N|M]).
adr(A,N) :- atom(A),!,nb_getval(counter,C),C1 is C-8,nb_linkval(counter,C1),
            format(atom(N),'~w(%rbp)',[C1]),
            nb_getval(m,M),nb_linkval(m,[A:N|M]).
adr(A,A).
adrs(A,A1) :- maplist(adr,A,A1).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1))     :- adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(label(A),label(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code1(Code,(Out,Kill),Code_) :-
  nb_getval(lives,Lives),subtract(Lives,Kill,Lives1),nb_linkval(lives,Lives1),
  code(Code,Code_),
  nb_getval(lives,Lives2),union(Lives2,Out,Lives3),nb_linkval(lives,Lives3).

bb((L,BB),(Lives,Kill),(L,BB1)) :-
  nb_linkval(lives,Lives),maplist(code1,BB,Kill,BB1).
func((N,BBs),(Cols,Kills),(N,[(N1,[sub(Size1,'%rsp')|Cs])|BBs1])) :-
  nb_linkval(cols,Cols),nb_linkval(counter,0),nb_linkval(m,[]),
  maplist(bb,BBs,Kills,[(N1,Cs)|BBs1]),nb_getval(counter,Counter),
  Size is floor((15-Counter)/16)*16,format(atom(Size1),'$~w',[Size]).

regAlloc(Fs,Fs_) :- alloc(Fs,Allocs),maplist(func,Fs,Allocs,Fs_).
