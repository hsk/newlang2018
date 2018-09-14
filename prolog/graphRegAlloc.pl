:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).

adr(A,V) :- atom(A),nb_getval(m,M),member(A:V,M),!.
adr(A,A) :- atom(A),re_match('^[%$1-9]',A),!.
adr(A,N) :- atom(A),!,nb_getval(counter,C),C1 is C-8,nb_linkval(counter,C1),
            format(atom(N),'~w(%rbp)',[C1]),
            nb_getval(m,M),nb_linkval(m,[A:N|M]).
adr(A,A).
adrs(A,A1) :- maplist(adr,A,A1).
argv([],Rs,[],Rs).
argv([A|As],[R|Rs],[A:R|M],Rs_)  :- argv(As,Rs,M,Rs_).
argv(As,[],M,[]) :- argv2(As,16,M).
argv2([],_,[]).
argv2([A|As],C,[A:R|M]) :- format(atom(R),'~w(%rbp)',[C]),
                           C8 is C+8,argv2(As,C8,M).
prms(Ps,Ps_) :- regp(Regp),argv(Ps,Regp,M,_Regs1),
                nb_linkval(m,M),maplist([_:A1,A1]>>!,M,Ps_).
getPush(Cs) :- nb_getval(lives,Lives),nb_getval(m,M),regp2(Rs),
               foldl(getPush1(M,Rs),Lives,[],Cs).
getPush1(M,Rs,A,Cs,[R|Cs]) :- member(A:R,M),member(R,Rs),\+member(R,Cs).
getPush1(_,_,_,Cs,Cs).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1,Cs))  :- getPush(Cs),adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(label(A),label(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
code(prms(Ps),prms(Ps_))            :- prms(Ps,Ps_).
code(C,_) :- writeln(error:lineScan;code(C)),halt(-1).
code1(Code,(Out,Kill),Code_) :-
  nb_getval(lives,Lives),subtract(Lives,Kill,Lives1),nb_linkval(lives,Lives1),
  code(Code,Code_),
  union(Lives1,Out,Lives2),nb_linkval(lives,Lives2).

bb((L,BB),(Lives,Kill),(L,BB1)) :-
  nb_linkval(lives,Lives),maplist(code1,BB,Kill,BB1).
func((N,BBs),(M1,Kills),(N,Rs,[(N1,[enter(Size1,RRs)|Cs])|BBs1])) :-
  nb_linkval(counter,0),nb_linkval(m,M1),
  maplist(bb,BBs,Kills,[(N1,Cs)|BBs1]),nb_getval(counter,Counter),
  nb_getval(m,M),
  regs2(Regs2),include([R]>>member(_:R,M),Regs2,Rs),reverse(Rs,RRs),
  Size is floor((15-Counter)/16)*16,format(atom(Size1),'$~w',[Size]).

alloc_func((Live,Kills),(M,Kills)) :-
  gen_edges(Live,Edges),neighbors(Edges,Ns),coloring(Ns,Cs),
  regs(Regs),findall(A:R,(member(A:I,Cs),nth1(I,Regs,R)),M).

regAlloc(Fs,Fs_) :-
  collect(Fs,Lives),maplist(alloc_func,Lives,Allocs),
  maplist(func,Fs,Allocs,Fs_).
