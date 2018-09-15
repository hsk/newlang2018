:- module(graphRegAlloc,[regAlloc/2]).
:- use_module([liveness,graph]).

adr(A,V) :- atom(A),nb_getval(m,M),member(A:V,M),!.
adr(A,A) :- atom(A),re_match('^[%$1-9]',A),!.
adr(A,N) :- atom(A),!,nb_getval(c,C),C1 is C-8,nb_linkval(c,C1),
            format(atom(N),'~w(%rbp)',[C1]),
            nb_getval(m,M),nb_linkval(m,[A:N|M]).
adr(A,A).
adrs(A,A1) :- maplist(adr,A,A1).
prms(Ps,Ps_) :- regp(Regp),length(Regp,L),
                findall(P:R,(nth0(I,Ps,P),nth0(I,Regp,R)),M),
                findall(P:R,(nth0(I,Ps,P),I>=L,C is (I-L+2)*8,atom_concat(C,'(%rbp)',R)),M2),
                append(M,M2,M3),nb_getval(m,M4),union(M3,M4,M5),
                nb_linkval(m,M5),maplist([_:A1,A1]>>!,M3,Ps_).
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
  nb_getval(lives,Lives),subtract(Lives,Kill,Lives1),nb_linkval(lives,Lives1),
  code(Code,Code_),!,
  union(Lives1,Out,Lives2),nb_linkval(lives,Lives2).

bb((L,BB),(Lives,Kill),(L,BB1)) :-
  nb_linkval(lives,Lives),maplist(code1,BB,Kill,BB1).
func((N,_,BBs),(M1,Kills),(N,[],[(N1,[enter(Size,Rs)|Cs])|BBs1])) :-
  nb_linkval(c,0),nb_linkval(m,M1),
  maplist(bb,BBs,Kills,[(N1,Cs)|BBs1]),nb_getval(c,Size),
  nb_getval(m,M),
  regs2(Regs2),include([R]>>member(_:R,M),Regs2,Rs).

alloc_m(A:I,(Regs,I2R,M),(Regs,I2R,M2)) :- member(I:R,I2R),!,(member(A:_,M)->M2=M;M2=[A:R|M]).
alloc_m(A:I,(Regs,I2R,M),(Regs,[I:R|I2R],M)) :- member(A:R,M),!.
alloc_m(A:I,([R|Regs],I2R,M),(Regs,[I:R|I2R],[A:R|M])) :- !.
alloc_m(_,([],I2R,M),([],I2R,M)).
alloc_func((_,Ps,_),(Live,Kills),(M1,Kills)) :-
  gen_edges(Live,Edges),neighbors(Edges,Ns),coloring(Ns,Cs),
  nb_setval(m,[]),
  (prms(Ps,Ps_),!),maplist([P,A,P:A,I:A]>>(member(P:I,Cs),!),Ps,Ps_,M,I2R),
  regp(Regp),subtract(Regp,Ps_,Regp2),regs(Regs),union(Regs,Regp2,Regs2),
  foldl(alloc_m,Cs,(Regs2,I2R,M),(_,_,M1)).
alloc(Fs,Allocs) :- liveness(Fs,Lives),maplist(alloc_func,Fs,Lives,Allocs).

regAlloc(Fs,Fs_) :- alloc(Fs,Allocs),maplist(func,Fs,Allocs,Fs_).
