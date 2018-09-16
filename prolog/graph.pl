:- module(graph,[alloc/2]).
% Welsh-Powell-Algorithm
c2(C,E,A:N/_,Cs,Cs) :- (member(A:_,Cs);member(A,E);member(V,N),member(V:C,Cs)),!.
c2(C,_,A:_/_,Cs,[A:C|Cs]).
c1(A:_/_,(Cs,C,[_|I]),(Cs,C,I)) :- member(A:_,Cs),!.
c1(A:E/_,(Cs,C,[_|I]),(Cs1,C1,I)) :- C1 is C+1,foldl(c2(C1,E),I,[A:C1|Cs],Cs1),!.
c_cmp(>,_:_/L1,_:_/L2) :- L2>L1,!. c_cmp(<,_,_).
coloring(Ns,Cs) :- predsort(c_cmp,Ns,Ns_),foldl(c1,Ns_,([],0,Ns_),(Cs,_,_)),!.

n1(A-B,Ns,[A:[B|As]/Al1,B:[A|Bs]/Bl1|Ns_]) :- subtract(Ns,[A:As/Al,B:Bs/Bl],Ns_),
                             (As=[],Al1=1;Al1 is Al+1),(Bs=[],Bl1=1;Bl1 is Bl+1).
neighbors(Es,Ns) :- foldl(n1,Es,[],Ns),!.

addEs(X,X,Es,Es) :- member(X-_,Es);member(_-X,Es).
addEs(X,Y,Es,Es) :- member(X-Y,Es);member(Y-X,Es).
addEs(X,Y,Es,[X-Y|Es]).
add(Es,[],Es).
add(Es,[X|Xs],Es2) :- foldl(addEs(X),Xs,Es,Es_), add(Es_,Xs,Es2).
gen_edges3(O,(Es,S),(Es1,S1)):- addEs(O,O,Es,Es1),subtract(S,[O],S1).
gen_edges_code((Out,Inp),(Live,Es),(Live3,Es3)) :-
  union(Out,Inp,OutInp),add(Es,OutInp,Es1),
  foldl(gen_edges3,Out,(Es1,Live),(Es2,Live2)),
  union(Inp,Live2,InpLive2),add(Es2,InpLive2,Es3),
  union(Inp,Live2,Live3).
gen_edges_bb((_,BB),Es,Es2) :-
  subtract(BB,[o=Out,bb=Block],_),
  add(Es,Out,Es1),reverse(Block,RBlock),
  foldl(gen_edges_code,RBlock,(Out,Es1),(_,Es2)).
gen_edges(G,G2) :- foldl(gen_edges_bb,G,[],G2),!.

:- use_module(liveness).
prms(Ps,Ps_) :- regp(Regp),length(Regp,L),
                findall(P:R,(nth0(I,Ps,P),nth0(I,Regp,R)),M),
                findall(P:R,(nth0(I,Ps,P),I>=L,C is (I-L+2)*8,atom_concat(C,'(%rbp)',R)),M2),
                append(M,M2,M3),maplist([_:A1,A1]>>!,M3,Ps_).
alloc_m(A:I,(Regs,I2R,M),(Regs,I2R,M2)) :- member(I:R,I2R),!,(member(A:_,M)->M2=M;M2=[A:R|M]).
alloc_m(A:I,(Regs,I2R,M),(Regs,[I:R|I2R],M)) :- member(A:R,M),!.
alloc_m(A:I,([R|Regs],I2R,M),(Regs,[I:R|I2R],[A:R|M])) :- !.
alloc_m(_,([],I2R,M),([],I2R,M)).
alloc_func((_,Ps,_),(Live,Kills),(M1,Kills)) :-
  gen_edges(Live,Edges),neighbors(Edges,Ns),coloring(Ns,Cs),
  (prms(Ps,Ps_),!),maplist([P,A,P:A,I:A]>>(member(P:I,Cs),!),Ps,Ps_,M,I2R),
  regp(Regp),subtract(Regp,Ps_,Regp2),regs(Regs),union(Regs,Regp2,Regs2),
  foldl(alloc_m,Cs,(Regs2,I2R,M),(_,_,M1)).
alloc(Fs,Allocs) :- liveness(Fs,Lives),maplist(alloc_func,Fs,Lives,Allocs).
