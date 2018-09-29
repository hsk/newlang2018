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

addEs(X,Y) :- es(X-Y);es(Y-X);assert(es(X-Y)).
add(Xs,Ys) :- forall((member(X,Xs),member(Y,Ys),X\=Y),addEs(X,Y)).
gen_edges_code((Rms,Mks),I1,I3) :- add(I1,Mks),add(Mks,Mks),subtract(I1,Rms,I2),union(I2,Mks,I3).
gen_edges_bb((I1,Alives)) :- add(I1,I1),foldl(gen_edges_code,Alives,I1,_).
gen_edges(G,Es) :- dynamic(es/1),maplist(gen_edges_bb,G),findall(E,retract(es(E)),Es).

:- use_module(liveness).
prms(Ps,Ps_) :- regps(Regps),length(Regps,L),
                findall(P:R,(nth0(I,Ps,P),nth0(I,Regps,R)),M),
                findall(P:R,(nth0(I,Ps,P),I>=L,C is (I-L+2)*8,R=ptr(\rbp,C)),M2),
                append(M,M2,M3),maplist(prms1,M3,Ps_).
prms1(_:A1,A1).
alloc_m(A:I,(Regs,I2R,M),(Regs,I2R,M2)) :- member(I:R,I2R),!,(member(A:_,M)->M2=M;M2=[A:R|M]).
alloc_m(A:I,(Regs,I2R,M),(Regs,[I:R|I2R],M)) :- member(A:R,M),!.
alloc_m(A:I,([R|Regs],I2R,M),(Regs,[I:R|I2R],[A:R|M])) :- !.
alloc_m(_,([],I2R,M),([],I2R,M)).
alloc_func1(Cs,P,A,P:A,I:A) :- member(P:I,Cs).
alloc_func(_:Ps=_,Alives,(M1,Alives)) :-
  gen_edges(Alives,Edges),neighbors(Edges,Ns),coloring(Ns,Cs),
  (prms(Ps,Ps_),!),maplist(alloc_func1(Cs),Ps,Ps_,M,I2R),
  regps(Regps),subtract(Regps,Ps_,Regps2),regs(Regs),union(Regs,Regps2,Regs2),
  foldl(alloc_m,Cs,(Regs2,I2R,M),(_,_,M1)).
alloc(Fs,Allocs) :- liveness(Fs,Lives),maplist(alloc_func,Fs,Lives,Allocs).
