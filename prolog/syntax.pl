:- module(syntax,[syntax/2]).
:- op(1200,xfx,::=).
:- op(650,xfx,∈).
:- op(250,yf,*).
G∈{G}. G∈(G|_). G∈(_|G1):-G∈G1. G∈G.
syntax(S/[S2],AEs) :- compound_name_arguments(AEs,A,Es),syntax(S,A),syntax(S2,Es).
syntax(G,E):-G=..[O|Gs],E=..[O|Es],maplist(syntax,Gs,Es),!.
syntax(G,E):-(G::=Gs),!,G1∈Gs,syntax(G1,E),!.
syntax(i,I):-integer(I),!.
syntax(x,I):- atom(I),!.
syntax(E*,Ls) :- maplist(syntax(E),Ls).

e ::= i | x | e + e | e - e | x = e | x/[e*].
s ::= return(e) | if(e,s*,s*) | while(e,s*) | e.
d ::= x/[x*]=s* .
p ::= d* .

o  ::= addq | subq.
ae ::= i | x | bin(o,ae,ae) | mov(ae,x) | call(x,ae*).
as ::= ret(ae) | if(ae,as*,as*) | while(ae,as*) | ae.
af ::= x:x* = as* .
a  ::= af* .

cr ::= x | $i.
l  ::= x.
cc ::= bin(o,cr,cr,cr) | mov(cr,cr) | call(cr,cr*,cr) | bne(cr,l,l) | br(l) | ret(cr).
cf ::= x:x* = (l:cc*)* .
c  ::= cf* .

ri ::= i | -ri.
rr ::= \x | $i | ptr(\x,ri) | null.
rc ::= enter(ri,\x*) | bin(o,rr,rr,rr) | mov(rr,rr) | call(x,rr*,rr,\x*) | bne(rr,l,l) | br(l) | ret(rr).
rf ::= x:mov(rr,rr)* = (l:rc*)* .
r  ::= rf* .
