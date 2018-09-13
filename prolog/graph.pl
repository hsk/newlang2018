:- module(graph,[coloring/2,neighbors/2,liveness/2,gen_edges/2,kill/2]).
% Welsh-Powell-Algorithm
coloring(Ns,Cs) :- predsort(c_cmp,Ns,Ns_),foldl(c1,Ns_,([],0,Ns_),(Cs,_,_)),!.
c_cmp(>,_:_/L1,_:_/L2) :- L2>L1,!. c_cmp(<,_,_).
c1(A:_/_,(Cs,C,[_|I]),(Cs,C,I)) :- member(A:_,Cs),!.
c1(A:E/_,(Cs,C,[_|I]),(Cs1,C1,I)) :- C1 is C+1,foldl(c2(C1,E),I,[A:C1|Cs],Cs1),!.
c2(C,E,A:N/_,Cs,Cs) :- (member(A:_,Cs);member(A,E);member(V,N),member(V:C,Cs)),!.
c2(C,_,A:_/_,Cs,[A:C|Cs]).

neighbors(Es,Ns) :- foldl(n1,Es,[],Ns),!.
n1(A-B,Ns,[A:[B|As]/Al1,B:[A|Bs]/Bl1|Ns_]) :- subtract(Ns,[A:As/Al,B:Bs/Bl],Ns_),
                             (As=[],Al1=1;Al1 is Al+1),(Bs=[],Bl1=1;Bl1 is Bl+1).

liveness(G,R) :- maplist(l_func(G),G,G2),(G=G2->R=G;liveness(G2,R)).
l_func(G,(K,[inp=Inp,out=Out,block=Block,br=Br]),
         (K,[inp=In3,out=Ou2,block=Block,br=Br])) :-
    foldl(l_br(G),Br,(Out,Inp),(Ou1,In1)),reverse(Block,RBlock),
    ((Br \= [];Block=[]) -> (Ou2,In2)=(Ou1,In1)
    ; [(_,LInp)|_]=RBlock,union(LInp,Ou1,Ou2),union(LInp,In1,In2)),
    foldl(l_bb,RBlock,In2,In3).
l_br(G,B,(Out,Inp),(Ou2,In2))  :- member((B,V),G),member(inp=In1,V),
                                  union(In1,Out,Ou2),union(In1,Inp,In2).
l_bb((Out,Inp),In1,In3)        :- union(Inp,In1,In2),subtract(In2,Out,In3).

add(Es,[],Es).
add(Es,[X|Xs],Es2) :- foldl(addEs(X),Xs,Es,Es_), add(Es_,Xs,Es2).
addEs(X,X,Es,Es) :- member(X-_,Es);member(_-X,Es).
addEs(X,Y,Es,Es) :- member(X-Y,Es);member(Y-X,Es).
addEs(X,Y,Es,[X-Y|Es]).

gen_edges(G,G2) :- foldl(gen_edges_bb,G,[],G2),!.
gen_edges_bb((_,BB),Es,Es2) :-
  subtract(BB,[out=Out,block=Block],_),
  add(Es,Out,Es1),reverse(Block,RBlock),
  foldl(gen_edges_code,RBlock,(Out,Es1),(_,Es2)).
gen_edges_code((Out,Inp),(Live,Es),(Live3,Es3)) :-
  union(Out,Inp,OutInp),add(Es,OutInp,Es1),
  foldl(gen_edges3,Out,(Es1,Live),(Es2,Live2)),
  union(Inp,Live2,InpLive2),add(Es2,InpLive2,Es3),
  union(Inp,Live2,Live3).
gen_edges3(O,(Es,S),(Es1,S1)):- addEs(O,O,Es,Es1),subtract(S,[O],S1).

kill(G,R) :- maplist(kill_func,G,R).
kill_func((_,[inp=Inp,out=Out,block=Block|_]),(Inp,Kills)) :-
  reverse(Block,RBlock),foldl(kill_bb,RBlock,(Out,[]),(_,Kills)).
kill_bb((Out,Inp),(Lives,Kills),(Lives1,[(Out,Dies)|Kills])) :-
  subtract(Inp, Lives,Dies),subtract(Lives,Out,Lives1).
