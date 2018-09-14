:- module(liveness,[collect/2]).

liveness(G,R) :- maplist(l_func(G),G,G2),(G=G2->R=G;liveness(G2,R)).
l_func(G,(K,[inp=Inp,out=_,block=Block,br=Br]),
         (K,[inp=In3,out=Ou1,block=Block,br=Br])) :-
    foldl(l_br(G),Br,([],Inp),(Ou1,In1)),reverse(Block,RBlock),
    ((Br \= [];Block=[]) -> In2=In1
    ; [(_,LInp)|_]=RBlock,union(LInp,In1,In2)),
    foldl(l_bb,RBlock,In2,In3).
l_br(G,B,(Out,Inp),(Ou2,In2))  :- member((B,V),G),member(inp=In1,V),
                                  union(In1,Out,Ou2),union(In1,Inp,In2).
l_bb((Out,Inp),In1,In3)        :- union(Inp,In1,In2),subtract(In2,Out,In3).

kill(G,R) :- maplist(kill_func,G,R).
kill_func((_,[inp=Inp,out=Out,block=Block|_]),(Inp,Kills)) :-
  reverse(Block,RBlock),foldl(kill_bb,RBlock,(Out,[]),(_,Kills)).
kill_bb((Out,Inp),(Lives,Kills),(Lives2,[(Ou2,Dies)|Kills])) :-
  union(Inp,Out,InOu),subtract(InOu,Lives,Dies), % 最後の出現。ここで死んだ
  subtract(Out,Dies,Ou2), % 一度も使われない変数
  union(Inp,Lives,Lives1), % 入力あったので生きている
  subtract(Lives1,Out,Lives2), % 生まれる前は生きてない
  !.

imm1(R) :- atom(R),\+re_match('^[$%]',R).
imm(Is,Is_) :- include(imm1,Is,Is_).
code(mov(I,R),([R],Is)) :- imm([I],Is).
code(bin(_,I1,I2,R),([R],Is)) :- imm([I1,I2],Is).
code(ret(I),([],Is)) :- imm([I],Is).
code(br(_),([],[])).
code(bne(R,_,_),([],Is)) :- imm([R],Is).
code(call(_,Is,R),([R],Is_)) :- imm(Is,Is_).
code(Ir,_) :- writeln(error:code(Ir)),halt.
br(ret(_),[]).
br(br(L),[L]).
br(bne(_,L1,L2),[L1,L2]).
bb((K,[]),(K,[inp=[],out=[],block=[],br=[]])).
bb((K,BB),(K,[inp=[],out=[],block=Block,br=Br])) :-
  maplist(code,BB,Block),last(BB,B),br(B,Br).
bbs(BBs,BBIOs) :- maplist(bb,BBs,BBIOs).
func((_,BBs),(Live,Kills)) :-
  bbs(BBs,BBIOs),liveness(BBIOs,Live),kill(Live,Kills),!.
collect(Funcs,R) :- maplist(func,Funcs,R).
