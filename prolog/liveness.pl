:- module(liveness,[collect/2]).
:- use_module(graph).

last([X],X).
last([_|Xs],Y) :- last(Xs,Y).

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
