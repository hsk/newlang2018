:- module(liveness,[liveness/2,regp/1,regs/1,regp2/1,regs2/1]).

regp([\rdi,\rsi,\rdx,\rcx,\r8,\r9]).
regs([\r10,\r11,\rbx,\r12,\r13,\r14,\r15]).
regp2([\rdi,\rsi,\rdx,\rcx,\r8,\r9,\r10,\r11]).
regs2([\rbx,\r12,\r13,\r14,\r15]).

live_br(G,B,(O,I),(O2,I2)) :- member(B:V,G),member(i=I1,V),union(I1,O,O2),union(I1,I,I2).
live_code((O,I),I1,I3)     :- union(I,I1,I2),subtract(I2,O,I3).
live_bb(G,L:[i=I,o=_,bb=BB,br=Br],L:[i=I3,o=O1,bb=BB,br=Br]) :-
    foldl(live_br(G),Br,([],I),(O1,I1)),reverse(BB,RBB),
    ((Br \= [];BB=[]) -> I2=I1; [(_,LI)|_]=RBB,union(LI,I1,I2)),
    foldl(live_code,RBB,I2,I3).
live(G,R) :- maplist(live_bb(G),G,G2),(G=G2->R=G;live(G2,R)).

kill_code((O,I),(Lives,Kills),(Lives2,[(O2,Dies)|Kills])) :-
  union(I,O,IO),subtract(IO,Lives,Dies), % 最後の出現。ここで死んだ
  subtract(O,Dies,O2), % 一度も使われない変数
  union(I,Lives,Lives1), % 入力あったので生きている
  subtract(Lives1,O,Lives2). % 生まれる前は生きてない
kill_bb(_:[i=I,o=O,bb=BB|_],(I,Kills)) :-
  reverse(BB,RBB),foldl(kill_code,RBB,(O,[]),(_,Kills)).
kill(G,R) :- maplist(kill_bb,G,R).

io_imm(Is,Is_) :- findall(R,(member(R,Is),atom(R),($_\=R,\_\=R)),Is_).
io_code(mov(I,R),([R],Is)) :- io_imm([I],Is).
io_code(bin(_,I1,I2,R),([R],Is)) :- io_imm([I1,I2],Is).
io_code(ret(I),([],Is)) :- io_imm([I],Is).
io_code(br(_),([],[])).
io_code(bne(R,_,_),([],Is)) :- io_imm([R],Is).
io_code(call(_,Is,R),([R],Is_)) :- io_imm(Is,Is_).
io_code(Ir,_) :- writeln(error:liveness(io_code(code(Ir)))),halt(-1).
io_br(br(L),[L]).
io_br(bne(_,L1,L2),[L1,L2]).
io_bb_br(BB,Br) :- last(BB,B),io_br(B,Br).
io_bb_br(_,[]).
io_bb(L:BB,L:[i=[],o=[],bb=BBIO,br=Br]) :- maplist(io_code,BB,BBIO),io_bb_br(BB,Br).
io_bbs(BBs,BBIOs) :- maplist(io_bb,BBs,BBIOs).
func(_:_=BBs,(Live,Kills)) :- io_bbs(BBs,BBIOs),live(BBIOs,Live),kill(Live,Kills),!.
liveness(Funcs,R) :- maplist(func,Funcs,R).
