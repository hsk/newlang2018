:- module(liveness,[liveness/2,regps/1,regs/1,regps2/1,regs2/1,regp/1,regp2/1,reg/1,reg2/1]).
allregs([\rdi,\rsi,\rdx,\rcx,\r8,\r9,\r10,\r11,\rbx,\r12,\r13,\r14,\r15]).
:- allregs(Rs),findall(R,(between(1,6,I),nth1(I,Rs,R),assert(regp(R))),Rs_),assert(regps(Rs_)).
:- allregs(Rs),findall(R,(between(7,13,I),nth1(I,Rs,R),assert(reg(R))),Rs_),assert(regs(Rs_)).
:- allregs(Rs),findall(R,(between(1,8,I),nth1(I,Rs,R),assert(regp2(R))),Rs_),assert(regps2(Rs_)).
:- allregs(Rs),findall(R,(between(9,13,I),nth1(I,Rs,R),assert(reg2(R))),Rs_),assert(regs2(Rs_)).
live_br(G,B,I>O,I2>O2) :- member(B:V,G),member(i=I1,V),union(I1,O,O2),union(I1,I,I2).
live_code(I>O,I1,I3)   :- union(I,I1,I2),subtract(I2,O,I3).
live_bb(G,L:[i=_,o=_,bb=BB,br=Br],L:[i=I3,o=O,bb=BB,br=Br]) :-
  reverse(BB,RBB),foldl(live_br(G),Br,[]>[],IO>O),foldl(live_code,RBB,IO,I3).
live(G,R) :- maplist(live_bb(G),G,G2),(G=G2->R=G;live(G2,R)).

alive_code(I>O,(Lives,Alives),(Lives2,[(Mks,Rms)|Alives])) :-
  intersection(O,Lives,Mks),subtract(Lives,Mks,Lives1),
  subtract(I,Lives1,Rms),union(I,Lives1,Lives2).
alive_bb(_:[i=I,o=O,bb=BB|_],(I,Alives)) :-
  reverse(BB,RBB),foldl(alive_code,RBB,(O,[]),(I,Alives)).
alive(G,R) :- maplist(alive_bb,G,R).

io_imm(Is,Is_) :- findall(R,(member(R,Is),atom(R),($_\=R,\_\=R)),Is_).
io_code(mov(I,R),Is>[R]) :- io_imm([I],Is).
io_code(bin(_,I1,I2,R),Is>[R]) :- io_imm([I1,I2],Is).
io_code(ret(I),Is>[]) :- io_imm([I],Is).
io_code(br(_),[]>[]).
io_code(bne(R,_,_),Is>[]) :- io_imm([R],Is).
io_code(call(_,Is,R),Is_>[R]) :- io_imm(Is,Is_).
io_code(Ir,_) :- throw(liveness(io_code(code(Ir)))).
io_br(br(L),[L]).
io_br(bne(_,L1,L2),[L1,L2]).
io_bb_br(BB,Br) :- last(BB,B),io_br(B,Br).
io_bb_br(_,[]).
io_bb(L:BB,L:[i=[],o=[],bb=BBIO,br=Br]) :- maplist(io_code,BB,BBIO),io_bb_br(BB,Br).
io_bbs(BBs,BBIOs) :- maplist(io_bb,BBs,BBIOs).
func(_:_=BBs,Alives) :- io_bbs(BBs,BBIOs),live(BBIOs,Live),alive(Live,Alives),!.
liveness(Funcs,R) :- maplist(func,Funcs,R).
