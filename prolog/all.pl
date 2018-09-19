% parse
pExpr(I,I) :- integer(I),!.
pExpr(A,A) :- atom(A),!.
pExpr(E1+E2,bin(addq,E1_,E2_)) :- pExpr(E1,E1_),pExpr(E2,E2_).
pExpr(E1-E2,bin(subq,E1_,E2_)) :- pExpr(E1,E1_),pExpr(E2,E2_).
pExpr(A=E,mov(E_,A)) :- atom(A),pExpr(E,E_).
pExpr(AEs,call(A,Es_)) :- compound_name_arguments(AEs,A,Es),maplist(pExpr,Es,Es_).
pExpr(E,_) :- throw(main(pExpr(E))).
pStmt(return(E),ret(E_)) :- pExpr(E,E_).
pStmt(if(E,S1,S2),if(E_,S1_,S2_)) :- pExpr(E,E_),maplist(pStmt,S1,S1_),maplist(pStmt,S2,S2_).
pStmt(while(E,S),while(E_,S_)) :- pExpr(E,E_),maplist(pStmt,S,S_).
pStmt(S,S_) :- pExpr(S,S_).
pFunc(NP=B,N:P=B_) :- compound_name_arguments(NP,N,P),maplist(pStmt,B,B_).
parse(Fs,Fs_) :- maplist(pFunc,Fs,Fs_),!.
parseFile(File,Fs_) :- read_file_to_terms(File,Fs,[]),parse(Fs,Fs_).

% genCode
resetid     :- retractall(id(_)),assert(id(0)).
genid(S,A)  :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'.~w~w',[S,C]).
label(L) :- (commit;true),assert(l(L)).
add(C) :- c(ret(_));assert(c(C)).
commit :- retract(l(L)),findall(C,retract(c(C)),Cs),assert(bb(L:Cs)).
commit(BBs) :- add(ret($0)),commit,findall(BB,retract(bb(BB)),BBs).

gExpr(bin(Op,A,B),R) :- genid(r,R),gExpr(A,A1),gExpr(B,B1),add(bin(Op,A1,B1,R)).
gExpr(mov(A,R),R) :-    gExpr(A,R1),add(mov(R1,R)).
gExpr(call(A,B),R) :-   genid(r,R),maplist(gExpr,B,Rs),add(call(A,Rs,R)).
gExpr(R,R) :-           atom(R),!.
gExpr(I,$I) :-          integer(I),!.
gExpr(E,_) :-           throw(genCode(gExpr(E))).
gStmt(if(A,C,D)) :-     genid(then,Then),genid(else,Else),
                        gExpr(A,R1),add(bne(R1,Then,Else)),genid(cont,Cont),
                        label(Then),gStmt(C),add(br(Cont)),
                        label(Else),gStmt(D),add(br(Cont)),label(Cont).
gStmt(while(A,B)) :-    genid(while,While),genid(then,Then),genid(cont,Cont),
                        label(While),gExpr(A,R1),add(bne(R1,Then,Cont)),
                        label(Then),gStmt(B),add(br(While)),label(Cont).
gStmt(ret(E)) :-        gExpr(E,R),add(ret(R)).
gStmt(B) :-             is_list(B),!,forall(member(S,B),gStmt(S)).
gStmt(E) :-             gExpr(E,_).
gFunc(N:A=B,N:A=BBs) :- genid(enter,Enter),label(Enter),gStmt(B),commit(BBs).
genCode(P,R) :-         resetid,dynamic(c/1),maplist(gFunc,P,R),!.

% mem alloc
regps([\rdi,\rsi,\rdx,\rcx,\r8,\r9]).
adr(A,A) :- ($_=A;\_=A;ptr(_,_)=A),!.
adr(A,N) :- m(A:N),!.
adr(A,N) :- retract(c(C)),C1 is C+8,assert(c(C1)),N=ptr(\rbp,-C1),asserta(m(A:N)).
adrs(A,A1) :- maplist(adr,A,A1).
prms(Ps,Ps_) :- regps(Regps),length(Regps,L),retractall(m(_)),
                findall(P:R,(nth0(I,Ps,P),I>=L,C is (I-L+2)*8,R=ptr(\rbp,C),assert(m(P:R))),M2),
                findall(P:R,(nth0(I,Ps,P),nth0(I,Regps,R)),M),append(M,M2,M3),
                forall(member(A:_,M),adr(A,_)),maplist(prms1,M3,Ps_).
prms1(A:R,mov(R,V)) :- m(A:V).
aCode(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
aCode(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
aCode(call(A,B,C),call(A,B1,C1,[]))  :- adrs(B,B1),adr(C,C1).
aCode(ret(A),ret(A1))                :- adr(A,A1).
aCode(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
aCode(br(A),br(A)).
aCode(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
aCode(C,_) :- throw(memAlloc(aCode(C))).
aBb(L:BB,L:BB1) :- maplist(aCode,BB,BB1).
aFunc(N:Ps=BBs,N:Ps_=[N1:[enter(Size,[])|Cs]|BBs1]) :-
  assert(c(0)),prms(Ps,Ps_),maplist(aBb,BBs,[N1:Cs|BBs1]),retract(c(Size)).
memAlloc(Fs,Fs_) :- maplist(aFunc,Fs,Fs_),!.

% gen amd64
regs([\rdi,\rsi,\rdx,\rcx,\r8,\r9]).
align(V,Align,S)     :- S is ((V+Align-1) div Align) * Align.
imm(A,A)             :- atom(A),!.
imm($I,A)            :- format(atom(A),'$~w',[I]).
imm(\R,A)            :- format(atom(A),'%~w',[R]).
imm(ptr(\R,I),A)     :- format(atom(A),'~w(%~w)',[I,R]).
emit(S)              :- fp(FP),writeln(FP,S).
emit(S,F)            :- fp(FP),maplist(imm,F,F_),format(FP,S,F_),nl(FP).
code(mov(A,B))       :- B=null;(emit('\tmov ~w,~w',[A,\rax]),emit('\tmov ~w,~w',[\rax,B])).
code(bin(Op,A,B,C))  :- emit('\tmov ~w,~w',[A,\rax]),emit('\t~w ~w,~w',[Op,B,\rax]),
                        code(mov(\rax,C)).
code(br(A))          :- emit('\tjmp ~w',[A]).
code(bne(A,B,C))     :- emit('\tmov ~w,~w',[A,\rax]),emit('\tcmp ~w,~w',[$0,\rax]),
                        emit('\tjne ~w',[B]),emit('\tjmp ~w',[C]).
code(enter(Size,Rs)) :- length(Rs,L),Align is (L mod 2)*8,
                        align(Size+Align,16,S),S1 is S - Align,
                        (S1=0;emit('\tsubq ~w,~w',[$S1,\rsp])),
                        reverse(Rs,RRs),forall(member(R,RRs),emit('\tpush ~w',[R])),
                        asserta(leave(Rs)).
code(ret(A))         :- emit('\tmov ~w,~w',[A,\rax]),name(Name),
                        emit('\tjmp .end.~w',[Name]).
code(call(N,B,A,Cs)) :- length(Cs,CsLen),CsAlign is CsLen mod 2,
                        forall(member(C,Cs),emit('\tpush ~w',[C])),
                          regs(Rs),findall([P,R],(nth0(I,Rs,R),nth0(I,B,P)),PRs),
                          length(Rs,RsLen),findall(P,(nth0(I,B,P),I>=RsLen),Ps),
                          length(Ps,PsLen),Align is ((PsLen+CsAlign) mod 2) * 8,
                          (Align=0;emit('\tsub ~w,~w',[$Align,\rsp])),
                            reverse(Ps,RPs),forall(member(F,RPs),emit('\tpush ~w',[F])),
                            reverse(PRs,RPRs),forall(member(F,RPRs),emit('\tmov ~w,~w',F)),
                            emit('\tcall ~w',[N]),
                          Align2 is PsLen*8+Align,
                          (Align2=0;emit('\tadd ~w,~w',[$Align2,\rsp])),
                        reverse(Cs,RCs),forall(member(C,RCs),emit('\tpop ~w',[C])),
                        code(mov(\rax,A)).
code(E)              :- throw(emit(code(E))).
bb(L:Cs)             :- emit('~w:',[L]),forall(member(C,Cs),code(C)),!.
func(Name:Ps=BBs)    :- emit('\t.globl ~w',[Name]),emit('~w:',[Name]),
                        emit('\tpushq\t~w',[\rbp]),emit('\tmovq\t~w,~w',[\rsp,\rbp]),
                        forall(member(P,Ps),code(P)),
                        asserta(name(Name)),forall(member(BB,BBs),bb(BB)),
                        emit('.end.~w:',[Name]),
                        retract(leave(Leave)),forall(member(R,Leave),emit('\tpop ~w',[R])),
                        emit('\tleave'),emit('\tret'),retractall(name(_)).
genAmd64(File,Ls)    :- setup_call_cleanup((open(File,write,FP),assert(fp(FP))),
                                           forall(member(L,Ls),func(L)),
                                           (close(FP),retract(fp(_)))).

% main
compile(Alloc,File) :- parseFile(File,P),genCode(P,E),call(Alloc,E,M),genAmd64('a.s',M).
main([Src])       :- compile(memAlloc,Src).
:- current_prolog_flag(argv,Argv),
   catch(main(Argv),E,(format('\033[0;41m~w\033[0;39m\n',[E]),halt(-1))),halt.
