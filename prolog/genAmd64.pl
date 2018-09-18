:- module(genAmd64,[genAmd64/2]).
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
