:- module(genAmd64,[genAmd64/2]).
regs(['%rdi','%rsi','%rdx','%rcx','%r8','%r9']).
align(V,Align,S)     :- S is ((V+Align-1) div Align) * Align.
code(mov(A,A)).
code(mov(A,B))       :- (re_match('^[%$]',A);re_match('^%',B)),!,
                        emit('\tmovq ~w,~w',[A,B]).
code(mov(A,B))       :- emit('\tmov ~w,%rax',[A]),
                        emit('\tmov %rax,~w',[B]).
code(bin(Op,A,B,C))  :- emit('\tmov ~w,%rax',[A]),
                        emit('\t~w ~w,%rax',[Op,B]),
                        emit('\tmov %rax,~w',[C]).
code(label(A))       :- emit('~w:',[A]).
code(br(A))          :- emit('\tjmp ~w',[A]).
code(bne(A,B,C))     :- emit('\tmov ~w,%rax',[A]),
                        emit('\tcmp $0,%rax'),
                        emit('\tjne ~w',[B]),
                        emit('\tjmp ~w',[C]).
code(enter(Size,Rs)) :- length(Rs,L),Align is (L mod 2)*8,
                        align(Size+Align,16,S),S1 is S - Align,
                        (S1=0;emit('\tsubq $~w,%rsp',[S1])),
                        reverse(Rs,RRs),forall(member(R,RRs),emit('\tpush ~w',[R])),
                        nb_setval(leave,Rs).
code(ret(A))         :- emit('\tmov ~w,%rax',[A]),nb_getval(name,Name),
                        emit('\tjmp .end.~w',[Name]).
code(call(N,B,A,Cs)) :- length(Cs,CsLen),CsAlign is CsLen mod 2,
                        forall(member(C,Cs),emit('\tpush ~w',[C])),
                          regs(Rs),findall([P,R],(nth0(I,Rs,R),nth0(I,B,P)),PRs),
                          length(Rs,RsLen),findall(P,(nth0(I,B,P),I>=RsLen),Ps),
                          length(Ps,PsLen),Align is ((PsLen+CsAlign) mod 2) * 8,
                          (Align=0;emit('\tsub $~w,%rsp',[Align])),
                            reverse(Ps,RPs),forall(member(F,RPs),emit('\tpush ~w',[F])),
                            reverse(PRs,RPRs),forall(member(F,RPRs),emit('\tmov ~w,~w',F)),
                            emit('\tcall ~w',[N]),
                          Align2 is PsLen*8+Align,
                          (Align2=0;emit('\tadd $~w,%rsp',[Align2])),
                        reverse(Cs,RCs),forall(member(C,RCs),emit('\tpop ~w',[C])),
                        emit('\tmov %rax,~w',[A]).
code(E)              :- writeln(error:emit;code(E)),halt.
bb((L,Cs))           :- emit('~w:',[L]),forall(member(C,Cs),code(C)),!.
func((Name,Ps,BBs))  :- emit('\t.globl ~w',[Name]),
                        emit('~w:',[Name]),
                        emit('\tpushq\t%rbp'),
                        emit('\tmovq\t%rsp,%rbp'),
                        forall(member(P,Ps),code(P)),
                        nb_setval(name,Name),forall(member(BB,BBs),bb(BB)),
                        emit('.end.~w:',[Name]),
                        nb_getval(leave,Leave),forall(member(R,Leave),emit('\tpop ~w',R)),
                        emit('\tleave'),
                        emit('\tret').
emit(S)              :- nb_getval(fp,FP),writeln(FP,S).
emit(S,F)            :- nb_getval(fp,FP),format(FP,S,F),nl(FP).
genAmd64(File,Ls)    :- setup_call_cleanup((open(File,write,FP),nb_setval(fp,FP)),
                                           forall(member(L,Ls),func(L)),
                                           close(FP)).
