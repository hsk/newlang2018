:- module(emit,[emit/2]).
regs(['%rdi','%rsi','%rdx','%rcx','%r8','%r9']).
open(File) :- open(File,write,FP),nb_setval(fp,FP).
close() :- nb_getval(fp,FP),close(FP).
asm(S) :- nb_getval(fp,FP),writeln(FP,S).
asm(S,F) :- nb_getval(fp,FP),format(FP,S,F),nl(FP).
readfile(File,A) :- read_file_to_string(File,Str,[]),atom_string(A,Str).
align(V,Align,S) :- S is ((V+Align-1) div Align) * Align.
code(mov(A,A)).
code(mov(A,B)) :-       (re_match('^[%$]',A);re_match('^%',B)),!,
                        asm('\tmovq ~w,~w',[A,B]).
code(mov(A,B)) :-       asm('\tmov ~w,%rax',[A]),
                        asm('\tmov %rax,~w',[B]).
code(enter(Size,Rs)) :- length(Rs,L),Align is (L mod 2)*8,
                        align(-Size+Align,16,S),S1 is S - Align,
                        (S1=0;asm('\tsubq $~w,%rsp',[S1])),
                        forall(member(R,Rs),asm('\tpush ~w',[R])).
code(bin(Op,A,B,C)) :-  asm('\tmov ~w,%rax',[A]),
                        asm('\t~w ~w,%rax',[Op,B]),
                        asm('\tmov %rax,~w',[C]).
code(call(N,B,A,Cs)) :- length(Cs,CsLen),CsAlign is CsLen mod 2,
                        forall(member(C,Cs),asm('\tpush ~w',[C])),
                          regs(Rs),findall([P,R],(nth0(I,Rs,R),nth0(I,B,P)),PRs),
                          length(Rs,RsLen),findall(P,(nth0(I,B,P),I>=RsLen),Ps),
                          length(Ps,PsLen),Align is ((PsLen+CsAlign) mod 2) * 8,
                          (Align=0;asm('\tsub $~w,%rsp',[Align])),
                            reverse(Ps,RPs),forall(member(F,RPs),asm('\tpush ~w',[F])),
                            reverse(PRs,RPRs),forall(member(F,RPRs),asm('\tmov ~w,~w',F)),
                            asm('\tcall ~w',[N]),
                          Align2 is PsLen*8+Align,
                          (Align2=0;asm('\tadd $~w,%rsp',[Align2])),
                        reverse(Cs,RCs),forall(member(C,RCs),asm('\tpop ~w',[C])),
                        asm('\tmov %rax,~w',[A]).
code(ret(A)) :-         asm('\tmov ~w,%rax',[A]),nb_getval(name,Name),
                        asm('\tjmp .end.~w',[Name]).
code(bne(A,B,C)) :-     asm('\tmov ~w,%rax',[A]),
                        asm('\tcmp $0,%rax'),
                        asm('\tjne ~w',[B]),
                        asm('\tjmp ~w',[C]).
code(br(A)) :-          asm('\tjmp ~w',[A]).
code(label(A)) :-       asm('~w:',[A]).
code(prms(A)) :-        asm('\t# params ~w',[A]).
code(E) :-              writeln(error:emit;code(E)),halt.
bb((L,Cs)) :-           asm('~w:',[L]),forall(member(C,Cs),code(C)),!.
func((Name,Rs,BBs)) :-  asm('\t.globl ~w',[Name]),
                        asm('~w:',[Name]),
                        asm('\tpushq\t%rbp'),
                        asm('\tmovq\t%rsp,%rbp'),
                        nb_linkval(name,Name),forall(member(BB,BBs),bb(BB)),
                        asm('.end.~w:',[Name]),
                        forall(member(A,Rs),asm('\tpop ~w',[A])),
                        asm('\tleave'),
                        asm('\tret').
emit(File,Ls) :-        open(File),forall(member(L,Ls),func(L)),close().
