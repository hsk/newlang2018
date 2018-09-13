:- module(emit,[emit/2]).
:- use_module(utils).
regs(['%rdi','%rsi','%rdx']).
open(File) :- open(File,write,FP),nb_setval(fp,FP).
close() :- nb_getval(fp,FP),close(FP).
asm(S) :- nb_getval(fp,FP),writeln(FP,S).
asm(S,F) :- nb_getval(fp,FP),format(FP,S,F),nl(FP).
readfile(File,A) :- read_file_to_string(File,Str,[]),atom_string(A,Str).
prms([],_).
prms([P|Ps],[R|Rs]) :-  asm('\tmov ~w,~w',[P,R]),prms(Ps,Rs).
code(mov(A,B)) :-       (re_match('^[%$]',A);re_match('^%',B)),!,
                        asm('\tmovq ~w,~w',[A,B]).
code(mov(A,B)) :-       asm('\tmov ~w,%rax',[A]),
                        asm('\tmov %rax,~w',[B]).
code(sub(A,B)) :-       (A='$0';asm('\tsubq ~w,~w',[A,B])),
                        asm('\tpush %r12'),
                        asm('\tpush %r13'),
                        asm('\tpush %r14'),
                        asm('\tpush %r15'),
                        !.
code(bin(Op,A,B,C)) :-  asm('\tmov ~w,%rax',[A]),
                        asm('\t~w ~w,%rax',[Op,B]),
                        asm('\tmov %rax,~w',[C]).
code(call(N,B,C)) :-    regs(Regs),
                        asm('\tpush %r10'),
                        asm('\tpush %r11'),
                        prms(B,Regs),
                        asm('\tcall ~w',[N]),
                        asm('\tpop %r11'),
                        asm('\tpop %r10'),
                        asm('\tmov %rax,~w',[C]).
code(ret(A)) :-         asm('\tmov ~w,%rax',[A]),
                        asm('\tpop %r15'),
                        asm('\tpop %r14'),
                        asm('\tpop %r13'),
                        asm('\tpop %r12'),
                        asm('\tleave'),
                        asm('\tret').
code(bne(A,B,C)) :-     asm('\tmov ~w,%rax',[A]),
                        asm('\tcmp $0,%rax'),
                        asm('\tjne ~w',[B]),
                        asm('\tjmp ~w',[C]).
code(br(A)) :-          asm('\tjmp ~w',[A]).
code(label(A)) :-       asm('~w:',[A]).
bb((L,Cs)) :-           asm('~w:',[L]),maplist(code,Cs).
func((Name,Body)) :-    asm('\t.globl ~w',[Name]),
                        asm('~w:',[Name]),
                        asm('\tpushq\t%rbp'),
                        asm('\tmovq\t%rsp,%rbp'),
                        maplist(bb,Body).
emit(File,Ls) :-        open(File),maplist(func,Ls),close().
