:- module(emit,[emit/2]).
:- use_module(utils).
regs(['%rdi','%rsi','%rdx','%rcx','%r8','%r9']).
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
code(enter(A,Rs))   :-  (A='$0';asm('\tsubq %rsp,~w',[A])),
                        maplist([R]>>asm('\tpush ~w',[R]),Rs),!.
code(bin(Op,A,B,C)) :-  asm('\tmov ~w,%rax',[A]),
                        asm('\t~w ~w,%rax',[Op,B]),
                        asm('\tmov %rax,~w',[C]).
code(call(N,B,C,Cs)) :- regs(Regs),
                        maplist([R]>>asm('\tpush ~w',[R]),Cs),
                        prms(B,Regs),
                        asm('\tcall ~w',[N]),
                        reverse(Cs,RCs),maplist([R]>>asm('\tpop ~w',[R]),RCs),
                        asm('\tmov %rax,~w',[C]).
code(ret(A)) :-         asm('\tmov ~w,%rax',[A]),nb_getval(name,Name),
                        asm('\tjmp .end.~w',[Name]).
code(bne(A,B,C)) :-     asm('\tmov ~w,%rax',[A]),
                        asm('\tcmp $0,%rax'),
                        asm('\tjne ~w',[B]),
                        asm('\tjmp ~w',[C]).
code(br(A)) :-          asm('\tjmp ~w',[A]).
code(label(A)) :-       asm('~w:',[A]).
bb((L,Cs)) :-           asm('~w:',[L]),maplist(code,Cs).
func((Name,Rs,BBs)) :-  asm('\t.globl ~w',[Name]),
                        asm('~w:',[Name]),
                        asm('\tpushq\t%rbp'),
                        asm('\tmovq\t%rsp,%rbp'),
                        nb_linkval(name,Name),maplist(bb,BBs),
                        asm('.end.~w:',[Name]),
                        maplist([A]>>asm('\tpop ~w',[A]),Rs),
                        asm('\tleave'),
                        asm('\tret').
emit(File,Ls) :-        open(File),maplist(func,Ls),close().
