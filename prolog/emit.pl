:- module(emit,[emit/2]).
:- use_module(utils).
regs(['%edi','%esi','%edx']).
open(File) :- open(File,write,FP),nb_setval(fp,FP).
close() :- nb_getval(fp,FP),close(FP).
asm(S) :- nb_getval(fp,FP),writeln(FP,S).
asm(S,F) :- nb_getval(fp,FP),format(FP,S,F),nl(FP).
readfile(File,A) :- read_file_to_string(File,Str,[]),atom_string(A,Str).
prms([],_).
prms([P|Ps],[R|Rs]) :-  asm('\tmovl ~w,~w',[P,R]),prms(Ps,Rs).
code(movl(A,B)) :-      (re_match('^[%$]',A);re_match('^%',B)),!,
                        asm('\tmovl ~w,~w',[A,B]).
code(movl(A,B)) :-      asm('\tmovl ~w,%eax',[A]),
                        asm('\tmovl %eax,~w',[B]).
code(subq(A,B)) :-      asm('\tsubq ~w,~w',[A,B]).
code(bin(Op,A,B,C)) :-  asm('\tmovl ~w,%eax',[A]),
                        asm('\t~w ~w,%eax',[Op,B]),
                        asm('\tmovl %eax,~w',[C]).
code(call(N,B)) :-      regs(Regs),prms(B,Regs),
                        asm('\tcall ~w',[N]).
code(ret(A)) :-         asm('\tmovl ~w,%eax',[A]),
                        asm('\tleave'),
                        asm('\tret').
code(bne(A,B,C)) :-     asm('\tmovl ~w,%eax',[A]),
                        asm('\tcmpl $0,%eax'),
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
