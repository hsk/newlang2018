:- module(emit,[emit/2]).
:- use_module(asm).
regs(['%edi','%esi','%edx']).
prms([],_).
prms([P|Ps],[R|Rs]) :-  asm('\tmovl ~w,~w',[P,R]),
                        prms(Ps,Rs).
code(movl(A,B)) :-      asm('\tmovl ~w,~w',[A,B]).
code(subq(A,B)) :-      asm('\tsubq ~w,~w',[A,B]).
code(addl(A,B,C)) :-    asm('\tmovl ~w,%eax',[A]),
                        asm('\taddl ~w,%eax',[B]),
                        asm('\tmovl %eax,~w',[C]).
code(call(N,B)) :-      regs(Regs),
                        prms(B,Regs),
                        asm('\tcall ~w',[N]).
code(ret(A)) :-         asm('\tmovl ~w,%eax',[A]),
                        asm('\tleave'),
                        asm('\tret').
code(ifeq(A,B,C,D)) :-  genid('id_else',Id_else),
                        genid('id_cont',Id_cont),
                        asm('\tmovl ~w,%eax',[A]),
                        asm('\tcmpl ~w,%eax',[B]),
                        asm('\tjne ~w',[Id_else]),
                        maplist(code,C),
                        asm('~w:',[Id_else]),
                        asm('\tjmp ~w',[Id_cont]),
                        maplist(code,D),
                        asm('~w:',[Id_cont]).
func((Name,Body)) :-    asm('\t.globl ~w',[Name]),
                        asm('~w:',[Name]),
                        asm('\tpushq\t%rbp'),
                        asm('\tmovq\t%rsp,%rbp'),
                        maplist(code,Body).
emit(File,Ls) :-        asm:open(File),
                        maplist(func,Ls),
                        asm:close().
