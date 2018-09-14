:- module(emit,[emit/2]).
regs(['%rdi','%rsi','%rdx','%rcx','%r8','%r9']).
open(File) :- open(File,write,FP),nb_setval(fp,FP).
close() :- nb_getval(fp,FP),close(FP).
asm(S) :- nb_getval(fp,FP),writeln(FP,S).
asm(S,F) :- nb_getval(fp,FP),format(FP,S,F),nl(FP).
readfile(File,A) :- read_file_to_string(File,Str,[]),atom_string(A,Str).
prms([],_,0).
prms([P|Ps],[R|Rs],N) :- prms(Ps,Rs,N),asm('\tmov ~w,~w',[P,R]).
prms(Ps,[],N) :- prms2(Ps,0,N).
prms2([],C,C).
prms2([P|Ps],C,N) :- C8 is C+8,prms2(Ps,C8,N),asm('\tpush ~w',[P]).

code(mov(A,A)).
code(mov(A,B)) :-       (re_match('^[%$]',A);re_match('^%',B)),!,
                        asm('\tmovq ~w,~w',[A,B]).
code(mov(A,B)) :-       asm('\tmov ~w,%rax',[A]),
                        asm('\tmov %rax,~w',[B]).
code(enter(A,Rs))   :-  (A='$0';asm('\tsubq ~w,%rsp',[A])),
                        maplist([R]>>asm('\tpush ~w',[R]),Rs),!.
code(bin(Op,A,B,C)) :-  asm('\tmov ~w,%rax',[A]),
                        asm('\t~w ~w,%rax',[Op,B]),
                        asm('\tmov %rax,~w',[C]).
code(call(N,B,C,Cs)) :- regs(Regs),
                        maplist([R]>>asm('\tpush ~w',[R]),Cs),
                        prms(B,Regs,P),
                        asm('\tcall ~w',[N]),
                        (P=0;asm('\taddq $~w,%rsp',[P])),
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
code(prms(A)) :-        asm('\t# params ~w',[A]).
code(E) :- writeln(error:emit;code(E)),halt.
bb((L,Cs)) :-           asm('~w:',[L]),maplist(code,Cs),!.
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
