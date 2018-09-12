:- module(asm,[open/1,asm/1,asm/2,close/0]).
open(File) :- open(File,write,FP), nb_setval(fp,FP).
asm(S) :- nb_getval(fp,FP),writeln(FP,S).
asm(S,F) :- format(atom(S1),S,F),nb_getval(fp,FP),writeln(FP,S1).
close() :- nb_getval(fp,FP),close(FP).
readfile(File,A) :- read_file_to_string(File,Str,[]),atom_string(A,Str).
