:- module(asm,[open/1,asm/1,asm/2,close/0,genid/1,genid/2,resetid/0]).
open(File) :- open(File,write,FP), nb_setval(fp,FP).
asm(S) :- nb_getval(fp,FP),writeln(FP,S).
asm(S,F) :- format(atom(S1),S,F),nb_getval(fp,FP),writeln(FP,S1).
close() :- nb_getval(fp,FP),close(FP).
readfile(File,A) :- read_file_to_string(File,Str,[]),atom_string(A,Str).
:- nb_setval(idcounter,0).
resetid :- nb_setval(idcounter,0).
genid(C) :- nb_getval(idcounter,C),C1 is C+1,nb_setval(idcounter,C1).
genid(S,S1) :- nb_getval(idcounter,C),C1 is C+1,nb_setval(idcounter,C1),atom_concat(S,C,S1).
