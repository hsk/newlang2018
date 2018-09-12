:- module(genid,[genid/1,genid/2,resetid/0]).
resetid :- nb_setval(idcounter,0).
genid(C) :- nb_getval(idcounter,C),C1 is C+1,nb_setval(idcounter,C1).
genid(S,S1) :- nb_getval(idcounter,C),C1 is C+1,nb_setval(idcounter,C1),atom_concat(S,C,S1).
:- nb_setval(idcounter,0).
