:- module(memAlloc,[memAlloc/2]).

adr(A,V) :- atom(A),nb_getval(m,M),member(A:V,M),!.
adr(A,A) :- atom(A),re_match('^[%$]',A),!.
adr(A,N) :- atom(A),!,
            nb_getval(counter,C),C1 is C - 8,nb_setval(counter,C1),
            format(atom(N),'~w(%rbp)',[C1]),
            nb_getval(m,M),nb_setval(m,[A:N|M]).
adr(A,A).
adrs(A,A1) :- maplist(adr,A,A1).
code(mov(A,B),mov(A1,B1))           :- adr(A,A1),adr(B,B1).
code(bin(O,A,B,C),bin(O,A1,B1,C1))  :- adr(A,A1),adr(B,B1),adr(C,C1).
code(call(A,B,C),call(A,B1,C1))     :- adrs(B,B1),adr(C,C1).
code(ret(A),ret(A1))                :- adr(A,A1).
code(bne(A,B,C),bne(A1,B,C))        :- adr(A,A1).
code(br(A),br(A)).
code(label(A),label(A)).
code(if(A,C,D),if(A1,C1,D1))        :- adr(A,A1),adrs(C,C1),adrs(D,D1).
bb((L,BB),(L,BB1)) :- maplist(code,BB,BB1).
func((N,BBs),(N,[(N1,[sub(Size1,'%rsp')|Cs])|BBs1])) :-
                                    nb_setval(counter,0), nb_setval(m,[]),
                                    maplist(bb,BBs,[(N1,Cs)|BBs1]),
                                    nb_getval(counter,Counter),
                                    Size is floor((15-Counter)/16)*16,
                                    format(atom(Size1),'$~w',[Size]).
memAlloc(Fs,Fs_)                 :- maplist(func,Fs,Fs_).
