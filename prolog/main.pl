:- use_module([genCode,memAlloc,emit]).

expr(I,I) :- integer(I),!.
expr(A,A) :- atom(A),!.
expr(E1+E2,bin(addl,E1_,E2_)) :- expr(E1,E1_),expr(E2,E2_).
expr(E1-E2,bin(subl,E1_,E2_)) :- expr(E1,E1_),expr(E2,E2_).
expr(A=E,mov(E_,A)) :- atom(A),expr(E,E_).
expr(AEs,call(A,Es_)) :- compound_name_arguments(AEs,A,Es),maplist(expr,Es,Es_).
expr(E,_) :- writeln(error:E),halt.
stmt(return(E),ret(E_)) :- expr(E,E_).
stmt(if(E,S1,S2),if(E_,S1_,S2_)) :- expr(E,E_),maplist(stmt,S1,S1_),maplist(stmt,S2,S2_).
stmt(S,S_) :- expr(S,S_).
func(NP=B,(N,P,B_)) :- compound_name_arguments(NP,N,P),maplist(stmt,B,B_).
parse(Fs,Fs_) :- maplist(func,Fs,Fs_),!.
parseFile(File,Fs_) :- read_file_to_terms(File,Fs,[]),parse(Fs,Fs_).

compile(File) :-  parseFile(File,P),genCode(P,E),memAlloc(E,M),emit('a.s',M),
                  shell('gcc -static -o a a.s lib/lib.c').

:- compile('src.mc'),shell('./a').
:- halt.
