:- use_module([syntax,genCode,memAlloc,graphRegAlloc,linearScanRegAlloc,genAmd64]).
expr(I,I) :- integer(I),!.
expr(A,A) :- atom(A),!.
expr(E1+E2,bin(addq,E1_,E2_)) :- expr(E1,E1_),expr(E2,E2_).
expr(E1-E2,bin(subq,E1_,E2_)) :- expr(E1,E1_),expr(E2,E2_).
expr(A=E,mov(E_,A)) :- atom(A),expr(E,E_).
expr(AEs,call(A,Es_)) :- compound_name_arguments(AEs,A,Es),maplist(expr,Es,Es_).
expr(E,_) :- throw(main(expr(E))).
stmt(return(E),ret(E_)) :- expr(E,E_).
stmt(if(E,S1,S2),if(E_,S1_,S2_)) :- expr(E,E_),maplist(stmt,S1,S1_),maplist(stmt,S2,S2_).
stmt(while(E,S),while(E_,S_)) :- expr(E,E_),maplist(stmt,S,S_).
stmt(S,S_) :- expr(S,S_).
func(NP=B,N:P=B_) :- compound_name_arguments(NP,N,P),maplist(stmt,B,B_).
parse(Fs,Fs_) :- maplist(func,Fs,Fs_),!.
readFile(File,Fs) :- read_file_to_terms(File,Fs,[]).
compile(Alloc,File) :- readFile(File,P),syntax(p,P),parse(P,A),syntax(a,A),
                       genCode(A,C),syntax(c,C),call(Alloc,C,R),syntax(r,R),genAmd64('a.s',R).
main([Src])       :- compile(memAlloc,Src).
main(['-O1',Src]) :- compile(linearScanRegAlloc,Src).
main(['-O2',Src]) :- compile(regAlloc,Src).
main :- current_prolog_flag(argv,Argv),
        catch(main(Argv),E,(format('\033[0;41m~w\033[0;39m\n',[E]),halt(-1))),halt.
comp :- current_prolog_flag(os_argv,OS),current_prolog_flag(argv,Argv),
        subtract(OS,Argv,OS2),member('-c',OS2).
:- comp; main.
:- comp; halt.
