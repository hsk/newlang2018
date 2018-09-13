% Welsh-Powell-Algorithm
c(Ns,Cs) :- predsort(c_cmp,Ns,Ns_),foldl(c1,Ns_,([],0,Ns_),(Cs,_,_)).
c_cmp(>,_:_/L1,_:_/L2) :- L2>L1,!. c_cmp(<,_,_).
c1(A:_/_,(Cs,C,[_|I]),(Cs,C,I)) :- member(A:_,Cs),!.
c1(A:E/_,(Cs,C,[_|I]),(Cs1,C1,I)) :- C1 is C+1,foldl(c2(C1,E),I,[A:C1|Cs],Cs1),!.
c2(C,E,A:N/_,Cs,Cs) :- (member(A:_,Cs);member(A,E);member(V,N),member(V:C,Cs)),!.
c2(C,_,A:_/_,Cs,[A:C|Cs]).
n(Es,Ns) :- foldl(n1,Es,[],Ns).
n1(A-B,Ns,[A:[B|As]/Al1,B:[A|Bs]/Bl1|Ns_]) :- subtract(Ns,[A:As/Al,B:Bs/Bl],Ns_),
                             (As=[],Al1=1;Al1 is Al+1),(Bs=[],Bl1=1;Bl1 is Bl+1).
dot(Cs,Es,R) :- maplist(dot1,Cs,R1),maplist(dot2,Es,R2),
                append([['graph G{\n'],R1,R2,['}']],R3),atomic_list_concat(R3,R).
dot1(K:C,A) :- format(atom(A),'  ~w [label="~w r~w"]\n',[K,K,C]).
dot2(A-B,R) :- format(atom(R),'  ~w -- ~w\n',[A,B]).
run(F,Es) :- n(Es,Ns),c(Ns,Cs),dot(Cs,Es,R2),
        format(atom(Dot),'~w.dot',[F]),open(Dot,write,O),writeln(O,R2),close(O),
        format(atom(Cmd),'dot -Tpng -o~w.png ~w.dot',[F,F]),shell(Cmd).
:- run(wpl1,[a-f,f-e,f-m,f-j,f-z,f-b,f-c,e-z,e-h,e-b,e-m,j-k,j-d,j-h,j-g,
             j-z,j-b,j-c,k-b,k-d,k-g,k-z,k-b,k-c,b-m,b-d,b-c,m-c,m-d,h-g]).
:- run(wpl,[x-'1',y-'2','4'-'1','5'-'2','6'-'4','6'-'5','4'-'5','8'-'1','9'-'8',
             '18'-'1','11'-'1','12'-'11','12'-'1','14'-'1','15'-'14','15'-'1']).
:- halt.
