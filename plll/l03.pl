:- dynamic(begin/2).
term_expansion(:-begin(M,E),:-true) :- assert(begin(M,E)).
term_expansion(:-end(M),:-true) :- retract(begin(M,E)),forall(retract(data(P)),M:assert(P)),
                                   forall(member(P1,E),(M:export(M:P1),user:import(M:P1))).
term_expansion(P,:-true) :- begin(_,_),assert(data(P)).
:- op(1200,xfx,::=).
:- op(650,xfx,∈).
:- op(250,yf,*).
:- begin(syntax,[syntax/2]).
  G∈{G}. G∈(G|_). G∈(_|G1):-G∈G1. G∈G.
  syntax(G,E):-G=..[O|Gs],E=..[O|Es],maplist(syntax,Gs,Es),!.
  syntax(G,E):-(G::=Gs),!,G1∈Gs,syntax(G1,E),!.
  syntax(i,I):-integer(I),!.
  syntax(id,I):- atom(I),!.
  syntax(E*,Ls) :- maplist(syntax(E),Ls).
  e ::= eint(i) | eadd(e,e) | emul(e,e) | eprint(e) | eblock(e*).
  r ::= rl(id) | rn(i).
  v ::= vprint(r) | vbin(r,id,r,r).
:- end(syntax).
:- begin(env,[resetid/0,genid/2,genreg/1]).
  resetid    :- retractall(id(_)),assert(id(0)).
  genid(S,A) :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'~w~w',[S,C]).
  genreg(rl(Id)) :- genid('..',Id).
:- end(env).
:- begin(compile,[compile/2]).
  add(V) :- assert(v(V)).
  e(eint(I),rn(I)).
  e(eadd(E1,E2),R) :- e(E1,R1),e(E2,R2),genreg(R),add(vbin(R,add,R1,R2)).
  e(emul(E1,E2),R) :- e(E1,R1),e(E2,R2),genreg(R),add(vbin(R,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(void),R).
  e(eprint(E1),rn(void)) :- e(E1,R1),add(vprint(R1)).
  compile(E,Vs) :- syntax(e,E),resetid,e(E,_),findall(V,retract(v(V)),Vs).
:- end(compile).
:- begin(emit,[emit/2]).
  p(rl(Id),X) :- format(atom(X),'%~w',[Id]).
  p(rn(Id),Id).
  p(A,A) :- atom(A).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w i64 ~w,~w',[p(Id),p(Op),p(A),p(B)]).
  out(vprint(A)) :- asm('\tcall void @print_l(i64 ~w)',[p(A)]).
  entry :-  asm('define i32 @main() {'),
            asm('entry:').
  leave :-  asm('\tret i32 0'),
            asm('}').
  printl :- asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
            asm('define void @print_l(i64 %a) {'),
            asm('entry:'),
            asm('\t%0 = call i32 (i8*,...) @printf(i8* ~w,i64 %a)',
                [p('getelementptr inbounds ([5 x i8],[5 x i8]* @.str,i32 0,i32 0)')]),
            asm('\tret void'),
            asm('}'),
            asm('declare i32 @printf(i8*,...)').
  emit(File,Vs) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (entry,maplist(out,Vs),leave,printl),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile(eblock([
    eprint(eint(1)),
    eprint(eadd(eint(2),eint(3))),
    eprint(emul(eadd(eint(2),eint(3)),eint(2)))
  ]),Codes),
  format('# l=~w\n',[Codes]),
  syntax(v*,Codes),
  emit('l03.ll',Codes),!,
  shell('llc l03.ll -o l03.s'),
  shell('gcc -static l03.s -o l03.exe'),
  shell('./l03.exe').
:- halt.
