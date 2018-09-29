:- dynamic(begin/2).
term_expansion(:-begin(M,E),:-true) :- assert(begin(M,E)).
term_expansion(:-end(M),:-true) :- retract(begin(M,E)),forall(retract(data(P)),M:assert(P)),
                                   forall(member(P1,E),(M:export(M:P1),user:import(M:P1))).
term_expansion(P,:-true) :- begin(_,_),assert(data(P)).
:- op(1200,xfx,::=).
:- op(650,xfx,∈).
:- begin(syntax,[syntax/2]).
G∈{G}. G∈(G|_). G∈(_|G1):-G∈G1. G∈G.
syntax(G,E):-G=..[O|Gs],E=..[O|Es],maplist(syntax,Gs,Es),!.
syntax(G,E):-(G::=Gs),!,G1∈Gs,syntax(G1,E),!.
syntax(i,I):-integer(I),!.
syntax(id,I):- atom(I),!.
syntax(list(E),Ls) :- maplist(syntax(E),Ls).
t ::= tv | ti(i) | tp(t).
e ::= eint(i) | eadd(e,e) | emul(e,e) | eprint(e) | eblock(list(e)) | evar(id,t) | eid(id) | eassign(e,e).
r ::= rl(t,id) | rn(t,i).
v ::= vprint(r) | vbin(r,id,r,r) | valloca(r) | vload(r,r) | vstore(r,r).
vs ::= list(v).
:- end(syntax).
:- begin(compile,[compile/2]).
  resetid    :- retractall(id(_)),assert(id(0)).
  genid(S,A) :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'~w~w',[S,C]).
  genreg(T,rl(T,Id)) :- genid('..',Id).
  add(V) :- assert(v(V)).
  arr(eid(Id),rl(tp(T),Id)) :- env(Id,T).
  arr(_,_) :- throw(error).
  compile(E,Vs) :- syntax(e,E),resetid,e(E,_),findall(V,retract(v(V)),Vs).
  e(eint(I),rn(ti(64),I)).
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(ti(64),R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(ti(64),R3),add(vbin(R3,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(tv,void),R).
  e(eprint(E1),rn(tv,void)) :- e(E1,R1),add(vprint(R1)).
  e(evar(Id,T),R1) :- R1=rl(T,Id),add(valloca(R1)),assert(env(Id,T)).
  e(eassign(E1,E2),R1) :- e(E2,R1),arr(E1,R2),add(vstore(R1,R2)).
  e(eid(Id),R1) :- env(Id,T),genreg(T,R1),arr(eid(Id),R2),add(vload(R1,R2)).
:- end(compile).
:- begin(emit,[emit/2]).
  t(rl(T,_),T).
  t(rn(T,_),T).
  pt(R,X) :- t(R,T),!,pt(T,X).
  pt(ti(I),X) :- format(atom(X),'i~w',[I]).
  pt(tv,void).
  pt(tp(T),X) :- pt(T,X1),format(atom(X),'~w*',[X1]).
  p(A,A) :- atom(A),!.
  p(rl(_,Id),X) :- format(atom(X),'%~w',[Id]).
  p(rn(_,Id),Id).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w ~w ~w,~w',[p(Id),p(Op),pt(A),p(A),p(B)]).
  out(vprint(A)) :- asm('\tcall void @print_l(~w ~w)',[pt(A),p(A)]).
  out(valloca(R)) :- asm('\t~w = alloca ~w',[p(R),pt(R)]).
  out(vload(R1,R2)) :- asm('\t~w = load ~w,~w ~w',[p(R1),pt(R1),pt(R2),p(R2)]).
  out(vstore(R1,R2)) :- asm('\tstore ~w ~w,~w ~w',[pt(R1),p(R1),pt(R2),p(R2)]).
  entry :-  asm('define i32 @main() {'),
            asm('entry:').
  leave :-  asm('\tret i32 0'),
            asm('}').
  printl :- asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
            asm('define void @print_l(i64 %a) {'),
            asm('entry:'),
            asm('\t%a_addr = alloca i64'),
            asm('\tstore i64 %a,i64* %a_addr'),
            asm('\t%0 = load i64,i64* %a_addr'),
            asm('\t%1 = call i32 (i8*,...) @printf(i8* ~w,i64 %0)',
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
    evar(c,ti(64)),
    eassign(eid(c),eadd(eint(10),eint(5))),
    eprint(eid(c))
  ]),Codes),
  syntax(vs,Codes),
  emit('l05.ll',Codes),!,
  shell('llc l05.ll -o l05.s'),
  shell('gcc -static l05.s -o l05.exe'),
  shell('./l05.exe').
:-halt.
