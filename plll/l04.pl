:- dynamic(start/2).
term_expansion(:-start(M,E),:-true) :- assert(start(M,E)).
term_expansion(:-end(M),:-true) :- retract(start(M,E)),forall(retract(data(P)),M:assert(P)),
                                   forall(member(P1,E),(M:export(M:P1),user:import(M:P1))).
term_expansion(P,:-true) :- start(_,_),assert(data(P)).
:- start(compile,[compile/2]).
  resetid     :- retractall(id(_)),assert(id(0)).
  genid(S,A)  :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'.~w~w',[S,C]).
  genreg(T,rl(T,Id)) :- genid('..',Id).
  add(V) :- assert(v(V)).
  compile(E,Vs) :- resetid,e(E,_),findall(V,retract(v(V)),Vs).
  e(eint(I),rn(ti(64),I)).
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(ti(64),R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(ti(64),R3),add(vbin(R3,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(tv,void),R).
  e(eprint(E1),rn(tv,void)) :- e(E1,R1),add(vprint(R1)).
:- end(compile).
:- start(emit,[emit/2]).
  t(rl(T,_),T).
  t(rn(T,_),T).
  pt(R,X) :- t(R,T),!,pt(T,X).
  pt(ti(I),X) :- format(atom(X),'i~w',[I]).
  pt(tv,void).
  p(A,A) :- atom(A),!.
  p(rl(_,Id),X) :- format(atom(X),'%~w',[Id]).
  p(rn(_,Id),Id).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w ~w ~w,~w',[p(Id),p(Op),pt(A),p(A),p(B)]).
  out(vprint(A)) :- asm('\tcall void @print_l(~w ~w) nounwind ssp',[pt(A),p(A)]).
  entry :-  asm('define i32 @main() {'),
            asm('entry:').
  leave :-  asm('\tret i32 0'),
            asm('}'),
            asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
            asm('define void @print_l(i64 %a) {'),
            asm('entry:'),
            asm('\t%a_addr = alloca i64'),
            asm('\tstore i64 %a,i64* %a_addr'),
            asm('\t%0 = load i64,i64* %a_addr'),
            asm('\t%1 = call i32 (i8*,...) @printf(i8* getelementptr inbounds ([5 x i8],[5 x i8]* @.str,i32 0,i32 0),i64 %0)'),
            asm('\tret void'),
            asm('}'),
            asm('declare i32 @printf(i8*,...)').
  emit(File,Vs) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (entry,maplist(out,Vs),leave),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile(eblock([
    eprint(eint(1)),
    eprint(eadd(eint(2),eint(3))),
    eprint(emul(eadd(eint(2),eint(3)),eint(2)))
  ]),Codes),
  format('# l=~w\n',[Codes]),
  emit('l04.ll',Codes),!,
  shell('llc l04.ll -o l04.s'),
  shell('gcc -m64 -static l04.s -o l04.exe'),
  shell('./l04.exe').
:-halt.
