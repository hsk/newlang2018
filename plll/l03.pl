:- dynamic(begin/2).
term_expansion(:-begin(M,E),:-true) :- assert(begin(M,E)).
term_expansion(:-end(M),:-true) :- retract(begin(M,E)),forall(retract(data(P)),M:assert(P)),
                                   forall(member(P1,E),(M:export(M:P1),user:import(M:P1))).
term_expansion(P,:-true) :- begin(_,_),assert(data(P)).
:- begin(compile,[compile/2]).
  resetid     :- retractall(id(_)),assert(id(0)).
  genid(S,A)  :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'~w~w',[S,C]).
  genreg(rl(Id)) :- genid('..',Id).
  add(V) :- assert(v(V)).
  compile(E,Vs) :- resetid,e(E,_),findall(V,retract(v(V)),Vs).
  e(eint(I),rn(I)).
  e(eadd(E1,E2),R) :- e(E1,R1),e(E2,R2),genreg(R),add(vbin(R,add,R1,R2)).
  e(emul(E1,E2),R) :- e(E1,R1),e(E2,R2),genreg(R),add(vbin(R,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(void),R).
  e(eprint(E1),R2) :- e(E1,R1),R2 = rn(void),add(vprint(R1)).
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
            asm('}'),
            asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
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
                    (entry,maplist(out,Vs),leave),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile(eblock([
    eprint(eint(1)),
    eprint(eadd(eint(2),eint(3))),
    eprint(emul(eadd(eint(2),eint(3)),eint(2)))
  ]),Codes),
  format('# l=~w\n',[Codes]),
  emit('l03.ll',Codes),!,
  shell('llc l03.ll -o l03.s'),
  shell('gcc -static l03.s -o l03.exe'),
  shell('./l03.exe').
:- halt.
