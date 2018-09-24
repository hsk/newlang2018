:- dynamic(start/2).
term_expansion(:-start(M,E),:-true) :- assert(start(M,E)).
term_expansion(:-end(M),:-true) :- retract(start(M,E)),forall(retract(data(P)),M:assert(P)),
                                   forall(member(P1,E),(M:export(M:P1),user:import(M:P1))).
term_expansion(P,:-true) :- start(_,_),assert(data(P)).
:- start(compile,[compile/2,str/2]).
  resetid     :- retractall(id(_)),assert(id(0)).
  genid(S,A)  :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'.~w~w',[S,C]).
  genreg(T,rl(T,Id)) :- genid('..',Id).
  add_env(Id,T) :- assert(env(Id,T)), add_str(T).
  add_str(tarr(T,_)) :- add_str(T).
  add_str(tstr(Ls)) :- T=tstr(Ls),(str(T,_); genreg(T,R),assert(str(T,R))),
                       forall(member(_:T1,Ls),add_str(T1)).
  add_str(_).
  add(V) :- assert(v(V)).
  cut(tp(tarr(T,_)),tp(T)).
  cut(tp(T),T).
  arr(eid(Id),rl(tp(T),Id)) :- !,env(Id,T).
  arr(earray(Id,E),R4) :- !,e(E,R1),R2=rn(ti(64),0),arr(Id,R3),
                          emit:t(R3,T3),cut(T3,T4),genreg(T4,R4),
                          (T3=tp(tp(_)) -> genreg(T4,R5),add(vload(R5,R3)),add(vbin(R4,add,R5,R1))
                          ; add(vfield(R4,R3,R2,R1))).
  arr(efield(Id,Idx),R4) :- arr(Id,R3),emit:t(R3,tp(tstr(M))),T=tstr(M),member(Idx:T2,M),
                            index(T,Idx,N),R1=rn(ti(32),N),R2=rn(ti(64),0),genreg(tp(T2),R4),
                            add(vfield(R4,R3,R2,R1)).
  arr(eptr(Id),R1) :- arr(earray(Id,eint(0)),R1).
  arr(E,_) :- findall(env(Id,T),env(Id,T),Vs),throw(error:arr(E);Vs).
  index(tstr(Ls),Id,N) :- nth0(N,Ls,Id:_).
  index(_,_) :- throw(error).
  compile(E,Vs) :- resetid,dynamic(str/2),e(E,_),findall(V,retract(v(V)),Vs).
  e(eint(I),rn(ti(64),I)) :- !.
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),emit:t(R1,T1),genreg(T1,R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),emit:t(R1,T1),genreg(T1,R3),add(vbin(R3,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(tv,void),R).
  e(eprint(E1),rn(tv,void)) :- !,e(E1,R1),add(vprint(R1)).
  e(evar(Id,T),R1) :- R1=rl(T,Id),add(valloca(R1)),add_env(Id,T).
  e(eassign(E1,E2),R1) :- !,e(E2,R1),arr(E1,R2),add(vstore(R1,R2)).
  e(E,R2) :- (E=eid(_);E=earray(_,_);E=efield(_,_);E=eptr(_)),!,
             arr(E,R1),emit:t(R1,T1),cut(T1,T2),genreg(T2,R2),add(vload(R2,R1)).
  e(eref(E),R1) :- arr(E,R1).
  e(E,_) :- writeln(error(compile:e(E))),halt(-1).
:- end(compile).
:- start(emit,[emit/2]).
  t(rl(T,_),T).
  t(rn(T,_),T).
  pt(R,X) :- t(R,T),!,pt(T,X).
  pt(ti(I),X) :- format(atom(X),'i~w',[I]).
  pt(tv,void).
  pt(tp(T),X) :- pt(T,X1),format(atom(X),'~w*',[X1]).
  pt(tarr(T,Size),X) :- pt(T,X1),format(atom(X),'[~w x ~w]',[Size,X1]).
  pt(tstr(Ls),X) :- compile:str(tstr(Ls),R),p(R,X).
  p(A,A) :- atom(A),!.
  p(rl(_,Id),X) :- format(atom(X),'%~w',[Id]).
  p(rn(_,Id),Id).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,add,A,B)) :- t(A,tp(T)),asm('\t~w = getelementptr inbounds ~w,~w ~w,~w ~w',[p(Id),pt(T),pt(A),p(A),pt(B),p(B)]),!.
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w ~w ~w,~w',[p(Id),p(Op),pt(A),p(A),p(B)]),!.
  out(vprint(A)) :- asm('\tcall void @print_l(~w ~w)',[pt(A),p(A)]).
  out(valloca(R)) :- asm('\t~w = alloca ~w',[p(R),pt(R)]).
  out(vload(R1,R2)) :- t(R2,tp(tarr(T,N))),asm('\t~w = getelementptr inbounds ~w,~w ~w,i32 0,i32 0',[p(R1),pt(tarr(T,N)),pt(R2),p(R2)]).
  out(vload(R1,R2)) :- asm('\t~w = load ~w,~w ~w',[p(R1),pt(R1),pt(R2),p(R2)]).
  out(vstore(R1,R2)) :- asm('\tstore ~w ~w,~w ~w',[pt(R1),p(R1),pt(R2),p(R2)]).
  out(vfield(R1,Addr,Zero,A)) :- t(Addr,tp(T)),asm('\t~w = getelementptr inbounds ~w,~w ~w,~w ~w,~w ~w',
                                  [p(R1),pt(T),pt(Addr),p(Addr),pt(Zero),p(Zero),pt(A),p(A)]).
  out(vcomment(S)) :- asm('\t; ~w',[p(S)]).
  out(V) :- writeln(error:out(V)),halt.
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
  strs :- forall(compile:str(tstr(Ls),R),(
            maplist([_:T,A]>>pt(T,A),Ls,As),atomic_list_concat(As,',',A),
            asm('~w = type { ~w }',[p(R),p(A)])
          )).
  emit(File,Vs) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (strs,entry,maplist(out,Vs),leave),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile(eblock([
    evar(ar,tarr(ti(64),10)),
    eassign(earray(eid(ar),eint(0)),eint(100)),
    eassign(earray(eid(ar),eint(1)),eint(200)),
    eprint(earray(eid(ar),eint(0))),
    evar(a,ti(64)),
    evar(b,ti(64)),
    evar(c,ti(64)),
    evar(p,tp(ti(64))),
    eassign(eid(p),eref(eid(c))),
    eassign(eid(a),eint(200)),
    eassign(eid(p),eadd(eid(p),eint(2))),
    eprint(earray(eid(p),eint(0))),
    eassign(earray(eid(p),eint(0)),eint(300)),
    eprint(eid(a)),
    eprint(eptr(eid(p))),
    eassign(earray(eid(p),eint(0)),eint(400)),
    eprint(eid(a)),
    eassign(eid(p),eid(ar)),
    eprint(eptr(eid(p))),
    eassign(eid(p),eref(earray(eid(ar),eint(1)))),
    eprint(eptr(eid(p)))
  ]),Codes),!,
  emit('l10.ll',Codes),!,
  shell('llc l10.ll -o l10.s'),
  shell('gcc -static l10.s -o l10.exe'),
  shell('./l10.exe').
:-halt.
