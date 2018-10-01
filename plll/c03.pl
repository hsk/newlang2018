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
  e ::= eint(i) | eadd(e,e) | emul(e,e) | eblock(e*) | evar(id,e) | eid(id) | ecall(e,e*).
  g ::= eassign(eid(id),efun(id*,e)).
  r ::= rl(id) | rn(i) | rg(id).
  v ::= vbin(r,id,r,r) | vfun(id,id*,v*) | vret(r) | vcall(r,r,r*).
:- end(syntax).
:- begin(env,[resetid/0,genid/2,genreg/1,env/2]).
  resetid    :- retractall(id(_)),assert(id(0)),dynamic(env/2),add(print_l,rg(print_l)).
  genid(S,A) :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'~w~w',[S,C]).
  genreg(rl(Id)) :- genid('..',Id).
  push :- findall(env(Id,R),env(Id,R),Envs),asserta(stack(Envs)).
  pop :- retract(stack(Envs)),retractall(env(_,_)),forall(member(Env,Envs),assert(Env)).
  add(Id,R) :- assert(env(Id,R)).
:- end(env).
:- begin(compile,[compile/2]).
  add(V) :- assert(v(V)).
  e(eint(I),rn(I)).
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(R3),add(vbin(R3,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(0),R).
  e(evar(Id,E1),R1) :- e(E1,R1),env:add(Id,R1).
  e(eid(Id),R1) :-  env(Id,R1).
  e(ecall(E1,Es),R0) :- e(E1,R1),maplist(e,Es,Rs),genreg(R0),add(vcall(R0,R1,Rs)).
  g(eassign(eid(A),efun(Prms,Body))) :-
    env:add(A,rg(A)),
    env:push,forall(member(S,Prms),env:add(S,rl(S))),e(Body,R),add(vret(R)),
    env:pop,findall(V,retract(v(V)),Vs),assert(func(vfun(A,Prms,Vs))).
  compile(Es,Fs) :- syntax(g*,Es),resetid,forall(member(E,Es),g(E)),findall(F,func(F),Fs).
:- end(compile).
:- begin(emit,[emit/2]).
  p(A,A) :- atom(A),!.
  p(rl(Id),X) :- format(atom(X),'%~w',[Id]).
  p(rg(Id),X) :- format(atom(X),'@~w',[Id]).
  p(rn(Id),Id).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w i64 ~w,~w',[p(Id),p(Op),p(A),p(B)]).
  out(vcall(A,B,C)):- maplist([A1,A2]>>(p(A1,X),format(atom(A2),'i64 ~w',[X])),C,Cs),
                      atomic_list_concat(Cs,',',S),
                      asm('\t~w = call i64 ~w(~w)',[p(A),p(B),p(S)]).
  out(vret(R1)) :- asm('\tret i64 ~w',[p(R1)]).
  out(V) :- writeln(error:out(V)),halt.
  printl :- asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
            asm('define i64 @print_l(i64 %a) {'),
            asm('entry:'),
            asm('\t%0 = call i32 (i8*,...) @printf(i8* ~w,i64 %a)',
                [p('getelementptr inbounds ([5 x i8],[5 x i8]* @.str,i32 0,i32 0)')]),
            asm('\tret i64 0'),
            asm('}'),
            asm('declare i32 @printf(i8*,...)').
  func(vfun(A,Prms,Vs)) :-
    maplist([A1,R]>>(format(atom(R),'i64 %~w',[A1])),Prms,Prms2),
    atomic_list_concat(Prms2,',',Prms3),
    asm('define i64 @~w(~w) {',[p(A),p(Prms3)]),
    asm('entry:'),
    maplist(out,Vs),
    asm('}').
  emit(File,Fs) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (maplist(func,Fs),printl),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile([
    eassign(eid(add),efun([a,b],eblock([
      eadd(eid(a),eid(b))
    ]))),
    eassign(eid(main),efun([],eblock([
      ecall(eid(print_l),[ecall(eid(add),[eint(3),eint(4)])])
    ])))
  ],Codes),
  syntax(v*,Codes),
  emit('c03.ll',Codes),!,
  shell('llc c03.ll -o c03.s'),
  shell('gcc -static c03.s -o c03.exe'),
  shell('./c03.exe').
:-halt.
