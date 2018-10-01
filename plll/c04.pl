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
  t ::= tv | ti(i) | tp(t) | tfun(t*,t).
  e ::= eint(ti(i),i) | eadd(e,e) | emul(e,e) | eprint(e) | eblock(e*)
      | evar(id,t,e) | eid(id) | ecall(e,e*).
  g ::= eassign(eid(id),efun((id:t)*,t,e)).
  r ::= rl(t,id) | rn(t,i) | rg(t,id).
  v ::= vprint(r) | vbin(r,id,r,r)
      | vfun(id,(id:t)*,t,v*) | vret(r) | vcall(r,r,r*).
:- end(syntax).
:- begin(env,[resetid/0,genid/2,genreg/2,t/2,id/2,cut_t/2,env/2]).
  t(rl(T,_),T).
  t(rn(T,_),T).
  t(rg(T,_),T).
  id(rl(_,Id),Id).
  id(rn(_,Id),Id).
  id(rg(_,Id),Id).
  resetid    :- retractall(id(_)),assert(id(0)),dynamic(env/2).
  genid(S,A) :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'~w~w',[S,C]).
  genreg(T,rl(T,Id)) :- genid('..',Id).
  push :- findall(env(Id,T),env(Id,T),Envs),asserta(stack(Envs)).
  pop :- retract(stack(Envs)),retractall(env(_,_)),forall(member(Env,Envs),assert(Env)).
  add(Id,T) :- assert(env(Id,T)).
:- end(env).
:- begin(compile,[compile/2]).
  add(V) :- assert(v(V)).
  arr(eid(Id),R) :- env(Id,R).
  arr(_,_) :- throw(error).
  e(eint(T,I),rn(T,I)).
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),t(R1,T1),genreg(T1,R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),t(R1,T1),genreg(T1,R3),add(vbin(R3,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(tv,void),R).
  e(eprint(E1),rn(tv,void)) :- e(E1,R1),add(vprint(R1)).
  e(evar(Id,_,E1),R1) :- e(E1,R1),env:add(Id,R1).
  e(eid(Id),R1) :-  arr(eid(Id),R1).
  e(ecall(E1,Es),R0) :- e(E1,R1),maplist(e,Es,Rs),t(R1,tfun(_,T)),genreg(T,R0),add(vcall(R0,R1,Rs)).
  g(eassign(eid(A),efun(Prms,T,Body))) :-
    findall(T,member(_:T,Prms),Ts),env:add(A,rg(tfun(Ts,T),A)),
    env:push,forall(member(S:T,Prms),env:add(S,rl(T,S))),e(Body,R),
    (T=tv -> add(vret(rn(ti(32),0))),T2=ti(32) ; add(vret(R)),T2=T),
    env:pop,findall(V,retract(v(V)),Vs),assert(func(vfun(A,Prms,T2,Vs))).
  compile(Es,Fs) :- syntax(g*,Es),resetid,forall(member(E,Es),g(E)),findall(F,func(F),Fs).
:- end(compile).
:- begin(emit,[emit/2]).
  pt(R,X) :- t(R,T),!,pt(T,X).
  pt(ti(I),X) :- format(atom(X),'i~w',[I]).
  pt(tv,void).
  pt(tp(T),X) :- pt(T,X1),format(atom(X),'~w*',[X1]).
  p(A,A) :- atom(A),!.
  p(rl(_,Id),X) :- format(atom(X),'%~w',[Id]).
  p(rg(_,Id),X) :- format(atom(X),'@~w',[Id]).
  p(rn(_,Id),Id).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w ~w ~w,~w',[p(Id),p(Op),pt(A),p(A),p(B)]).
  out(vprint(A)) :- asm('\tcall void @print_l(~w ~w)',[pt(A),p(A)]).
  out(vcall(A,B,C)):- maplist([A1,A2]>>(pt(A1,T),p(A1,X),format(atom(A2),'~w ~w',[T,X])),C,Cs),
                      atomic_list_concat(Cs,',',S),
                      asm('\t~w = call ~w ~w(~w)',[p(A),pt(A),p(B),p(S)]).
  out(vret(R1)) :- asm('\tret ~w ~w',[pt(R1),p(R1)]).
  out(V) :- writeln(error:out(V)),halt.
  printl :- asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
            asm('define void @print_l(i64 %a) {'),
            asm('entry:'),
            asm('\t%0 = call i32 (i8*,...) @printf(i8* ~w,i64 %a)',
                [p('getelementptr inbounds ([5 x i8],[5 x i8]* @.str,i32 0,i32 0)')]),
            asm('\tret void'),
            asm('}'),
            asm('declare i32 @printf(i8*,...)').
  func(vfun(A,Prms,T,Vs)) :-
    maplist([A1:B,R]>>(pt(B,T1),format(atom(R),'~w %~w',[T1,A1])),Prms,Prms2),
    atomic_list_concat(Prms2,',',Prms3),
    asm('define ~w @~w(~w) {',[pt(T),p(A),p(Prms3)]),
    asm('entry:'),
    maplist(out,Vs),
    asm('}').
  emit(File,Fs) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (maplist(func,Fs),printl),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile([
    eassign(eid(add),efun([a:ti(64),b:ti(64)],ti(64),eblock([
      eadd(eid(a),eid(b))
    ]))),
    eassign(eid(main),efun([],tv,eblock([
      eprint(ecall(eid(add),[eint(ti(64),3),eint(ti(64),4)]))
    ])))
  ],Codes),
  syntax(v*,Codes),
  emit('c04.ll',Codes),!,
  shell('llc c04.ll -o c04.s'),
  shell('gcc -static c04.s -o c04.exe'),
  shell('./c04.exe').
:-halt.
