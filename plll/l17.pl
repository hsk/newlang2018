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
  t ::= tv | ti(i) | tp(t) | tarr(t,i) | tstr((id:t)*) | tname(id) | tfun(t*,t)
      | tvariant((id:tstr((id:t)*))*).
  e ::= eint(ti(i),i) | eadd(e,e) | emul(e,e) | eprint(e) | eblock(e*)
      | evar(id,t,e) | eid(id) | eassign(e,e) | earray(e,e) | efield(e,id)
      | eref(e) | eptr(e) | ecall(e,e*) | null | etuple(e*) | eif(e,e,e)
      | etag(id,e*) | ecast(t,e) | ebin(e,id,e) | eswitch(e,(e:e)*) | eunit.
  g ::= eassign(eid(id),etyp(t)) | eassign(eid(id),efun((id:t)*,t,e)).
  r ::= rl(t,id) | rn(t,i) | rg(t,id).
  v ::= vprint(r) | vbin(r,id,r,r) | valloca(r) | vload(r,r) | vstore(r,r) | vfield(r,r,r,r)
      | vcomment(id) | vfun(id,(id:t)*,t,v*) | vret(r) | vcall(r,r,r*)
      | vjne(r,id,id) | vgoto(id) | vlabel(id) | vphi(r,id,id,t,r,r) | vbitcast(r,r).
:- end(syntax).
:- begin(env,[resetid/0,genid/2,genreg/2,t/2,id/2,cut_t/2,env/2,str/2]).
  t(rl(T,_),T).
  t(rn(T,_),T).
  t(rg(T,_),T).
  id(rl(_,Id),Id).
  id(rn(_,Id),Id).
  id(rg(_,Id),Id).
  resetid    :- retractall(id(_)),assert(id(0)),dynamic(env/2),dynamic(str/2).
  genid(S,A) :- retract(id(C)),C1 is C+1,assert(id(C1)),format(atom(A),'~w~w',[S,C]).
  genreg(T,rl(T,Id)) :- genid('..',Id).
  add(Id,T) :- assert(env(Id,T)),add_str(T,Id,false).
  add(Id,T,D) :- assert(env(Id,T)),add_str(T,Id,D).
  cut_t(tp(T),tp(T2)) :- cut_t(T,T2).
  cut_t(tname(T),T3) :- env(T,T2),cut_t(T2,T3).
  cut_t(T,T).
  add_str(T,Id) :- add_str(T,Id,true).
  add_str(tstr(Ls),Id,D) :- T=tstr(Ls),(str(T,_);D=true->assert(str(T,rl(T,Id)));genreg(T,R),assert(str(T,R))),
                         forall(member(_:T1,Ls),add_str(T1,Id,false)).
  add_str(tarr(T,_),Id,D) :- add_str(T,Id,D).
  add_str(tvariant(Ls),_,_) :- variantInfo(tvariant(Ls),_).
  add_str(_,_,_).
  size(tstr(M),S) :- genid('.',Id),add_str(tstr(M),Id),
                     foldl([_:T,S1,S2]>>(size(T,TS),S2 is S1+TS),M,0,S).
  size(ti(N),S) :- S is (N+7) div 8.
  size(tv,0).
  size(tp(_),8).
  size(tfun(_,_),8).
  size(tvariant(M),S) :- variantInfo(tvariant(M),(_,M)),size(M,S).
  variantInfo(tvariant(Ls),(T,Maxt)) :- foldl([_:tstr(M),(N,T),(N2,T2)]>>(
                                            VT2=tstr(['__tagIndex':ti(32)|M]),size(VT2,SizeVT),
                                            (SizeVT > N -> (N2,T2)=(SizeVT,VT2) ; (N2,T2)=(N,T))
                                        ),Ls,(0,null),(_,Maxt)),str(Maxt,T).
  variant(V,S) :- variantInfo(V,(S,_)).
  variantTagIdxAndStr(TagId, Ls,(N,tstr(['__tagIndex':ti(32)|M]),tstr(M))) :- nth0(N,Ls,TagId:tstr(M)).
  push :- findall(env(Id,T),env(Id,T),Envs),asserta(stack(Envs)).
  pop :- retract(stack(Envs)),retractall(env(_,_)),forall(member(Env,Envs),assert(Env)).
:- end(env).
:- begin(compile,[compile/2]).
  add(V) :- assert(v(V)).
  cut(T,T2) :- cut_t(T,T1),cut1(T1,T2).
  cut1(tp(tarr(T,_)),tp(T)).
  cut1(tp(T),T).
  arr(eid(Id),rl(tp(T),Id)) :- !,env(Id,T).
  arr(earray(Id,E),R4) :- !,e(E,R1),R2=rn(ti(64),0),arr(Id,R3),
                          t(R3,T3),cut_t(T3,T2),cut(T3,T4),genreg(T4,R4),
                          (T2=tp(tp(_)) -> genreg(T4,R5),add(vload(R5,R3)),add(vbin(R4,add,R5,R1))
                          ; add(vfield(R4,R3,R2,R1))).
  arr(eptr(Id),R1) :- arr(earray(Id,eint(ti(64),0)),R1).
  arr(efield(Id,Idx),R4) :- arr(Id,R3),t(R3,tp(T)),cut_t(T,T1),
                            (T1=tstr(M) -> member(Idx:T2,M),
                              index(T,Idx,N),R1=rn(ti(32),N),R2=rn(ti(64),0),genreg(tp(T2),R4),
                              add(vfield(R4,R3,R2,R1))
                            ;T1=tp(tstr(M)) -> arr(efield(eptr(Id),Idx),R4)
                            ;throw(error:T)).
  arr(ecast(T,E1),R3) :- arr(E1,R1),genreg(T,R2),add(vbitcast(R2,R1)),genreg(tp(T),R3),
                         id(R3,Id),add(valloca(rl(T,Id))),add(vstore(R2,R3)).
  arr(eref(E1),R1) :- arr(E1,R1).
  arr(E,_) :- findall(env(Id,T),env(Id,T),Vs),throw(error:arr(E);Vs).
  index(T,Id,N) :- cut_t(T,T1),index1(T1,Id,N).
  index1(tstr(Ls),Id,N) :- !,nth0(N,Ls,Id:_).
  index1(_,_,_) :- throw(error).
  setAssign(E1,T,E) :- cut_t(T,T1),setAssign1(E1,T1,E).
  setAssign1(E1,tstr(Types),etuple(Ls)) :- maplist([E,N:T]>>(E=etuple(_)->setAssign(efield(E1,N),T,E)
                                                            ;e(eassign(efield(E1,N),E),_)),Ls,Types).
  setAssign1(_,_,null).
  setAssign1(E1,tvariant(Types),etag(TagId, Ls)) :-
    env:variantTagIdxAndStr(TagId, Types,(TagIdx, StT, _)),
    e(ecast(tp(StT), eref(E1)),StR),id(StR,StRId),
    env:add(StRId, StT),StT=tstr(M),
    maplist([E,Id:_]>>e(eassign(efield(eid(StRId), Id), E),_),[eint(ti(32),TagIdx)|Ls],M).
  setAssign1(_,T,E) :- throw(error:setAssign(T,E)).
  e(eint(T,I),rn(T,I)).
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),t(R1,T1),genreg(T1,R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),t(R1,T1),genreg(T1,R3),add(vbin(R3,mul,R1,R2)).
  e(ebin(E1,Op,E2),R3) :- e(E1,R1),e(E2,R2),t(R1,T1),genreg(T1,R3),add(vbin(R3,Op,R1,R2)).
  e(eswitch(E1,Ps),R) :- pattern(eswitch(E1,Ps),R).
  e(eunit,tn(tv,null)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(tv,void),R).
  e(eprint(E1),rn(tv,void)) :- !,e(E1,R1),add(vprint(R1)).
  e(evar(Id,T,E),R1) :- R1=rl(T,Id),add(valloca(R1)),env:add(Id,T),
                        (E=null;e(eassign(eid(Id),E),_)).
  e(ecast(T,E1),R2) :- e(E1,R1),genreg(T,R2),add(vbitcast(R2,R1)).
  e(eassign(E1,E2),R1) :- arr(E1,R2),t(R2,T2),cut_t(T2,T3),
                          (T3=tp(T),(T=tstr(_);T=tvariant(_)) -> setAssign(E1,T,E2),R1=R2
                          ; e(E2,R1),add(vstore(R1,R2))).
  e(E,R2) :- (E=eid(_);E=earray(_,_);E=efield(_,_);E=eptr(_)),!,
             arr(E,R1),t(R1,T1),cut(T1,T2),
             (T2=tfun(_,_)->id(R1,Id),R2=rg(T2,Id) ; genreg(T2,R2),add(vload(R2,R1))).
  e(eref(E),R1) :- arr(E,R1).
  e(ecall(E1,Es),R0) :- e(E1,R1),maplist(e,Es,Rs),t(R1,T1),
                        cut_t(T1,tfun(_,T)),genreg(T,R0),add(vcall(R0,R1,Rs)).
  e(eif(A,B,C),R2) :-
    genid(then,L0),genid(else,L1),genid(endif,L2),
    e(A,R),add(vjne(R,L0,L1)),% cond
    add(vlabel(L0)),e(B,R0),add(vgoto(L2)),% then
    add(vlabel(L1)),e(C,R1),add(vgoto(L2)),% else
    add(vlabel(L2)),(t(R0,T0),t(R1,T1),T0\=tv,T0=T1 ->
                     genreg(T0,R2),add(vphi(R2,L0,L1,T0,R0,R1));R2=rn(tv,null)).
  e(E,_) :- writeln(error(compile:e(E))),halt(-1).
  pattern(eswitch(E1,Ptns),R) :-
    e(E1,R1),t(R1,T1),cut_t(T1,tvariant(Vls)),genid('..',TagId),reverse(Ptns,RPtns),
    e(evar(TagId,ti(32),eptr(ecast(tp(ti(32)),eref(E1)))),_),!,
    foldl([etag(Id,Ptns1):BodyE,E,eif(ebin(eint(ti(32),PtnTagIdx),eq,eid(TagId)),eblock(B),E)]>>(
      env:variantTagIdxAndStr(Id,Vls,(PtnTagIdx,PtnStTIdx,tstr(M))),
      maplist([eid(A),N:T,evar(A,T,efield(ecast(tp(PtnStTIdx),eref(E1)),N))]>>!,Ptns1,M,BindEs),
      append(BindEs,[BodyE],B),!
    ),RPtns,eunit,IfE),!,e(IfE,R).
  g(eassign(eid(A),efun(Prms,T,Body))) :-
    env:push,findall(T,(member(S:T,Prms),env:add(S,T)),Ts),e(Body,R),cut_t(T,T1),
    (T1=tv -> add(vret(rn(ti(32),0))),T2=ti(32) ; add(vret(R)),T2=T1),
    env:pop,env:add(A,tfun(Ts,T),true),findall(V,retract(v(V)),Vs),
    assert(func(vfun(A,Prms,T2,Vs))).
  g(eassign(eid(S),etyp(T))) :- env:add(S,T,true).
  compile(Es,Fs) :- syntax(g*,Es),resetid,forall(member(E,Es),g(E)),findall(F,func(F),Fs).
:- end(compile).
:- begin(emit,[emit/2]).
  pt(R,X) :- t(R,T),!,cut_t(T,T1),pt1(T1,X).
  pt(T,X) :- cut_t(T,T1),pt1(T1,X).
  pt1(ti(I),X) :- format(atom(X),'i~w',[I]).
  pt1(tv,void).
  pt1(tp(T),X) :- pt(T,X1),format(atom(X),'~w*',[X1]).
  pt1(tarr(T,Size),X) :- pt(T,X1),format(atom(X),'[~w x ~w]',[Size,X1]).
  pt1(tstr(Ls),X) :- str(tstr(Ls),R),p(R,X).
  pt1(tname(N),X) :- format(atom(X),'%~w',[N]).
  pt1(tvariant(Ls),X) :- env:variant(tvariant(Ls),R),p(R,X).
  p(A,A) :- atom(A),!.
  p(rl(_,Id),X) :- format(atom(X),'%~w',[Id]).
  p(rg(_,Id),X) :- format(atom(X),'@~w',[Id]).
  p(rn(_,Id),Id).
  asm(S)              :- fp(FP),writeln(FP,S).
  asm(S,F)            :- fp(FP),maplist(call,F,F_),format(FP,S,F_),nl(FP).
  out(vbin(Id,add,A,B)) :- t(A,tp(T)),asm('\t~w = getelementptr inbounds ~w,~w ~w,~w ~w',[p(Id),pt(T),pt(A),p(A),pt(B),p(B)]),!.
  out(vbin(Id,Op,A,B)) :- member(Op,[eq,ne]),!,genreg(ti(1),R1),
                          asm('\t~w = icmp ~w ~w ~w,~w',[p(R1),p(Op),pt(A),p(A),p(B)]),
                          asm('\t~w = zext i1 ~w to ~w',[p(Id),p(R1),pt(Id)]).
  out(vbin(Id,Op,A,B)) :- member(Op,[lt,le,gt,ge]),!,genreg(ti(1),R1),
                          asm('\t~w = icmp s~w ~w ~w, ~w',[p(R1),p(Op),pt(A),p(A),p(B)]),
                          asm('\t~w = zext i1 ~w to ~w',[p(Id),p(R1),pt(Id)]).
  out(vbin(Id,Op,A,B)) :- asm('\t~w = ~w ~w ~w,~w',[p(Id),p(Op),pt(A),p(A),p(B)]),!.
  out(vprint(A)) :- asm('\tcall void @print_l(~w ~w)',[pt(A),p(A)]).
  out(valloca(R)) :- asm('\t~w = alloca ~w',[p(R),pt(R)]).
  out(vload(R1,R2)) :- t(R2,tp(tarr(T,N))),asm('\t~w = getelementptr inbounds ~w,~w ~w,i32 0,i32 0',[p(R1),pt(tarr(T,N)),pt(R2),p(R2)]).
  out(vload(R1,R2)) :- asm('\t~w = load ~w,~w ~w',[p(R1),pt(R1),pt(R2),p(R2)]).
  out(vstore(R1,R2)) :- asm('\tstore ~w ~w,~w ~w',[pt(R1),p(R1),pt(R2),p(R2)]).
  out(vfield(R1,Addr,Zero,A)) :- t(Addr,tp(T)),asm('\t~w = getelementptr inbounds ~w,~w ~w,~w ~w,~w ~w',
                                  [p(R1),pt(T),pt(Addr),p(Addr),pt(Zero),p(Zero),pt(A),p(A)]).
  out(vcomment(S)) :- asm('\t; ~w',[p(S)]).
  out(vcall(A,B,C)):- maplist([A1,A2]>>(pt(A1,T),p(A1,X),format(atom(A2),'~w ~w',[T,X])),C,Cs),
                      atomic_list_concat(Cs,',',S),
                      asm('\t~w = call ~w ~w(~w)',[p(A),pt(A),p(B),p(S)]).
  out(vret(R1)) :- asm('\tret ~w ~w',[pt(R1),p(R1)]).
  out(vjne(R,J1,J2)) :-         genid('%reg_',R1),
                                asm('\t~w = icmp ne ~w ~w,0',[p(R1),pt(R),p(R)]),
                                asm('\tbr i1 ~w,label %~w,label %~w',[p(R1),p(J1),p(J2)]).
  out(vgoto(J)) :-              asm('\tbr label %~w',[p(J)]).
  out(vlabel(L)) :-             asm('~w:',[p(L)]).
  out(vphi(R,L1,L2,T,R1,R2)) :- asm('\t~w = phi ~w[~w,%~w],[~w,%~w]',[p(R),pt(T),p(R1),p(L1),p(R2),p(L2)]).
  out(vbitcast(D,S)) :- asm('\t~w = bitcast ~w ~w to ~w',[p(D),pt(S),p(S),pt(D)]).
  out(V) :- writeln(error:out(V)),halt.
  printl :- asm('@.str = private constant [5 x i8] c"%ld\\0A\\00"'),
            asm('define void @print_l(i64 %a) {'),
            asm('entry:'),
            asm('\t%0 = call i32 (i8*,...) @printf(i8* ~w,i64 %a)',
                [p('getelementptr inbounds ([5 x i8],[5 x i8]* @.str,i32 0,i32 0)')]),
            asm('\tret void'),
            asm('}'),
            asm('declare i32 @printf(i8*,...)').
  strs :- forall(str(tstr(Ls),R),(
            maplist([_:T,A]>>pt(T,A),Ls,As),atomic_list_concat(As,',',A),
            asm('~w = type { ~w }',[p(R),p(A)])
          )).
  func(vfun(A,Prms,T,Vs)) :-
    maplist([A1:B,R]>>(pt(B,T1),format(atom(R),'~w %.~w',[T1,A1])),Prms,Prms2),
    atomic_list_concat(Prms2,',',Prms3),
    asm('define ~w @~w(~w) {',[pt(T),p(A),p(Prms3)]),
    asm('entry:'),
    forall(member(S:T,Prms),(
      out(valloca(rl(T,S))),atom_concat('.',S,S1),out(vstore(rl(T,S1),rl(tp(T),S)))
    )),
    maplist(out,Vs),
    asm('}').
  emit(File,Fs) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (strs,maplist(func,Fs),printl),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile([
    eassign(eid('Data'),etyp(
      tvariant([
        'A':tstr([a:ti(64)]),
        'B':tstr([a:ti(64),b:ti(64)])
      ])
    )),
    eassign(eid(main),efun([],tv,eblock([
      evar(data,tname('Data'),etag('A',[eint(ti(64),555)])),
      eswitch(eid(data),[
        etag('A',[eid(a1)]):         eprint(eid(a1)),
        etag('B',[eid(a2),eid('_')]):eprint(eid(a2))
      ]),
      evar(data2,tname('Data'),etag('B',[eint(ti(64),1),eint(ti(64),2)])),
      eswitch(eid(data2),[
        etag('A',[eid(a3)]):        eprint(eid(a3)),
        etag('B',[eid(a4),eid(b4)]):eprint(eadd(eid(a4),eid(b4)))
      ])
    ])))
  ],Codes),!,
  syntax(v*,Codes),
  emit('l17.ll',Codes),!,
  shell('llc l17.ll -o l17.s'),
  shell('gcc -static l17.s -o l17.exe'),
  shell('./l17.exe').
:-halt.
