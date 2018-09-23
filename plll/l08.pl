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
  cut(tp(tarr(T,_)),tp(T)).
  arr(eid(Id),rl(tp(T),Id)) :- !,env(Id,T).
  arr(earray(Id,E),R4) :- !,e(E,R1),R2=rn(ti(64),0),arr(Id,R3),
                          emit:t(R3,T),cut(T,T4),genreg(T4,R4),add(vfield(R4,R3,R2,R1)).
      case EField(id, idx) =>
        val t = env.map(id)
        val reg1 = RN(Ti(32),""+index(t, idx))
        val reg2 = RN(Ti(64), "0")
        val reg3 = arr(EId(id))
        val reg4 = RL(Tp(Ti(64)), genid(".."))
        add(VField(reg4, reg3, reg2, reg1))
        reg4
  def index(t:T, id:String):Int = {
    t match {
      case TStr(ls) =>
        ls.foldLeft(0) {
          case (n,(id1,_))=>
            if(id1 == id) return n
            n + 1
        }
      case _ => throw new Exception("Error")
    }
  }

  arr(E,_) :- findall(env(Id,T),env(Id,T),Vs),throw(error:arr(E);Vs).
  compile(E,(Ts,Vs)) :- resetid,e(E,_),findall(V,retract(v(V)),Vs),
                        setof(T,(findall(tstr(M),env(_,tstr(M)),Ts),member(T,Ts)),Ts).
  e(eint(I),rn(ti(64),I)).
  e(eadd(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(ti(64),R3),add(vbin(R3,add,R1,R2)).
  e(emul(E1,E2),R3) :- e(E1,R1),e(E2,R2),genreg(ti(64),R3),add(vbin(R3,mul,R1,R2)).
  e(eblock(Es),R) :- foldl([E,R,R1]>>e(E,R1),Es,rn(tv,void),R).
  e(eprint(E1),rn(tv,void)) :- e(E1,R1),add(vprint(R1)).
  e(evar(Id,T),R1) :- R1=rl(T,Id),add(valloca(R1)),assert(env(Id,T)).
  e(eassign(E1,E2),R1) :- e(E2,R1),arr(E1,R2),add(vstore(R1,R2)).
  e(E,R2) :- (E=eid(_);E=earray(_,_)),!,arr(E,R1),genreg(ti(64),R2),add(vload(R2,R1)).
:- end(compile).
:- start(emit,[emit/2]).
  t(rl(T,_),T).
  t(rn(T,_),T).
  pt(R,X) :- t(R,T),!,pt(T,X).
  pt(ti(I),X) :- format(atom(X),'i~w',[I]).
  pt(tv,void).
  pt(tp(T),X) :- pt(T,X1),format(atom(X),'~w*',[X1]).
  pt(tarr(T,Size),X) :- pt(T,X1),format(atom(X),'[~w x ~w]',[Size,X1]).
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
  out(vfield(R1,Addr,Zero,A)) :- t(Addr,tp(T)),asm('~w = getelementptr inbounds ~w,~w ~w,~w ~w,~w ~w',
                                  [p(R1),pt(T),pt(Addr),p(Addr),pt(Zero),p(Zero),pt(A),p(A)]).
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
  strs(Ts) :- forall(member(tstr(Ls),Ts),(
    ls.map{case(_, t)=>p(t)}.mkString(", ")
    asm('~w = type { ~w }',[p(R)])
    asm(p(r) + " =␣type␣{ " + ls.map{case(_, t)=>p(t)}.mkString(", ") + " }")
  ))

  emit(File,(Vs,Ts)) :- setup_call_cleanup(
                    (open(File,write,FP),assert(fp(FP))),
                    (entry,strs(Ts),maplist(out,Vs),leave),
                    (close(FP),retract(fp(_)))).
:- end(emit).
:-compile(eblock([
    evar(b, tstr([x:ti(64), y:ti(64)])),
    eassign(efield(b, x), eint(3)),
    eassign(efield(b, y), emul(eint(3), eint(5))),
    eprint(efield(b, x)),
    eprint(efield(b, y)),
    eprint(eadd(efield(b, x), efield(b, y)))
  ]),Codes),
  emit('l08.ll',Codes),!,
  shell('llc l08.ll -o l08.s'),
  shell('gcc -static l08.s -o l08.exe'),
  shell('./l08.exe').
:-halt.
