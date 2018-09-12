open Syntax
open Code

let rec typ = function
  | TInt -> I32
  | TBool -> I1
  | TChar -> I8
  | TPtr t -> Ptr (typ t)
  | TAbs(t,ts) -> Func(typ t,List.map typ ts)
  | TVoid -> Void

let global_vars = ref StringMap.empty
let static_vars = ref StringMap.empty
let function_decls = ref StringMap.empty

let set_globals globals =
  let global_var m (t, n) =
    StringMap.add n (IInt(typ t, 0)) m
  in
  global_vars := List.fold_left global_var StringMap.empty globals

let set_func_decls funs =
  let function_decl m fdecl =
    let name = fdecl.fname in
    let formal_types = List.map (fun (t,_) -> typ t) fdecl.formals in
    let ftype = Func(typ fdecl.typ, formal_types) in
    StringMap.add name ftype m
  in
  function_decls := List.fold_left function_decl StringMap.empty funs

let local_vars = ref StringMap.empty
let lookup n =
  try StringMap.find n !local_vars
  with Not_found -> StringMap.find n !global_vars

let set_local_vars fdecl =
  let add_formal m (t, n) =
    let t = typ t in
    let r = new_reg() in
    add_ir(IAlloca(r,Ptr t));
    add_ir(IStore(IReg(t,n),IReg(Ptr t,r)));
    StringMap.add n (IReg(Ptr t,r)) m
  in
  let add_local m (t, n) =
    let t = typ t in
    let r = new_reg() in
    add_ir(IAlloca(r,Ptr t));
    StringMap.add n (IReg(Ptr t,r)) m
  in
  let formals = List.fold_left add_formal StringMap.empty fdecl.formals in
  local_vars := List.fold_left add_local formals fdecl.locals

let rec expr = function
  | EInt i -> IInt(I32, i)
  | EBool b -> IInt(I1, if b then 1 else 0)
  | ENop -> IInt(I32, 0)
  | EId s ->
    (match lookup s with
    | IReg(t,r) ->
      add_ir(ILoad(s, IReg(t,r)));
      IReg(t,s)
    | _ -> failwith "lookup type error"
    )
  | EBin (e1, EOr, e2) ->
      let bb = new_reg() in
      let set0 = new_reg() in
      let set1 = new_reg() in
      let last = new_reg() in
      let r1 = expr e1 in
      add_ir(IBne(r1, set1, bb));
      add_label(bb);
      let r2 = expr e2 in
      add_ir(IBne(r2, set1, set0));
      add_label(set0);
      let r = new_reg () in
      add_ir(IMov(r,IInt(I1,0)));
      add_ir(IBr(last));
      add_label(set1);
      add_ir(IMov(r,IInt(I1,1)));
      add_ir(IBr(last));
      add_label(last);
      IReg(I1,r)
  | EBin (e1, EAnd, e2) ->
      let bb = new_reg() in
      let set0 = new_reg() in
      let set1 = new_reg() in
      let last = new_reg() in
      let r1 = expr e1 in
      add_ir(IBne(r1, bb,set0));
      add_label(bb);
      let r2 = expr e2 in
      add_ir(IBne(r2, set1,set0));
      add_label(set0);
      let r = new_reg () in
      add_ir(IMov(r,IInt(I1,0)));
      add_ir(IBr(last));
      add_label(set1);
      add_ir(IMov(r,IInt(I1,1)));
      add_ir(IBr(last));
      add_label(last);
      IReg(I1,r)
  | EBin (e1, op, e2) ->
      let i1 = expr e1 in
      let i2 = expr e2 in
      let r = new_reg () in
      begin match op with
        | EAdd -> add_ir (IBin(r,i1,IAdd,i2))
        | ESub -> add_ir (IBin(r,i1,ISub,i2))
        | EMul -> add_ir (IBin(r,i1,IMul,i2))
        | EDiv -> add_ir (IBin(r,i1,ISDiv,i2))
        | EAnd -> add_ir (IBin(r,i1,IAnd,i2))
        | EOr  -> add_ir (IBin(r,i1,IOr,i2))
        | EEq  -> add_ir (ICmp(r,i1,IEq,i2))
        | ENe  -> add_ir (ICmp(r,i1,INe,i2))
        | ELt  -> add_ir (ICmp(r,i1,ISLt,i2))
        | ELe  -> add_ir (ICmp(r,i1,ISLe,i2))
        | EGt  -> add_ir (ICmp(r,i1,ISGt,i2))
        | EGe  -> add_ir (ICmp(r,i1,ISGe,i2))
      end;
      IReg((get_t i1),r)
  | EUn(op, e) ->
      let i = expr e in
      let r = new_reg () in
      (match op with
      | ENeg     -> add_ir(IUn(r,INeg, i))
      | ENot     -> add_ir(IUn(r,INot, i))
      );
      IReg((get_t i),r)
  | EAssign (s, e) ->
    let i = expr e in
    add_ir (IStore(i, lookup s));
    i
  | ECall ("print", [e]) | ECall ("printb", [e]) ->
      let r = new_reg () in
      let s = new_reg () in
      static_vars := StringMap.add "%d\n" s !static_vars;
      add_ir( ICall(r, ("printf", Func(I32, [ Ptr I8 ])), [ IStr "%d\n" ; expr e ]));
      IReg(I32, r)
  | ECall ("printbig", [e]) ->
      let r = new_reg () in
      add_ir(ICall(r,("printbig", Func(I32, [ I32 ])),[ expr e ]));
      IReg(I32, r)
  | ECall (f, act) ->
      let fdef = StringMap.find f !function_decls in
      let actuals = List.rev (List.map expr (List.rev act)) in
      let (ft,r) =
        match fdef with
        | Func(Void,_) -> (Void,"")
        | Func(t,_)-> (t,new_reg())
        | _ -> failwith "type error"
      in
      add_ir(ICall(r,(f,fdef),actuals));
      IReg(ft,r)

let stmt fdecl =
  let rec stmt = function
    | SBlock sl -> List.iter stmt sl
    | SExpr e -> ignore (expr e)
    | SReturn e -> (match fdecl.typ with
      | TVoid -> add_ir(IRetVoid)
      | _     -> add_ir(IRet(expr e)))
    | SIf (predicate, then_stmt, else_stmt) ->
      let bool_val = expr predicate in
      let merge_bb = new_reg() in
      let then_bb = new_reg() in
      let else_bb = new_reg() in
      add_ir(IBne(bool_val, then_bb, else_bb));
      add_label(then_bb);
      ignore(stmt then_stmt);
      add_ir(IBr merge_bb);
      add_label(else_bb);
      ignore(stmt else_stmt);
      add_ir(IBr merge_bb);
      add_label(merge_bb)
    | SWhile (e1, body) ->
      let pred_bb = new_reg() in
      let body_bb = new_reg() in
      let merge_bb = new_reg() in
      add_ir(IBr(pred_bb));
      add_label(pred_bb);
      let bool_val = expr e1 in
      add_ir(IBne(bool_val, body_bb, merge_bb));
      add_label(body_bb);
      stmt body;
      add_ir(IBr(pred_bb));
      add_label(merge_bb)
    | SFor (e1, e2, e3, body) ->
      ignore(expr e1);
      stmt(SWhile (e2, SBlock[body; SExpr e3]))

  in
  stmt

let func fdecl =
  let func = StringMap.find fdecl.fname !function_decls in
  init_bbs(new_reg());
  set_local_vars fdecl;
  List.iter (stmt fdecl) fdecl.body;
  (fdecl.fname,func,get_bbs ())

let compile (globals, funcs) =
  static_vars := StringMap.empty;
  init_cnt ();
  set_globals globals;
  set_func_decls funcs;
  (StringMap.bindings !global_vars, StringMap.bindings !static_vars, List.map func funcs)
(* imm StringMap.t * func *)
