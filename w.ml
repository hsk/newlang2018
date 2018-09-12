(* Welsh-Powell-Algorithm *)
open Printf
module M = struct
  include Map.Make(String)
  let of_list ls = List.fold_left (fun m (k,v)-> add k v m) empty ls
end
module S = Set.Make(String)

module Graph = struct
  let coloring ns =
    let cmp (_,x) (_,y) = S.cardinal y - S.cardinal x in
    let ns = List.sort cmp (M.bindings ns) in
    let rec loop = function
      | (cs,c,[]) -> cs
      | (cs,c,(a,e)::i) ->
        if M.mem a cs then loop (cs,c,i) else
        loop (List.fold_left (fun cs (a,e1) ->
          if M.mem a cs || S.mem a e || S.exists(fun v->Some c=M.find_opt v cs) e1 then cs
          else M.add a c cs
        ) (M.add a c cs) i,c+1,i)
    in
    loop (M.empty,0,ns)
  let neighbors es =
    let add x y ns = match M.find_opt x ns with
      | None -> M.add x (S.singleton y) ns
      | Some xs -> M.add x (S.add y xs) ns
    in
    List.fold_left (fun ns (x,y) ->
      (add y x (add x y ns)) 
    ) M.empty es
  let dot cs es =
    let dot1(k,c) = sprintf "  %s [label=\"%s r%d\"]\n" k k c in
    let dot2(a,b) = sprintf "  %s -- %s\n" a b in
    "graph G{\n" ^
    String.concat "" (List.map dot1 (M.bindings cs)) ^
    String.concat "" (List.map dot2 es) ^ "}"
  let run es =
    let r = dot (coloring (neighbors es)) es in
    let fp = open_out "wml.dot" in fprintf fp "%s\n" r; close_out fp
  (*
  let _ =
    run ["a","f";"f","e";"f","m";"f","j";"f","z";"f","b";"f","c";"e","z";"e","h";
        "e","b";"e","m";"j","k";"j","d";"j","h";"j","g";"j","z";"j","b";"j","c";
        "k","b";"k","d";"k","g";"k","z";"k","b";"k","c";"b","m";"b","d";"b","c";
        "m","c";"m","d";"h","g"]
  let _ =
    run["x","1";"y","2";"4","1";"5","2";"6","4";"6","5";"4","5";"8","1";"9","8";
        "18","1";"11","1";"12","11";"12","1";"14","1";"15","14";"15","1"]
  *)
  type bb_io = {
    inp : S.t;
    out : S.t;
    block: (string list * string list) list;
    br : string list;
  }
  let rec last = function
    | [] -> raise Not_found
    | [a] -> a
    | x::xs -> last xs

  let rec liveness g =
    let g2 = List.map(fun (k,{inp;out;block;br}) ->
      let out,inp = List.fold_left(fun (out,inp) b ->
          S.fold(fun i (out,inp) -> (S.add i out,S.add i inp)) (List.assoc b g).inp (out,inp)
      ) (out,inp) br in
      let out,inp = if br<>[] || block=[] then out,inp else
        List.fold_left(fun (out,inp) i -> (S.add i out,S.add i inp)) (out,inp) (snd(last block))
      in
      let inp = List.fold_left(fun vinp (out,inp) ->
        S.diff (S.union (S.of_list inp) vinp) (S.of_list out)
      ) inp (List.rev block) in
      (k,{inp;out;block;br})
    ) g in
    if g = g2 then g else liveness g2

  let gen_edges g =
    let addEs x y es =
      if x=y then es else
      if List.exists(fun (a,b)->x=a&&y=b||x=b&&y=a) es then es else
      (x,y)::es
    in
    let rec add es = function
      | [] -> es
      | x::xs -> add (List.fold_left(fun es y->addEs x y es) es xs) xs
    in
    List.fold_left(fun es (k,{out;block})->
      let es = add es (S.elements out) in
      let live = out in
      let (live,es) = List.fold_left (fun (live,es) (out,inp)->
        let es = add es (out@inp) in
        let live = List.fold_left (fun s o-> S.remove o s) live out in
        let es = add es (inp @ S.elements live) in
        let live = List.fold_left(fun s v -> S.add v s) live inp in
        (live,es)
      ) (live,es) (List.rev block) in
      es
    ) [] g

  let run_bb g = run (gen_edges (liveness g))
  (*
  let _ = run_bb [
      "0", {inp=S.empty;out=S.empty;block=[
        ["1"],[];
        ["2"],[];
        [],["x";"1"];
        [],["y";"2"];
        [],[];
      ];br=["3"]};
      "3", {inp=S.empty;out=S.empty;block=[
        ["4"],["1"];
        ["5"],["2"];
        ["6"],["4";"5"];
        [],["6"];
      ];br=["7";"17"]};
      "7", {inp=S.empty;out=S.empty;block=[
        ["8"],["1"];
        ["9"],["8"];
        [],["9"];
      ];br=["10";"13"]};
      "10", {inp=S.empty;out=S.empty;block=[
        ["11"],["1"];
        ["12"],["11"];
        [],["12";"1"];
        [],[];
      ];br=["16"]};
      "13", {inp=S.empty;out=S.empty;block=[
        ["14"],["1"];
        ["15"],["14"];
        [],["15";"1"];
        [],[];
      ];br=["16"]};
      "16", {inp=S.empty;out=S.empty;block=[
        [],[];
      ];br=["3"]};
      "17", {inp=S.empty;out=S.empty;block=[
        ["18"],["1"];
        [],["18"];
      ];br=[]};
    ]
  *)
end

type e =
  | EInt of int
  | EAdd of e * e

type reg = string
type imm = IReg of reg | IInt of int
type label = string
type ir =
  | IMov of reg * imm
  | IAdd of reg * imm * imm
  | ICmp of reg * string * imm * imm
  | IRet of imm
  | IAlloca of reg
  | IBr of label
  | IBr3 of imm * label * label
  | ILoad of reg * imm
  | IStore of imm * imm
type bb = string * ir list 

module Compiler = struct

  let cnt = ref 0
  let new_reg () =
    incr cnt;
    string_of_int !cnt
  let bb_name = ref ""
  let bb = ref []
  let bbs = ref []
  let init () =
    bb_name := "";
    bb := [];
    bbs := [];
    cnt := 0
  let new_bb () =
    incr cnt;
    let label = string_of_int !cnt in
    (label,[])
  let set_bb (label,bb1) =
    bb_name := label;
    bb := bb1
  let get_bb () = (!bb_name,!bb)
  let add ir =
    bb := ir :: !bb
  let add_bb bb =
    bbs := bb :: !bbs
  let get_bbs () =
    List.fold_left(fun bbs (label,bb) ->
      (label,List.rev bb)::bbs
    ) [] !bbs
  let rec comp_e = function
    | EInt i -> IInt i
    | EAdd(e1,e2) ->
      let r1 = comp_e e1 in
      let r2 = comp_e e2 in
      let r3 = new_reg () in
      add (IAdd(r3,r1,r2));
      IReg r3
  let rec compile e =
    set_bb (new_bb());
    let r = comp_e e in
    add (IRet r);
    add_bb (get_bb());
    get_bbs()
end

module RegAlloc = struct

  let collect_io_imm1 imms = function
    | IReg a -> a::imms
    | _ -> imms

  let collect_io_imm = List.fold_left collect_io_imm1 []
  let collect_io_ir = function
    | IMov(r,i1) -> [r],collect_io_imm[i1]
    | IAdd(r,i1,i2) -> [r],collect_io_imm[i1;i2]
    | ICmp(r,_,i1,i2) -> [r],collect_io_imm[i1;i2]
    | ILoad(r,i) -> [r],collect_io_imm[i]
    | IRet(i) -> [],collect_io_imm[i]
    | IAlloca(r) -> [r],[]
    | IBr(l) -> [],[]
    | IBr3(i,l1,l2) -> [],collect_io_imm[i]
    | IStore(i1,i2) -> [],collect_io_imm[i1;i2]
  let collect_io_br = function
    | IRet(i) -> []
    | IBr(l) -> [l]
    | IBr3(i,l1,l2) -> [l1;l2]
    | _ -> assert false
  let collect_io_bbs = List.map (fun (k,bb)->
    let block = List.map collect_io_ir bb in
    let br = if bb=[] then [] else collect_io_br (Graph.last bb) in
    (k,{Graph.inp=S.empty;out=S.empty;block;br})
  )

  let alloc bbs =
    let io_bbs = collect_io_bbs bbs in
    Graph.coloring (Graph.neighbors (Graph.gen_edges (Graph.liveness io_bbs)))
end
(*
let bbs = [
"0", [
  IAlloca("1");
  IAlloca("2");
  IStore(IReg "x",IReg "1");
  IStore(IReg "y",IReg "2");
  IBr("3");
];
"3", [
  ILoad("4",IReg "1");
  ILoad("5",IReg "2");
  ICmp("6","slt",IReg "4",IReg "5");
  IBr3(IReg "6","7","17");
];
"7", [
  ILoad("8",IReg "1");
  ICmp("9","slt",IReg "8",IInt 3);
  IBr3(IReg "9","10","13");
];
"10", [
  ILoad("11",IReg "1");
  IAdd("12",IReg "11",IInt 1);
  IStore(IReg "12",IReg "1");
  IBr("16");
];
"13", [
  ILoad("14",IReg "1");
  IAdd("15",IReg "14",IInt 2);
  IStore(IReg "15",IReg "1");
  IBr("16");
];
"16", [
  IBr("3");
];
"17",[
  ILoad("18",IReg "1");
  IRet(IReg "18");
];
]
let _ =
  let b = collect_io_bbs bbs in
  run_bb b
*)
module Emit = struct
  let emit str =
    Printf.printf "\t%s\n" str
  let p str =
    Printf.printf "%s\n" str

  let regs = [|"r10d"; "r11d"; "ebx"; "r12d"; "r13d"; "r14d"; "r15d"|]
  let cs : int M.t ref = ref (M.empty)
  let emit_reg r =
    try let no = M.find r !cs in
      "%" ^ regs.(no)
    with _ -> r
  let emit_imm = function
    | IReg r -> emit_reg r
    | IInt i -> "$" ^ string_of_int i

  let emit_ir = function
    | IMov(r,i1)      -> emit ("movl "^emit_imm i1 ^", "^emit_reg r)
    | IAdd(r,i1,i2)   -> emit ("movl "^emit_imm i1 ^", "^emit_reg r);
                        emit ("addl "^emit_imm i2 ^", "^emit_reg r)
    | ICmp(r,_,i1,i2) -> emit ("")
    | ILoad(r,i)      -> emit ("")
    | IAlloca(r)      -> emit ("")
    | IRet(r)         -> emit ("movl "^emit_imm r ^", %eax")
    | IBr(l)          -> emit ("jmp L."^l)
    | IBr3(i,l1,l2)   -> emit ("")
    | IStore(i1,i2)   -> emit ("")

  let emit_bb (label,irs) =
    p ("L."^label ^ ":");
    List.iter emit_ir irs
  let emit_bbs (cols:int M.t) bbs =
    cs := cols;
    emit ".section .rodata";
    p ".LC0:";
    emit ".string \"%d\\n\"";
    emit ".text";
    p "print_int:";
    emit "pushq %rbp";
    emit "movq %rsp, %rbp";
    emit "subq $16, %rsp";
    emit "movl %edi, -4(%rbp)";
    emit "movl -4(%rbp), %eax";
    emit "movl %eax, %esi";
    emit "leaq .LC0(%rip), %rdi";
    emit "movl $0, %eax";
    emit "call printf@PLT";
    emit "leave";
    emit "ret";

    emit ".globl main";
    p "main:";
    emit "pushq %rbp";
    emit "movq %rsp, %rbp";
    emit "subq $16, %rsp";
    p "";
    List.iter emit_bb bbs;
    p "";
    emit "movl %eax, -4(%rbp)";
    emit "movl -4(%rbp), %eax";
    emit "movl %eax, %edi";
    emit "call print_int";
    emit "movl $0, %eax";
    emit "leave";
    emit "ret"
  (*
  let _ =
    emit_bbs (M.of_list ["0",0;"1",1;"2",2;"3",3;"4",4]) [
      "0",[
        IMov("0",IInt 100);
        IAdd("1",IReg "0",IInt 23);
        IBr("1");
      ];
      "1",[
        IRet(IReg "1");
      ];
    ]*)
end

let _ =
  let bbs = Compiler.compile (EAdd (EAdd (EInt 4,EInt 30),EAdd (EInt 200,EInt 1000))) in
  let cols = RegAlloc.alloc bbs in
  Emit.emit_bbs cols bbs
