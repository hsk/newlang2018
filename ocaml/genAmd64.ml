open Graph
open Code

let emit fmt = Printf.kprintf (fun s -> Printf.printf "\t%s\n" s) fmt

let p fmt = Printf.kprintf (fun s -> Printf.printf "%s\n" s) fmt

let cs : int M.t ref = ref (M.empty)
let locals = ref M.empty
let set_locals locals1 = locals := locals1

let lives = ref S.empty

let roundup((x:int), (align:int)):int =
  ((x + align - 1) / align) * align

let regs8 = [|"r10b"; "r11b"; "bl"; "r12b"; "r13b"; "r14b"; "r15b"|]
let regs32 = [|"r10d"; "r11d"; "ebx"; "r12d"; "r13d"; "r14d"; "r15d"|]
let regs = [|"r10"; "r11"; "rbx"; "r12"; "r13"; "r14"; "r15"|]
let argregs8 = [|"dil"; "sil"; "dl"; "cl"; "r8b"; "r9b"|]
let argregs32 = [|"edi"; "esi"; "edx"; "ecx"; "r8d"; "r9d"|]
let argregs = [|"rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"|]

let get_size = function
    | I8 | Void -> 1
    | I32 -> 4
    | I1 | Func(_,_) | Ptr _ | I64 -> 8

let emit_reg t r =
  try let no = M.find r !cs in
    match get_size t with
    | 1 -> "%" ^ regs8.(no)
    | 4 -> "%" ^ regs32.(no)
    | _ -> "%" ^ regs.(no)
  with Not_found -> r
let emit_areg t no =
    match get_size t with
    | 1 -> "%" ^ argregs8.(no)
    | 4 -> "%" ^ argregs32.(no)
    | _ -> "%" ^ argregs.(no)

let static_vars = ref []
let fname = ref ""

let escaped = Array.make 256 '0'
let () =
  escaped.(8) <- 'b'; escaped.(12) <- 'f';
  escaped.(int_of_char '\n') <- 'n';  escaped.(int_of_char '\r') <- 'r';
  escaped.(int_of_char '\t') <- 't'; escaped.(int_of_char '\\') <- '\\';
  escaped.(int_of_char '\'') <- '\''; escaped.(int_of_char '"') <- '"'

let backslash_escape (s : string) =
  let sb = Buffer.create 1024 in
  for i = 0 to String.length s -1 do
    let c = String.get s i in
    let esc = escaped.(int_of_char c) in
    if esc > '0' then
      (Buffer.add_char sb '\\'; Buffer.add_char sb esc)
    else if c >= Char.chr 32 && c <= Char.chr 126 then
      Buffer.add_char sb c
    else Buffer.add_string sb (Printf.sprintf "\\%03o" (int_of_char c))    
  done;
  Buffer.contents sb

let emit_imm = function
  | IReg(t,r) -> emit_reg t r
  | IInt(_,i) -> "$" ^ string_of_int i
  | IStr(s)   -> emit "leaq	L.%s(%%rip), %%rax" (List.assoc s !static_vars);
                 "%rax"
let emit_imm_t t = function
  | IReg(_,r) -> emit_reg t r
  | IInt(_,i) -> "$" ^ string_of_int i
  | IStr(s)   -> emit "leaq	L.%s(%%rip), %%rax" (List.assoc s !static_vars);
                 "%rax"
let emit_imm64 = function
  | IReg(_,r) -> emit_reg I64 r
  | i -> emit_imm i
let emit_op = function
  | IAdd -> "add"
  | ISub -> "sub"
  | IMul -> "imul"
  | IOr -> "or"
  | IAnd -> "and"

let emit_cmp = function
  | IEq -> "sete"
  | INe -> "setne"
  | ISLt -> "setl"
  | ISLe -> "setle"
  | ISGt -> "setg"
  | ISGe -> "setge"

let emit_unop = function
  | INeg -> "neg"
  | INot -> "not"
let emit_local r =
  -8 * (1+M.find r !locals)

let emit_call r (n,Func(t,ts)) is =
  let (md,pushes) = List.fold_left (fun (md,ls) s->
    match M.find_opt s !cs with
    | Some n when n < 2 -> (not md,n::ls)
    | _ -> (md,ls)
  ) (false,[]) (S.elements !lives) in
  List.iter(fun n->emit "push %%%s" (regs.(n))) pushes;
  if md then emit "sub $8, %%rsp";
  List.iteri (fun i i1-> emit "mov %s,%s" (emit_imm i1) (emit_areg (get_t i1) i)) is;
  emit "call %s" n;
  if md then emit "add $8, %%rsp";
  List.iter(fun n->emit "pop %%%s" (regs.(n))) (List.rev pushes);
  emit "mov %%rax,%s" (emit_reg I64 r)
let emit_q imm =
  match get_size(get_t imm) with
  | 1 -> "b"
  | 4 -> "l"
  | _ -> "q"

let emit_ax t =
  match get_size(t) with
  | 1 -> "%al"
  | 2 -> "%ax"
  | 4 -> "%eax"
  | 8 -> "%rax"

let max_t t1 t2 =
  let s1 = get_size t1 in
  let s2 = get_size t2 in
  if s1 > s2 then t1 else t2

let rec emit_ir (out,kill) ir =
  lives := S.diff !lives kill;
  (match ir with
  | IMov(r,i1)       -> emit "mov %s, %s" (emit_imm i1) (emit_reg (get_t i1) r)
  | IBin(r,i1,ISDiv,i2) ->
                        emit "mov %s, %%rax" (emit_imm64 i1);
                        emit "mov %s, %s" (emit_imm64 i2) (emit_reg (get_t i2) r);
                        emit "cltd";
                        emit "idiv %s" (emit_reg (get_t i1) r);
                        emit "mov %%rax,%s" (emit_reg I64 r)
  | IBin(r,i1,op,i2) -> let t1 = get_t i1 in let t2 = get_t i1 in let t = max_t t1 t2 in
                        emit "mov %s, %s" (emit_imm i1) (emit_reg t1 r);
                        emit "%s %s, %s" (emit_op op) (emit_imm_t t i2) (emit_reg t r)
  | IUn(r,op,i1)     -> emit "%s %s, %s" (emit_unop op) (emit_imm i1) (emit_reg (get_t i1) r)
  | ICmp(r,i1,op,i2) -> let t1 = get_t i1 in let t2 = get_t i1 in let t = max_t t1 t2 in
                        emit "mov %s, %s" (emit_imm i1) (emit_reg t1 r);
                        emit "cmp %s, %s" (emit_imm_t t i2) (emit_reg t r);
                        emit "%s %%al" (emit_cmp op);
                        emit "movzbl %%al, %s" (emit_reg t r)
  | IBr(l)           -> emit "jmp L.%s" l
  | IBne(r,l1,l2)    -> emit "mov %s,%s" (emit_imm r) (emit_ax (get_t r));
                        emit "test %s,%s" (emit_ax (get_t r)) (emit_ax (get_t r));
                        emit "jne L.%s" l1;
                        emit "jmp L.%s" l2
  | IRet(r)          -> emit "mov %s,%s" (emit_imm r) (emit_ax (get_t r));
                        emit "jmp L.%s.end" !fname
  | IRetVoid         -> emit "mov $0,%%rax";
                        emit "jmp L.%s.end" !fname
  | IAlloca(r,t)     -> emit "lea %d(%%rbp),%s" (emit_local r) (emit_reg t r)
  | ILoad(r,i)       -> emit "mov (%s),%s" (emit_imm i) (emit_reg (get_t i) r)
  | IStore(i1,i2)    -> emit "mov%s %s,(%s)" (emit_q i1) (emit_imm i1) (emit_imm i2)
  | ICall(r,n,is)    -> emit_call r n is
  );
  lives := S.union !lives out

let emit_bb (label,irs) (inp,kills) =
  p "L.%s:" label;
  lives := inp;
  List.iter2 emit_ir kills irs

type allocation = int M.t * int M.t * (S.t*(S.t*S.t) list) list
type allocations = allocation list
module IntSet = Set.Make(struct type t= int let compare a b = a - b end)
let emit_bbs ((locals:int M.t),(cols:int M.t),(kills:(S.t*(S.t*S.t) list) list)) bbs =
  set_locals locals;
  cs := cols;
  let regset = List.fold_left (fun s (k,i) ->
    Printf.fprintf stderr "%s->%d\n" k i;
    IntSet.add i s
  ) IntSet.empty (M.bindings cols) in
  let pushes = List.filter (fun s ->IntSet.mem s regset) [2;3;4;5;6] in
  let local_size = roundup((M.cardinal locals) * 8, 16)+((List.length pushes) mod 2)*8 in
  if local_size > 0 then emit "sub $%d, %%rsp" local_size;
  List.iter(fun (e:int)-> emit "push %%%s" (regs.(e))) pushes;
  List.iter2 emit_bb bbs kills;
	emit "mov $0,%%rax";
  p "L.%s.end:" !fname;
  List.iter(fun e-> emit "pop %%%s" (regs.(e))) (List.rev pushes);
  if local_size > 0 then emit "add $%d, %%rsp" local_size

let emit_func (allocation:allocation) ((name,(func:t),bbs):func) =
  fname := name;
  emit ".text";
  emit ".globl %s" name;
  p "%s:" name;
  emit "pushq %%rbp";
  emit "mov %%rsp, %%rbp";
  emit_bbs allocation bbs;
  emit "leave";
  emit "ret"

let emit_static (s,i) =
	emit ".section .rodata";
  p "L.%s:" i;
	emit ".string	\"%s\"" (backslash_escape s)

let emit (allocations:allocations) ((globals,statics,funcs):program) =
  static_vars := statics;
  List.iter emit_static statics;
  List.iter2 emit_func allocations funcs
