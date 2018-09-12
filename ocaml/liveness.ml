open Code
open Graph

let local_id = ref 0
let locals = ref M.empty
let init () = local_id := 0; locals := M.empty
let add_local r =
  locals := M.add r !local_id !locals;
  incr local_id

let collect_io_imm1 imms = function
  | IReg(_,a) -> a::imms
  | _ -> imms

let collect_io_imm = List.fold_left collect_io_imm1 []

let collect_io_ir = function
  | IMov(r,i1) -> [r],collect_io_imm[i1]
  | IBin(r,i1,_,i2) -> [r],collect_io_imm[i1;i2]
  | IUn(r,_,i1) -> [r],collect_io_imm[i1]
  | ICmp(r,i1,_,i2) -> [r],collect_io_imm[i1;i2]
  | ILoad(r,i) -> [r],collect_io_imm[i]
  | IRet(i) -> [],collect_io_imm[i]
  | IAlloca(r,_) -> add_local r; [r],[]
  | IBr(l) -> [],[]
  | IBne(r,l1,l2) -> [],collect_io_imm[r]
  | IStore(i1,i2) -> [],collect_io_imm[i1;i2]
  | ICall(r,_,is) -> [r],collect_io_imm is
  | IRetVoid -> [],[]
let collect_io_br = function
  | IRet(i) -> []
  | IBr(l) -> [l]
  | IBne(r,l1,l2) -> [l1;l2]
  | IRetVoid -> []
  | _ -> assert false

let collect_io_bbs = List.map (fun ((k:string),bb)->
  let block = List.map collect_io_ir bb in
  let br = if bb=[] then [] else collect_io_br (last bb) in
  (k,{inp=S.empty;out=S.empty;kills=[];block;br})
)

let collect_func (_,_,bbs) =
  init ();
  let live = liveness (collect_io_bbs bbs) in
  let kills = kill live in
  (!locals, live, kills)

let collect (global_vars,statics,funcs) =
  List.map collect_func funcs
