module StringMap = struct
  include Map.Make(String)
end
type reg = string [@@deriving show]
type t = I64 | I32 | I1 | I8 | Void | Ptr of t | Func of t * t list [@@deriving show]
type imm = IReg of t * reg | IInt of t * int | IStr of string [@@deriving show]
type label = string [@@deriving show]
type op = IAdd | ISub | IMul | ISDiv | IAnd | IOr [@@deriving show]
type unop = INeg | INot [@@deriving show]
type cmp = IEq | INe | ISLt | ISLe | ISGt | ISGe [@@deriving show]
type ir =
  | IMov of reg * imm
  | IBin of reg * imm * op * imm
  | ICmp of reg * imm * cmp * imm
  | IUn of reg * unop * imm
  | IRet of imm
  | IRetVoid
  | IAlloca of reg * t
  | IBr of label
  | IBne of imm * label * label
  | ILoad of reg * imm
  | IStore of imm * imm
  | ICall of reg * (string * t) * imm list
  [@@deriving show]
type bb = label * ir list [@@deriving show]
type bbs = bb list [@@deriving show]
type func = string * t * bbs [@@deriving show]
type globals = (string * imm) list [@@deriving show]
type statics = (string * string) list [@@deriving show]
type program = globals * statics * func list [@@deriving show]

type globals1 = imm StringMap.t

let show_globals1 g = show_globals (StringMap.bindings g)
let pp_globals1 f g = pp_globals f (StringMap.bindings g)

let get_t = function
  | IReg(t,_) -> t
  | IInt(t,_) -> t
  | IStr(_) -> Ptr I8
let cnt = ref 0
let label : label ref = ref "enter"
let irs:ir list ref = ref []
let bbs:bb list ref = ref []
let init_cnt () = cnt := 0
let init_bbs lbl = label := lbl; irs := []; bbs := []
let get_bbs () =
  List.fold_left(fun bbs (label,irs) ->
    (label,List.rev irs)::bbs
  ) [] ((!label,!irs)::!bbs)
let add_bb bb = bbs := bb :: !bbs
let add_label lbl =
  bbs := (!label,!irs) :: !bbs;
  label := lbl; irs := []
let add_ir ir = irs := ir :: !irs
let new_reg () = incr cnt; string_of_int !cnt
