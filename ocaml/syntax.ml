type op = EAdd | ESub | EMul | EDiv | EEq | ENe | ELt | ELe | EGt | EGe |
          EAnd | EOr
  [@@deriving show]

type uop = ENeg | ENot
  [@@deriving show]

type typ = TInt | TBool | TVoid | TChar | TPtr of typ | TAbs of typ * typ list
  [@@deriving show]

type bind = typ * string
  [@@deriving show]

type expr =
  | EInt of int
  | EBool of bool
  | EId of string
  | EBin of expr * op * expr
  | EUn of uop * expr
  | EAssign of string * expr
  | ECall of string * expr list
  | ENop
  [@@deriving show]

type stmt =
  | SBlock of stmt list
  | SExpr of expr
  | SReturn of expr
  | SIf of expr * stmt * stmt
  | SFor of expr * expr * expr * stmt
  | SWhile of expr * stmt
  [@@deriving show]

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }
  [@@deriving show]

type program = bind list * func_decl list
  [@@deriving show]

(* Pretty-printing functions *)

let string_of_op = function
  | EAdd -> "+"
  | ESub -> "-"
  | EMul -> "*"
  | EDiv -> "/"
  | EEq  -> "=="
  | ENe -> "!="
  | ELt -> "<"
  | ELe -> "<="
  | EGt -> ">"
  | EGe -> ">="
  | EAnd -> "&&"
  | EOr -> "||"

let string_of_uop = function
  | ENeg -> "-"
  | ENot -> "!"

let rec string_of_expr = function
  | EInt(l) -> string_of_int l
  | EBool(true) -> "true"
  | EBool(false) -> "false"
  | EId(s) -> s
  | EBin(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | EUn(o, e) -> string_of_uop o ^ string_of_expr e
  | EAssign(v, e) -> v ^ " = " ^ string_of_expr e
  | ECall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ENop -> ""

let rec string_of_stmt = function
  | SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_expr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | SWhile(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVoid -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
