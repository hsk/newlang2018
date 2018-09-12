open Printf

module M = struct
  include Map.Make(String)
  let of_list ls = List.fold_left (fun m (k,v)-> add k v m) empty ls
end
module MapInt = struct
  include Map.Make(struct type t = int let compare = (-) end)
  let of_list ls = List.fold_left (fun m (k,v)-> add k v m) empty ls
end

module S = Set.Make(String)

(* Welsh-Powell-Algorithm *)
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

type bb_io = {
  inp : S.t;
  out : S.t;
  kills : S.t list;
  block: (string list * string list) list;
  br : string list;
}
type g = (string * bb_io) list

let rec last = function
  | [] -> raise Not_found
  | [a] -> a
  | x::xs -> last xs

let rec liveness g =
  let g2 = List.map(fun (k,{inp;out;kills;block;br}) ->
    let out,inp = List.fold_left(fun (out,inp) b ->
        S.fold(fun i (out,inp) -> (S.add i out,S.add i inp)) (List.assoc b g).inp (out,inp)
    ) (out,inp) br in
    let out,inp = if br<>[] || block=[] then out,inp else
      List.fold_left(fun (out,inp) i -> (S.add i out,S.add i inp)) (out,inp) (snd(last block))
    in
    let inp = List.fold_left(fun vinp (out,inp) ->
      S.diff (S.union (S.of_list inp) vinp) (S.of_list out)
    ) inp (List.rev block) in
    (k,{inp;out;kills;block;br})
  ) g in
  if g = g2 then g else liveness g2

let rec kill g =
  List.map(fun (_,{inp;out;block;br}) ->
    let (_,kills) = List.fold_left(fun (lives,kills) (out,inp) ->
      let out = (S.of_list out) in
      let dies = S.diff (S.of_list inp) lives in
      let lives = S.diff lives out in
      (lives,(out,dies)::kills)
    ) (out,[]) (List.rev block) in
    (inp,kills)
  ) g

type edges = (string * string) list [@@deriving show]

let gen_edges g =
  let addEs x y es =
    if x=y && List.exists(fun (a,b)->x=a||y=b) es then es else
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
      let (es,live) = List.fold_left (fun (es,s) o->
        (addEs o o es, S.remove o s)
      ) (es,live) out in
      let es = add es (inp @ S.elements live) in
      let live = List.fold_left(fun s v -> S.add v s) live inp in
      (live,es)
    ) (live,es) (List.rev block) in
    es
  ) [] g

let run_bb g = run (gen_edges (liveness g))

module Output = struct
  type bb_io = {
    inp : string list;
    out : string list;
    kills : string list list;
    block: (string list * string list) list;
    br : string list;
  } [@@deriving show]
  type g = (string*bb_io) list [@@deriving show]
end
let bbio_to_bbio2 {inp;out;kills;block;br} =
  {Output.inp=S.elements inp;out=S.elements out;kills=List.map S.elements kills;block;br}
let g_to_g2 (g:g):Output.g = List.map(fun (k,b)-> (k,bbio_to_bbio2 b)) g
let pp_bb_io fmt bbio = Output.pp_bb_io fmt (bbio_to_bbio2 bbio)
let show_bb_io bbio = Output.show_bb_io (bbio_to_bbio2 bbio)
let show_g (g:g) = Output.show_g (g_to_g2 g)
let pp_g fmt (g:g) = Output.pp_g fmt (g_to_g2 g)
