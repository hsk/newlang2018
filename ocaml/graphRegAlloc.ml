let alloc program =
  let func_lives = Liveness.collect program in
  List.map(fun (locals,live,kills)->
    Printf.fprintf stderr "live=%s\n" (Graph.show_g live);
    let edges = Graph.gen_edges live in
    Printf.fprintf stderr "edges=%s\n" (Graph.show_edges edges);
    let cs = Graph.coloring (Graph.neighbors edges) in
    (locals,cs,kills)
  ) func_lives
