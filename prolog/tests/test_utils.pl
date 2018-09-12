:- use_module('../utils').
:- begin_tests('utils').
  test(genid) :-
    resetid,
    genid(a,a0),
    genid(a,a1),
    genid(a,a2),
    resetid.
:- end_tests('utils').
:- run_tests,halt; halt(-1).
