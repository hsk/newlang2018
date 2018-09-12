:- use_module('../genid.pl').
:- begin_tests(gen_id).
  test(genid) :-
    resetid,
    genid(a,a0),
    genid(a,a1),
    genid(a,a2),
    resetid.
:- end_tests(gen_id).
:- run_tests,halt; halt(-1).
