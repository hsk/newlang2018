:- use_module('../asm').
:- begin_tests('asm').
  test('write') :-
    asm:open('a.s'),
    asm('test'),
    asm:close(),
    asm:readfile('a.s','test\n').
  test(genid) :-
    resetid,
    genid(a,a0),
    genid(a,a1),
    genid(a,a2),
    resetid.
:- end_tests('asm').
:- run_tests,halt; halt(-1).
