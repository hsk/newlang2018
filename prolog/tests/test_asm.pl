:- use_module('../asm').
:- begin_tests('asm').
  test('write') :-
    asm:open('a.s'),
    asm('test'),
    asm:close(),
    asm:readfile('a.s','test\n').
:- end_tests('asm').
:- run_tests,halt; halt(-1).
