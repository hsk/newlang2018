:- use_module('../memAlloc').
:- use_module('../emit').

:- begin_tests(memAlloc).
  test(memAlloc) :-
    memAlloc([
      ('main',[
        (bb1,[
          mov('$1','a'),
          call('printInt',['a'],'%rax'),
          mov('$0','b'),
          ret('b')
        ])
      ])
    ],L),
    format('l=~w',[L]),
    emit('a.s',L),
    shell('gcc -static -o a a.s lib/lib.c'),
    shell('./a').
:- end_tests(memAlloc).
:- run_tests,halt; halt(-1).
