:- use_module('../setmem').
:- use_module('../expand').
:- use_module('../memAlloc').
:- use_module('../emit').

:- begin_tests(setmem).
  test(setmem) :-
    setmem([
      ('main',[],[
        call('printInt',[call('add',[100,20,3])]),
        ret(0)
      ]),
      ('add',['a','b','c'],[
        ret(add('a',add('b','c')))
      ])
    ],S),
    format('s=~w\n',[S]),!,
    expand(S,E),
    format('e=~w\n',[E]),
    memAlloc(E,M),
    format('m=~w\n',[M]),
    emit('a.s',M),
    shell('gcc -static -o a a.s lib/lib.c'),
    shell('./a').
:- end_tests(setmem).
:- run_tests,halt; halt(-1).
