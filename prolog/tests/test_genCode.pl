:- use_module('../genCode').
:- use_module('../graphRegAlloc').
:- use_module('../emit').
:- begin_tests(genCode).
  test(genCode) :-
    genCode([
      ('main',[],[
        mov(100,'a'),
        mov(20,'b'),
        mov(3,'c'),
        call('printInt',[call('add',['a','b','c'])]),
        mov(0,'e'),
        ret('e')
      ]),
      ('add',['a','b','c'],[
        ret(bin(add,'a',bin(add,'b','c')))
      ])
    ],P),
    regAlloc(P,M),
    emit('a.s',M),
    shell('gcc -static -o a a.s lib/lib.c'),
    shell('./a > a.txt ; echo 123 | diff a.txt -').
:- end_tests(genCode).
:- run_tests,halt; halt(-1).
