:- use_module('../graphRegAlloc').
:- use_module('../genAmd64').

:- begin_tests(regAlloc).
  test(regAlloc) :-
    regAlloc([
      main:[]=[
        bb1:[
          mov($1,a),
          call(printInt,[a],\rax),
          mov($0,b),
          ret(b)
        ]
      ]
    ],L),
    genAmd64('a.s',L),
    shell('gcc -static -o a a.s lib/lib.c'),
    shell('./a > a.txt ; echo 1 | diff a.txt -').
:- end_tests(regAlloc).
:- run_tests,halt; halt(-1).
