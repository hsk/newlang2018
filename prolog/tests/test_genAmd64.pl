:- use_module('../genAmd64.pl').
:- begin_tests(genAmd64).
  test('write') :-
    genAmd64:open('a.s'),
    genAmd64:emit('test'),
    genAmd64:close(),
    genAmd64:readfile('a.s','test\n').
  test(genAmd641) :-
    % 1を出力するプログラム
    genAmd64('a.s', [
      ('main',[],[
        ('.bb1',[
          mov('$1', '%rdi'),
          call('printInt',[],'%rax',[]),
          ret('$0')
        ])
      ])
    ]),
    shell('gcc -static -o a a.s lib/lib.c'),
    shell('./a>a.txt; echo 1 | diff a.txt -').
:- end_tests(genAmd64).
:- run_tests,halt; halt(-1).
