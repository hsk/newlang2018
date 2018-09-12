:- use_module('../emit.pl').
:- begin_tests(emit).
  test('write') :-
    emit:open('a.s'),
    emit:asm('test'),
    emit:close(),
    emit:readfile('a.s','test\n').
  test(emit1) :-
    % 1を出力するプログラム
    emit('a.s', [
      ('main',[
        ('.bb1',[
          movl('$1', '%edi'),
          call('printInt',[]),
          ret('$0')
        ])
      ])
    ]),
    shell('gcc -static -o a a.s lib/lib.c'),
    shell('./a').
:- end_tests(emit).
:- run_tests,halt; halt(-1).
