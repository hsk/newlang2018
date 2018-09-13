# Simple x86_64 native compiler writing by SWI-Prolog on linux.

250 line toy language compiler.

## feautures

- Simple memory allocation
    - All variable saved on memory.
- Graph coloring register allocation & spilling
    - Allocator use Welsh & Powell coloring algorithm.
    - Spilling use Simple memory allocation algorithm.

## install

    apt install swi-prolog gcc

## run

    make test

source file

    % src.mc
    main()=[
      if(0,[
        a=0
      ],[
        a=50000+5000-1000
      ]),
      printInt(a+add(300,20,1)),
      printInt(sum(10)),
      return(0)
    ].
    sum(n)=[
      if(n,[return(sum(n-1)+n)],[]),
      return(n)
    ].
    add(a,b,c)=[
      return(a+b+c)
    ].
    add2(a,b)=[
      return(a+b)
    ].

## compile path

- parse *.mc
    - read and convert AST
        - main.pl
- generate inner codes
    - compile AST to inner codes
        - genCode.pl
- regster allocation or memory allocation
    - allocate register to inner code with graph coloring register allocation algorithm
        - graph.pl liveness.pl graphRegAlloc.pl
- output x86_64 assembly code
    - output assembly from inner codes to a *.s file
    - emit.pl

## todo

- 関数内での使用レジスタのみを関数のenter leaveの処理でpush,popする
- 関数コール時に生きているレジスタのみpushする
- 引数足りなくなった場合でも動作するようにする
- コマンドラインでファイル名を指定する
