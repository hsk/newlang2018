# Simple x86_64 native compiler writing by SWI-Prolog on linux.

310 line toy language compiler.

## feautures

- Linear Scan register allocation & spilling
    - Spilling use Simple memory allocation algorithm.
- Graph coloring register allocation & spilling
    - Allocator use Welsh & Powell coloring algorithm.
    - Spilling use Simple memory allocation algorithm.

## install

    apt install swi-prolog gcc

## usage

    swipl main.pl src.mc

is linear scan register allocation or

    swipl main.pl -O1 src.mc

is graph register allocation.

## source program example

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
    - read prolog style expressions and convert AST
        - main.pl
- generate inner codes
    - compile AST to inner codes
        - genCode.pl
- regster allocation
    - allocate register from inner codes which linear scan or graph coloring register allocation algorithm
        - liveness.pl linearScanRegAlloc.pl graph.pl graphRegAlloc.pl
- output x86_64 assembly code
    - output assembly from inner codes to a *.s file
    - emit.pl
- generate a uniqe id
    - utils.pl
