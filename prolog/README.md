# Simple x86_64 native compiler from simple language writing by SWI-Prolog on linux.

256 line toy language compiler.

## Feautures

- Simple memory allocation
- Linear Scan register allocation & spilling
    - Spilling use Simple memory allocation algorithm.
- Graph coloring register allocation & spilling
    - Allocator use Welsh & Powell coloring algorithm.
    - Spilling use Simple memory allocation algorithm.

## Install

    apt install swi-prolog gcc

## Usage

    $ swipl main.pl src.mc
    $ gcc a.s -static lib/lib.c -o a.out
    $ ./a.out
    54321
    55

or

    make

or

    make test


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

## Compile path

- Parse *.mc
    - Read prolog style terms and convert AST
        - [main.pl](main.pl)
- Generate internal codes
    - Compile AST to internal codes of basic blocks
        - [genCode.pl](genCode.pl)
- Regster allocation
    - Simple memory allocation allocates memory addresses for all internal code variables.
        - [memAlloc.pl](memAlloc.pl)
- Output x86_64 assembly codes
    - Output assembly from internal codes to a *.s file
        - [genAmd64.pl](genAmd64.pl)

## Optimization

- Optimize Register allocation
    - Liveness analysis is a dataflow analysis that computes the variables making and removing in each code in the basic block. Result is shadow tree to internal codes.
        - [liveness.pl](liveness.pl)
    - Linear scan register allocation assigns registers to internal code variables in a simple but fast way.
        - [linearScanRegAlloc.pl](linearScanRegAlloc.pl)
    - Simple graph coloring register allocation assigns registers to internal code variables using Welsh & Powel Graph coloring algorithm.
        - [graph.pl](graph.pl) [graphRegAlloc.pl](graphRegAlloc.pl)
