# Simple x86_64 native compiler from simple language writing by SWI-Prolog on linux.

250 line toy language compiler.

    $ ls *.pl | grep -v all.pl | xargs wc
       47   132  3075 genAmd64.pl
       26    63  1451 genCode.pl
       36    82  1970 graph.pl
       27    62  1527 graphRegAlloc.pl
       29    70  1778 linearScanRegAlloc.pl
       37    78  2048 liveness.pl
       25    70  1314 main.pl
       23    59  1222 memAlloc.pl
      250   616 14385 合計

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


## Source program example

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

## One Source compiler

- [all.pl](all.pl) is one source 120lines memory allocation compiler.

    $ swipl all.pl src.mc & gcc -static a.s lib/lib.c & ./a.out
    54321
    55

## Simple Compile path

- Parse *.mc
    - Read prolog style terms and convert AST
        - [main.pl](main.pl) 25 lines
- Generate internal codes
    - Compile AST to internal codes of basic blocks
        - [genCode.pl](genCode.pl) 26 lines
- Regster allocation
    - Simple memory allocation allocates memory addresses for all internal code variables.
        - [memAlloc.pl](memAlloc.pl) 23 lines
- Output x86_64 assembly codes
    - Output assembly from internal codes to a *.s file
        - [genAmd64.pl](genAmd64.pl) 47 lines

121 lines.

## Optimization

- Optimize Register allocation
    - Liveness analysis is a dataflow analysis that computes the variables making and removing in each code in the basic block. Result is shadow tree to internal codes.
        - [liveness.pl](liveness.pl) 78 lines
    - Linear scan register allocation assigns registers to internal code variables in a simple but fast way.
        - [linearScanRegAlloc.pl](linearScanRegAlloc.pl) 29 lines
    - Simple graph coloring register allocation assigns registers to internal code variables using Welsh & Powel Graph coloring algorithm.
        - [graph.pl](graph.pl) 82 lines [graphRegAlloc.pl](graphRegAlloc.pl) 27 lines

all 250 lines.
