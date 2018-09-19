# Simple x86_64 native compiler writing by SWI-Prolog on linux.

256 line toy language compiler.

## feautures

- Simple memory allocation
- Linear Scan register allocation & spilling
    - Spilling use Simple memory allocation algorithm.
- Graph coloring register allocation & spilling
    - Allocator use Welsh & Powell coloring algorithm.
    - Spilling use Simple memory allocation algorithm.

## install

    apt install swi-prolog gcc

## usage

    swipl main.pl src.mc

is simple memory allocation,

    swipl main.pl -O1 src.mc

is linear scan register allocation and

    swipl main.pl -O2 src.mc

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

- Parse *.mc
    - Read prolog style terms and convert AST
        - [main.pl](main.pl)
- Generate internal codes
    - Compile AST to internal codes of basic blocks
        - [genCode.pl](genCode.pl)
- Regster allocation
    - Simple memory allocation allocates memory addresses for all internal code variables.
        - [memAlloc.pl](memAlloc.pl)
    - Liveness analysis is a dataflow analysis that computes the variables making and removing in each code in the basic block.
        - [liveness.pl](liveness.pl)
    - Linear scan register allocation assigns registers to internal code variables in a simple but fast way.
        - [liveness.pl](liveness.pl) [linearScanRegAlloc.pl](linearScanRegAlloc.pl)
    - Graph coloring register allocation assigns registers to internal code variables using simple Welsh & Powel Graph coloring algorithm.
        - [liveness.pl](liveness.pl) [graph.pl](graph.pl) [graphRegAlloc.pl](graphRegAlloc.pl)
- Output x86_64 assembly codes
    - Output assembly from internal codes to a *.s file
        - [genAmd64.pl](genAmd64.pl)
