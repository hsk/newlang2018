# Simple x86_64 native compiler from simple language writing by SWI-Prolog on linux.

285 line toy language compiler.

    $ ls *.pl | grep -v all.pl | xargs wc
       47   132  3075 genAmd64.pl
       26    63  1451 genCode.pl
       36    82  1958 graph.pl
       27    62  1527 graphRegAlloc.pl
       29    70  1778 linearScanRegAlloc.pl
       37    78  2048 liveness.pl
       26    71  1387 main.pl
       23    59  1222 memAlloc.pl
       34   163  1062 syntax.pl
      285   780 15508 合計

## Syntax

    e ::= i | x | e + e | e - e | x = e | x(e1,...,en)
    s ::= return(e) | if(e,s*,s*) | while(e,s*) | e
    d ::= x(x1,...,xn)=s*.
    p ::= d*

    o  ::= addq | subq
    ae ::= i | x | bin(o,ae,ae) | mov(ae,x) | call(x,ae*)
    as ::= ret(ae) | if(ae,as*,as*) | while(ae,as*) | ae
    af ::= x:x* = as*.
    a  ::= af*

    cr ::= x | $i
    l  ::= x
    cc ::= bin(o,cr,cr,cr) | mov(cr,cr) | call(cr,cr*,cr) | bne(cr,l,l) | br(l) | ret(cr)
    cf ::= x:x* = (l:cc*)*.
    c  ::= cf*

    ri ::= i | -ri
    rr ::= \x | $i | ptr(\x,ri) | null.
    rc ::= enter(ri,\x*)|bin(o,rr,rr,rr) | mov(rr,rr) | call(x,rr*,rr,\x*) | bne(rr,l,l) | br(l) | ret(rr)
    rf ::= x:mov(rr,rr)* = (l:rc*)*.
    r  ::= rf*

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

- [all.pl](all.pl) is one source 128 lines memory allocation compiler.

    $ swipl all.pl src.mc & gcc -static a.s lib/lib.c & ./a.out
    54321
    55

## Simple Compile path

- Parse *.mc
    - Read prolog style terms and convert AST
        - [main.pl](main.pl) 26 lines
- Generate internal codes
    - Compile AST to internal codes of basic blocks
        - [genCode.pl](genCode.pl) 26 lines
- Regster allocation
    - Simple memory allocation allocates memory addresses for all internal code variables.
        - [memAlloc.pl](memAlloc.pl) 23 lines
- Output x86_64 assembly codes
    - Output assembly from internal codes to a *.s file
        - [genAmd64.pl](genAmd64.pl) 47 lines
- Syntax check
    - Syntax check p,a,c and r languages.
    - [syntax.pl](syntax.pl) 34 lines

122 lines.

## Optimization

- Optimize Register allocation
    - Liveness analysis is a dataflow analysis that computes the variables making and removing in each code in the basic block. Result is shadow tree to internal codes.
        - [liveness.pl](liveness.pl) 78 lines
    - Linear scan register allocation assigns registers to internal code variables in a simple but fast way.
        - [linearScanRegAlloc.pl](linearScanRegAlloc.pl) 29 lines
    - Simple graph coloring register allocation assigns registers to internal code variables using Welsh & Powel Graph coloring algorithm.
        - [graph.pl](graph.pl) 82 lines [graphRegAlloc.pl](graphRegAlloc.pl) 27 lines

all 285 lines.
