let parse s = Parser.program Scanner.token (Lexing.from_string s)
let parseFile file =
  let fp = open_in file in
  let e = Parser.program Scanner.token (Lexing.from_channel fp) in
  close_in fp;
  e

let _ =
  let program = parse
    "int a4000() { return 1000*8/2; }
     int a1() { return 0||1||3; }
     int a20() { return 21-(0&&1)-1; }
     int a300() { return (10<10)+(10>10)+(10<=10)*100+(10>=10)*100+(99==99)*50+(100!=99)*50; }
     int a50000(int a) { int b;  b=25000;return b+a; }
     int main() { print(a1()+a20()+a300()+a4000()+a50000(25000)); return 0;}
    " in
  Printf.fprintf stderr "e=%s\n" (Syntax.show_program program);
  let program = GenCode.compile program in
  Printf.fprintf stderr "%s\n" (Code.show_program program);
  let allocation = GraphRegAlloc.alloc program in
  GenAmd64.emit allocation program
