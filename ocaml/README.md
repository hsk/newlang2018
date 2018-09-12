# グラフ彩色を用いたレジスタアロケーション

## todo

- 関数引数は全部メモリに保存する。
- 変数を全部メモリに保存した場合はrax,rcx,rdxあたりに読み込んで計算する。
- 溢れたレジスタは単にこれらのレジスタに読み込んで計算して保存すればいい。
- レジスタ割付はメモリからロードしなくて済むだけ。
- 溢れたレジスタは通常運転でメモリから読み込む。

- 基本のバックエンドが現状はレジスタ溢れがない物として作られているし、簡単な例では溢れたりしない。
- 関数引数をメモリに保存してしまってアドレスを読み込めば通常運転に戻れる。
- どこで関数引数をメモリに格納するかが問題なだけ。

- 関数引数に引数情報を書いてAMD64ジェネレータまで持ってく構造にする。
- AMD64ジェネレータは引数をすぐにメモリに保存し、溢れたレジスタが対比されている状況として処理する。
- 溢れたレジスタはメモリに保管されるだけ。

レジスタのロードストアは通常レジスタ番号と、指定レジスタ番号の２つを渡す。0番のレジスタの値をくれただし、失敗したらraxに入れてくれ。というようなことをしたい。

ax = cx + dx
"rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9"; "rax" あたりを使って計算する。
- 受け取った引数はメモリに保存子ローカル変数としてアクセスする。
- ローカル変数もメモリに保存してローカル変数としてアクセスする。
- 計算にはrax,rcx,rdxあたりを主に使い、メモリから読み込んで結果をメモリに書き出す。
- r100 = r200+r300 を計算することを考えると、rax = rax + rcx にすればいい。まずr200をraxにr300をrcxにロードしてrax+rcx=raxにしたあとr100にraxを送る。

 mov -200(rpc), rax
 mov -300(rpc), rcx
 add rcx,rax
 mov rax,-100(rpc)
 のような計算ができればよい。

  | IBin(r,i1,op,i2) -> let t1 = get_t i1 in let t2 = get_t i1 in let t = max_t t1 t2 in
                        let cx = (emit_immcx i1) and ax = (emit_regax t1 r) in
                        emit "mov %s, %s" cx ax;
                        emit "%s %s, %s" (emit_op op) (emit_immcx_t t i2) (emit_reg t ax)


最大サイズを求める。

	movl	a(%rip), %eax
	movl	c(%rip), %ecx          # a / c    eax / ecx = eax あまり edx
	cltd
	idivl	%ecx

- LLVMに似せるとか後で考える。
- C言語用のバックエンドでいい。

どちらにしてもデータは溢れる。
溢れているデータは一番うしろのレジスタにロードする。r15かな？
２つ溢れてた場合はどうするんだ？


- 各２項演算子
- 各単項演算子
- 関数引数受取り
    - LLVM式だとレジスタ番号が引数にもつく
    - 9cc方式だとつかない
    - 自分のものの方式では、どうするかという話で、とりあえず全部メモリに保存してしまおうかと思う。
    - とりあえず保存して、それから使う。
    - どうやって誰が保存するの？
    - 誰がレジスタの割当をするの？ってあたりが問題ありありありなのです。
    - 引数にはマイナス値の番号を割り当てるとかする？
    - マイナス値ならば、レジスタの溢れがあるとかにする。
    - レジスタは割り算でdxが溢れる。後は溢れないはず。割り算があったら強制的にdxの番号を溢れさせればいい。
    - どこでdxを溢れは、要するに溢れる。意味分かんない。
    - そもそもレジスタ溢れは、レジスタ番号外ならあふれるはず。
    - 溢れたレジスタはメモリ上のアドレスに保存する。
    - 参照されたら専用のレジスタを使って読み込む。
    - まず、引数として使われるレジスタの値はすぐにメモリに書き込むことにする。絶対だ。
    - 誰が書き込むのかというと、genCodeかgenAmd64かだけど、ごちゃまぜくんなのだ。
        - 例えば、genAMD64がするということにする利点はLLVMのように扱える利点があるわけだけど、問題はレジスタ溢れが引数のレジスタは発生しやすいはずであることと、書き換えられやすいのでメモリに保存されやすいことがある。また、保存したら後は使われないことも多いはずである。しかしながら、使われないといい切れるものではない。しかし、コンパイラが保存してそこから読み込んで使うつもりなら溢れることは気にしなくていい。
        溢れるかもしれないというか、溢れさせてしまうというか、溢れさせたとして参照するのであれば書き換わる前に保存して、それを読み込まないといけないのであるが、いつ書き換わるかわからないから早めの保存が寛容だ。
        関数呼び出しがあればpush,popでなんとかするけど、割り算があったら溢れるので保存だな。
        割り算があったら保存だの法則は徹底なので、割り算では、ダミー変数で色を塗る。ダミー変数は現れてすぐ消える。
        すぐ消えるけど、色が引数番号で塗ってある。

    生存情報はIRの段階で取得する。だから何？分からない。難しい疲れた。

- リニアスキャンレジスタアロケーション

最適化コンパイラではレジスタアロケーションをグラフ彩色を使って行うようです。
グラフ彩色というと難しいイメージがありますが入門として、 Welsh Powell のアルゴリズムを用いてレジスタアロケーションを行います。

## 1. グラフ彩色

Welsh Powell のアルゴリズムはかなり単純なアルゴリズムです。
まずグラフの点から隣の点のリストを作り長さも求めておきます。
次に点を隣の点の数が大きい順にソートします。
ソートしたら点を１つずつ選びだして色を塗り、それ以降の点もその色を塗ります。
色を塗るには色がまだ塗られておらず、隣が同じ色ではない点に塗ります。
すべての点を塗り終わったら次の色を塗ります。
最後の点まで色を塗り終えれば色付け終了です。

## 1.1. Welsh Powellのグラフ彩色

OCamlで書くと以下のようになります:

    module M = struct
      include Map.Make(String)
      let of_list ls = List.fold_left (fun m (k,v)-> add k v m) empty ls
    end
    module S = Set.Make(String)

    (* Welsh-Powell-Algorithm *)
    let coloring ns =
      let cmp (_,x) (_,y) = S.cardinal y - S.cardinal x in
      let ns = List.sort cmp (M.bindings ns) in
      let rec loop = function
        | (cs,c,[]) -> cs
        | (cs,c,(a,e)::i) when M.mem a cs -> loop (cs,c,i)
        | (cs,c,(a,e)::i) ->
          loop (List.fold_left (fun cs (a,e1) ->
            if M.mem a cs || S.mem a e || S.exists(fun v->Some c=M.find_opt v cs) e1 then cs
            else M.add a c cs
          ) (M.add a c cs) i,c+1,i)
      in
      loop (M.empty,0,ns)

MとSは文字列からのマップと文字列の集合です。
coloringでは ns:(string * S.t) list のデータを受取ります。隣の点は集合として受取ります。ソートはS.cardinalで集合内の要素数を求め比較します。
再帰的なループは色のマップと色番号とノードリストの３つの状態を持つ状態マシンで、ノードリストがなくなったら色のマップを返します。
残っている場合は色(c)をノードリスト内の要素(a,e)に対して塗ります。ただし,すでに色が塗られていたら飛ばします。
色が塗られていなければマップにaと色cを登録して、それ以降のリストiを塗ります。
ただし、csにaが含まれていたら飛ばします。eにaが含まれていても飛ばします。色を塗ろうとしている隣の点の色が同じ色なら飛ばします。
そうでなければ色を塗ります。

## 1.2. 点の隣のリストを求める

    let neighbors es =
      let add x y ns = match M.find_opt x ns with
        | None -> M.add x (S.singleton y) ns
        | Some xs -> M.add x (S.add y xs) ns
      in
      List.fold_left (fun ns (x,y) ->
        (add y x (add x y ns)) 
      ) M.empty es

辺のリストから点とその点の隣の点のリストを求めるには以上の関数で求めることができます。
点を受け取ったらyの隣にxを追加しxの隣にyを追加します。
add x y ns は nsのxの隣リストにyを加えます。nsにxの集合がないときは新たに作成して追加します。
xからyとyからxの両方に追加します。

## 1.3. グラフを表示する

    let dot cs es =
      let dot1(k,c) = sprintf "  %s [label=\"%s r%d\"]\n" k k c in
      let dot2(a,b) = sprintf "  %s -- %s\n" a b in
      "graph G{\n" ^
      String.concat "" (List.map dot1 (M.bindings cs)) ^
      String.concat "" (List.map dot2 es) ^ "}"
    let run es =
      let r = dot (coloring (neighbors es)) es in
      let fp = open_out "wml.dot" in fprintf fp "%s\n" r; close_out fp

グラフを描画するのがdot関数です。色リストと辺リストを受け取って文字列に変換します。
ファイルに出力までするので、dotコマンドでpng画像に変換すると画像として見れるようになります。

## 2. 基本ブロックの入出力からグラフ生成

我々は基本ブロックからレジスタ割付をグラフを用いて行いたいのでグラフから辺情報を取り出したい。
辺情報を取り出すためのレジスタ情報だけが含まれたカスタマイズされたデータ構造を作ってしまえば扱いが楽なはずです。
実際の基本ブロックの構造は無視して辺情報を取り出すための都合の良いデータ構造を用意してそれから辺情報を求めてみましょう。

グラフを生成するには生存解析を行ってから辺情報を集めることになります。

## 2.1. 基本ブロックの入出力データ形式

    type bb_io = {
      inp : S.t; (* 入力集合 *)
      out : S.t; (* 出力集合 *)
      block: (string list * string list) list; (* 各命令の出力レジスタリストと、入力レジスタリスト *)
      br : string list; (* 最後の命令の飛び先リスト *)
    }

## 2.2. 生存解析

生存解析では基本ブロックに入力されるレジスタ集合と、後続で用いられる出力レジスタ集合を求めます。

最初に後続の基本ブロックを最後のジャンプ命令から求めて飛び先の入力集合を出力集合に加えます。
出力集合を入力集合とします。

各命令をループして走査します。
各命令で参照されたレジスタは入力集合に追加します。
各命令での出力レジスタはレジスタがそこで定義されているので入力集合から消します。

すべてのブロックを走査し変更がなくなるまでくりかえします。

## 2.3. グラフ生成

生存解析結果の出力集合を用いることでグラフを生成します。

## 3. 構文

さてここからは、一度普通のプログラミング言語作成に戻りましょう。以下の構文の四則演算をまず出来る言語を考えます:

    program ::= (vdecl|fdecl)* EOF
    fdecl   ::= typ ID '(' (typ ID (',' typ ID)*)? ')' '{' vdecl* stmt* '}'
    typ     ::= 'int' | 'bool' | 'void'
    vdecl   ::= typ ID ';'
    stmt    ::= expr ';'
              | 'return' expr? ';'
              | '{' stmt* '}'
              | 'if' '(' expr ')' stmt ('else' stmt)?
              | 'for' '(' expr? ';' expr ';' expr? ')' stmt
              | 'while' '(' expr ')' stmt
    op      ::= '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '<=' | '>' | '>=' | '&&' | '||'
    unop    ::= '-' | '!'
    expr    ::= LITERAL | 'true' | 'false' | ID
              | expr binop expr | unop expr
              | ID '=' expr
              | ID '(' (expr (',' expr)*)? ')'
              | '(' expr ')'


eは式を表します。e には i(整数)と四則演算 e+e(足し算),e-e(引き算),e*e(掛け算),e/e(割り算) の構文要素があります。
ここからはこの言語をコンパイルすることを考えます。
今回は特にバックエンドに興味があるのでバックエンド側から考えていきます。

## 4. パーサ

ocamlyaccで作ります。

## 4.1. レキサ

ocamllexで作ります。

## 6. コンパイル

## 6.1. 基本ブロックの構造

    type reg = string
    type imm = IReg of reg | IInt of int
    type label = string
    type ir =
      | IMov of reg * imm
      | IAdd of reg * imm * imm
      | ICmp of reg * string * imm * imm
      | IRet of imm
      | IAlloca of reg
      | IBr of label
      | IBr3 of imm * label * label
      | ILoad of reg * imm
      | IStore of imm * imm
    type bb = string * ir list 

## 6.2. 基本ブロックの生成関数

    let init () =
      bb_name := "";
      bb := [];
      bbs := [];
      cnt := 0
    let new_reg () =
      incr cnt;
      string_of_int !cnt
    let new_bb () =
      incr cnt;
      let label = string_of_int !cnt in
      (label,[])
    let set_bb (label,bb1) =
      bb_name := label;
      bb := bb1
    let get_bb () = (!bb_name,!bb)
    let add ir =
      bb := ir :: !bb
    let add_bb bb =
      bbs := bb :: !bbs
    let get_bbs () =
      List.fold_left(fun bbs (label,bb) ->
        (label,List.rev bb)::bbs
      ) [] !bbs

## 6.3. コンパイル

コンパイル自体はすることがほとんどありません。

    let rec comp_e = function
      | EInt i -> IInt i
      | EAdd(e1,e2) ->
        let r1 = comp_e e1 in
        let r2 = comp_e e2 in
        let r3 = new_reg () in
        add (IAdd(r3,r1,r2));
        IReg r3

new_regでレジスタを生成してaddでIR命令を保存します。

    let rec compile e =
      set_bb (new_bb());
      let r = comp_e e in
      add (IRet r);
      add_bb (get_bb());
      get_bbs()

コンパイラのトップではベーシックブロックを作ってそこに命令を加えget_bb()で取り出してadd_bbで基本ブロックのリストに登録し、get_bbsで全体のプログラムを取得します。
LLVMのバインディングではコンテキストがどうのとかモジュールがどうのと面倒なのですが比較的単純なインターフェイスになっています。

## 7. 基本ブロックから入出力データ生成

基本ブロック内をトラバースして必要なデータを集めます。
特に重要なのがcollect_io_ir関数でirから入出力を以下のようにして求めます:

    let collect_io_ir = function
      | IMov(r,i1) -> [r],collect_io_imm[i1]
      | IAdd(r,i1,i2) -> [r],collect_io_imm[i1;i2]
      | ICmp(r,_,i1,i2) -> [r],collect_io_imm[i1;i2]
      | ILoad(r,i) -> [r],collect_io_imm[i]
      | IRet(i) -> [],collect_io_imm[i]
      | IAlloca(r) -> [r],[]
      | IBr(l) -> [],[]
      | IBr3(i,l1,l2) -> [],collect_io_imm[i]
      | IStore(i1,i2) -> [],collect_io_imm[i1;i2]

グラフ彩色は命令が増えても変わらずここのプログラムを変えれば良いので見通しがよく分かりやすいはずです。

## 8. x86コード生成

さて、レジスタの彩色はできたので、コード出力部分を作れば完成です。
コード出力部は基本ブロックリストとレジスタのマップを受け取って出力するようにします:
レジスタ名はここで初めて登場するのですが、emit_reg内で色リストcsから番号noを見つけ対応するレジスタを返すようにすることで、色つけします。

    let regs = [|"r10d"; "r11d"; "ebx"; "r12d"; "r13d"; "r14d"; "r15d"|]
    let cs : int M.t ref = ref (M.empty)
    let emit_reg r =
      try let no = M.find r !cs in
        "%" ^ regs.(no)
      with Not_found -> r

    let emit str = Printf.printf "\t%s\n" str
    let p str = Printf.printf "%s\n" str

    let emit_ir = function
      | IMov(r,i1)      -> emit ("movl "^emit_imm i1 ^", "^emit_reg r)
      | IAdd(r,i1,i2)   -> emit ("movl "^emit_imm i1 ^", "^emit_reg r);
                          emit ("addl "^emit_imm i2 ^", "^emit_reg r)
      | ICmp(r,_,i1,i2) -> emit ("")
      | ILoad(r,i)      -> emit ("")
      | IAlloca(r)      -> emit ("")
      | IRet(r)         -> emit ("movl "^emit_imm r ^", %eax")
      | IBr(l)          -> emit ("jmp L."^l)
      | IBr3(i,l1,l2)   -> emit ("")
      | IStore(i1,i2)   -> emit ("")

