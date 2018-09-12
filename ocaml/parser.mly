%{ open Syntax %}
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token <int> LITERAL
%token <string> ID
%token EOF
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG
%start program
%type <Syntax.program> program
%%
program     : decls EOF                 { List.rev (fst $1), List.rev (snd $1) }
decls       : /* empty */               { [], [] }
            | decls vdecl               { ($2 :: fst $1), snd $1 }
            | decls fdecl               { fst $1, ($2 :: snd $1) }
fdecl       : typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
                                        { { typ=$1; fname=$2; formals=$4;
                                            locals=List.rev $7; body=List.rev $8 } }
formals_opt : /* empty */               { [] }
            | formal_list               { List.rev $1 }
formal_list : typ ID                    { [($1,$2)] }
            | formal_list COMMA typ ID  { ($3,$4) :: $1 }
typ         : INT                       { TInt }
            | BOOL                      { TBool }
            | VOID                      { TVoid }
vdecl_list  : /* empty */               { [] }
            | vdecl_list vdecl          { $2 :: $1 }
vdecl       : typ ID SEMI               { ($1, $2) }
stmt_list   : /* empty */               { [] }
            | stmt_list stmt            { $2 :: $1 }
stmt        : expr SEMI                 { SExpr $1 }
            | RETURN SEMI               { SReturn ENop }
            | RETURN expr SEMI          { SReturn $2 }
            | LBRACE stmt_list RBRACE   { SBlock(List.rev $2) }
            | IF LPAREN expr RPAREN stmt %prec NOELSE
                                        { SIf($3, $5, SBlock([])) }
            | IF LPAREN expr RPAREN stmt ELSE stmt
                                        { SIf($3, $5, $7) }
            | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                        { SFor($3, $5, $7, $9) }
            | WHILE LPAREN expr RPAREN stmt
                                        { SWhile($3, $5) }
expr_opt    : /* empty */               { ENop }
            | expr                      { $1 }
expr        : LITERAL                   { EInt($1) }
            | TRUE                      { EBool(true) }
            | FALSE                     { EBool(false) }
            | ID                        { EId($1) }
            | expr PLUS   expr          { EBin($1, EAdd, $3) }
            | expr MINUS  expr          { EBin($1, ESub, $3) }
            | expr TIMES  expr          { EBin($1, EMul, $3) }
            | expr DIVIDE expr          { EBin($1, EDiv, $3) }
            | expr EQ     expr          { EBin($1, EEq,  $3) }
            | expr NEQ    expr          { EBin($1, ENe,  $3) }
            | expr LT     expr          { EBin($1, ELt,  $3) }
            | expr LEQ    expr          { EBin($1, ELe,  $3) }
            | expr GT     expr          { EBin($1, EGt,  $3) }
            | expr GEQ    expr          { EBin($1, EGe,  $3) }
            | expr AND    expr          { EBin($1, EAnd, $3) }
            | expr OR     expr          { EBin($1, EOr,  $3) }
            | MINUS expr %prec NEG      { EUn(ENeg, $2) }
            | NOT expr                  { EUn(ENot, $2) }
            | ID ASSIGN expr            { EAssign($1, $3) }
            | ID LPAREN actuals_opt RPAREN
                                        { ECall($1, $3) }
            | LPAREN expr RPAREN        { $2 }
actuals_opt : /* empty */               { [] }
            | actuals_list              { List.rev $1 }
actuals_list: expr                      { [$1] }
            | actuals_list COMMA expr   { $3 :: $1 }
