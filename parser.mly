/* Analyseur syntaxique pour PtiPython */

%{
  open Ast;;
%}
 
%token <string> CST
%token <string> STR
%token <string> IDENT
%token AND DEF FOR TRUE FALSE IN NOT OR RETURN NONE IF ELSE WHILE
%token EOF COLON
%token LP RP COMMA LB RB
%token PLUS MINUS TIMES DIV MOD
%token EQ EQQ
%token NEWLINE BEGIN END
%token LEQ GEQ LE GE NEQ

/* Définitions des priorités et associativités des tokens */
%left OR
%left AND
%nonassoc NOT
%nonassoc LE LEQ GE GEQ EQQ NEQ
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc uminus

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.prog> file

%%

file: NEWLINE? ; d = def* ;s = stmt+; NEWLINE ? ;EOF 
  {{defs = {name="-#-main-#-";body=Sblock(s),$startpos;args=[]}::d }} 
;
  
def: DEF ; nom = IDENT ; LP ; args = separated_list(COMMA,IDENT) ; RP ; COLON ; bod =  suite  
    {{name = nom ; args = args ; body = bod }}
;

suite: b = simple_stmt ; NEWLINE { b }
  | NEWLINE ; BEGIN ; s = stmt+ ; END { Sblock(s),$startpos }
;

left_value:
| s = IDENT { Var(s) }
| l = left_value; LB ; e = expr ; RB { Tab(l,e) }
;

simple_stmt: 
  | RETURN ; e = expr { Sreturn(e), $startpos } 
  | l = left_value ; EQ ; e = expr { Sassign(l,e), $startpos } 
  | e = expr { Sval(e), $startpos } 
;

stmt:
| s = simple_stmt ; NEWLINE  { s }
| FOR ; s = IDENT ; IN ; e = expr ; COLON ; b = suite  {Sfor(s,e,b), $startpos}
| IF ; cond = expr ; COLON ; b = suite  {Sif(cond, b), $startpos} 
| IF ; cond = expr ; COLON ; b = suite ; ELSE ; COLON ; e = suite  {Sifelse(cond,b,e), $startpos}
| WHILE ; cond = expr ; COLON; b = suite {Swhile(cond,b), $startpos}
;

expr:
| c = const                      { Const(c) }
| l = left_value                 { Val(l)}
| e1 = expr o = op e2 = expr     { Op(o,e1,e2) }
| MINUS e = expr %prec uminus    { Moins(e) } 
| NOT e = expr                   { Not(e) } 
| s = IDENT ; LP ; args = separated_list(COMMA,expr) ; RP { Ecall(s,args) }
| LB ; args = separated_list(COMMA,expr) ; RB { List(args)}
| LP ; e = expr ; RP { e}
;

%inline op:
| PLUS  { Add}
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
| MOD   { Mod }
| LEQ   { Leq }
| GEQ   { Geq }
| GE    { Ge  }
| LE    { Le  }
| NEQ   { Neq }
| EQQ   { Eq  }
| AND   { And }
| OR    { Or  } 
;

const:
| i = CST { Int(i) }
| s = STR { Str(s) }
| TRUE    { Bool(true)}
| FALSE   { Bool(false)}
| NONE    { Non }
;
