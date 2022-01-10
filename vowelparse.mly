/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA PLUS MINUS TIMES DIVIDE INTERSEC
%token MODULUS ASSIGN
%token INCREMENT DECREMENT
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID STRING
%token ARRAY
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT SLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right INCREMENT DECREMENT ASSIGN 
%left INTERSEC
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [], [])         }
 | decls stmt { let (stmt, vdecl, fdecl) = $1 in ($2::stmt, vdecl, fdecl) }
 | decls vdecl { let (stmt, vdecl, fdecl) = $1 in (stmt, $2::vdecl, fdecl) }
 | decls fdecl { let (stmt, vdecl, fdecl) = $1 in (stmt, vdecl, $2::fdecl) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = [];
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT    { Int   }
  | BOOL   { Bool  }
  | FLOAT  { Float }
  | VOID   { Void  }
  | STRING { String }
  | typ ARRAY { Arr($1, 0) }

/*
  vdecl_list:
        { [] }
  | vdecl_list vdecl { $2 :: $1 }

*/

vdecl:
    typ ID SEMI { ($1, $2) }


stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }


stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

/* assignment_op:
  ASSIGN       { Assign }
| INCREMENT    { Incr }
| DECREMENT    { Decr } */

expr:
    LITERAL          { Literal($1)            }
  | FLIT	           { Fliteral($1)           }
  | SLIT             { STRliteral($1)         }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MODULUS expr{ Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr INTERSEC expr {Binop($1, Intersec, $3)}
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | ID ASSIGN expr    { Assign($1, $3)        }
  | ID INCREMENT expr { Increment($1, $3)    }
  | ID DECREMENT expr { Decrement($1, $3)     } 
  /* Arrays */
  | ID LBRACKET expr RBRACKET { ArrayAccess($1, $3) }    
  | LBRACKET args_list RBRACKET { ArrayLit($2) }        
  | ID LBRACKET expr RBRACKET ASSIGN expr { ArrAssign($1, $3, $6) }      
  /* VARIABLE DECLARATION */
  | typ ID ASSIGN expr { DeclAssn($1, $2, $4) }
  


args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
