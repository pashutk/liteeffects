%{ open Ast %}

%token EOF
%token <int> INT
%token PLUS
%token EQUALS
%token CONST
%token <string> ID

%left PLUS
%nonassoc UMINUS

%start<Ast.exp> main

%%

main: e = expr EOF { e }

expr:
| INT { Int $1 }
| CONST id = ID EQUALS e = expr { Const (id, e) }
| expr PLUS expr { Add ($1, $3) }

%%
