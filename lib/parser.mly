%{ open Ast %}

%token EOF
%token <int> INT
%token PLUS
%token EQUALS
%token CONST
%token <string> ID
%token FUNCTION
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token SEMICOLON
%token ASTERISK

%left PLUS
%nonassoc UMINUS

%start<Ast.exp> main

%%

main: e = expr EOF { e }

func: FUNCTION name = ID LPAREN args = separated_list(COMMA, ID) RPAREN LBRACE e = expr RBRACE { Function (name, args, e) }

expr:
| INT { Int $1 }
| CONST id = ID EQUALS value = expr SEMICOLON e = expr { Bound (id, value, e) }
| expr PLUS expr { Add ($1, $3) }
| expr ASTERISK expr { Mult ($1, $3) }
| func { $1 }
| ID { Ref $1 }

%%
