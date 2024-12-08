%{ open Ast %}

%token EOF
%token <int> INT
%token PLUS
%token CONST
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token SEMICOLON
%token ASTERISK
%token EQUALS ARROW

%left PLUS
%nonassoc UMINUS

%start<Ast.exp> main

%%

main: e = expr EOF { e }

lambda: LPAREN params = separated_list(COMMA, ID) RPAREN ARROW LBRACE e = expr RBRACE { Lambda (params, e) }

app: id = ID LPAREN e = expr RPAREN { App (id, e) }

expr:
| INT { Int $1 }
| CONST id = ID EQUALS value = expr SEMICOLON e = expr { Bound (id, value, e) }
| expr PLUS expr { Add ($1, $3) }
| expr ASTERISK expr { Mult ($1, $3) }
| lambda { $1 }
| app { $1 }
| ID { Ref $1 }

%%
