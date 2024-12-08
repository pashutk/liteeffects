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
%token PERFORM
%token DOT
%token EFFECT
%token HANDLE
%token WITH
%token COLON

%left PLUS
%nonassoc UMINUS

%start<Ast.exp> main

%%

main: e = expr EOF { e }

lambda: LPAREN params = separated_list(COMMA, ID) RPAREN ARROW LBRACE? e = expr RBRACE? { Lambda (params, e) }

app: id = ID LPAREN args = separated_list(COMMA, expr) RPAREN { App (id, args) }

handle_field: action = ID COLON value = expr { (action, value) }

expr:
| INT { Int $1 }
| CONST id = ID EQUALS value = expr SEMICOLON e = expr { Bound (id, value, e) }
| PERFORM effect = ID DOT action = ID LPAREN args = separated_list(COMMA, expr) RPAREN { Perform (effect, action, args) }
| EFFECT name = ID LBRACE actions = separated_list(COMMA, ID) RBRACE { Effect (name, actions) }
| HANDLE e = expr WITH effect = ID LBRACE actions = separated_list(COMMA, handle_field) RBRACE { Handle (e, effect, actions) }
| expr PLUS expr { Add ($1, $3) }
| expr ASTERISK expr { Mult ($1, $3) }
| lambda { $1 }
| app { $1 }
| ID { Ref $1 }

%%
