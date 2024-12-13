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
%token TINT

%left PLUS
%nonassoc UMINUS

%start<Ast.exp> main

%%

main: e = expr EOF { e }

type_exp: TINT { TInt }

lambda_param: id = ID COLON ttype = type_exp { (id, ttype) }

lambda_params: LPAREN params = separated_list(COMMA, lambda_param) RPAREN { params }

lambda_body: LBRACE? body = expr RBRACE? { body }

lambda:
| params = lambda_params COLON ttype = type_exp ARROW body = lambda_body { Lambda (params, Some ttype, body) }
| params = lambda_params ARROW body = lambda_body { Lambda (params, None, body) }

app: id = ID LPAREN args = separated_list(COMMA, expr) RPAREN { App (id, args) }

handle_field: action = ID COLON value = expr { (action, value) }

expr:
| INT { Int $1 }
| CONST id = ID EQUALS value = expr SEMICOLON? e = expr { Bound (id, value, e) }
| PERFORM effect = ID DOT action = ID LPAREN args = separated_list(COMMA, expr) RPAREN { Perform (effect, action, args) }
| EFFECT name = ID LBRACE actions = separated_list(COMMA, ID) RBRACE { Effect (name, actions) }
| HANDLE e = expr WITH effect = ID LBRACE actions = separated_list(COMMA, handle_field) RBRACE { Handle (e, effect, actions) }
| expr PLUS expr { Add ($1, $3) }
| expr ASTERISK expr { Mult ($1, $3) }
| lambda { $1 }
| app { $1 }
| ID { Ref $1 }

%%
