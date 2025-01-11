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
%token LT
%token GT

%left PLUS
%left ASTERISK
%nonassoc UMINUS

%start<Ast.exp> main

%%

main: e = expr EOF { e }

type_exp:
| TINT { TInt }
| LPAREN params = separated_list(COMMA, type_exp) RPAREN ARROW result = type_exp { TLambda (params, None, result) }
| LPAREN params = separated_list(COMMA, type_exp) RPAREN ARROW LT effects = separated_list(COMMA, ID) GT result = type_exp { TLambda (params, Some effects, result ) }

lambda_param: id = ID COLON ttype = type_exp { (id, ttype) }

lambda_params: LPAREN params = separated_list(COMMA, lambda_param) RPAREN { params }

lambda_body: LBRACE? body = expr RBRACE? { body }

lambda:
| params = lambda_params COLON LT effects = separated_list(COMMA, ID) GT ttype = type_exp ARROW body = lambda_body { Lambda (params, Some effects, Some ttype, body) }
| params = lambda_params COLON LT effects = separated_list(COMMA, ID) GT ARROW body = lambda_body { Lambda (params, Some effects, None, body) }
| params = lambda_params COLON ttype = type_exp ARROW body = lambda_body { Lambda (params, None, Some ttype, body) }
| params = lambda_params ARROW body = lambda_body { Lambda (params, None, None, body) }

app: id = ID LPAREN args = separated_list(COMMA, expr) RPAREN { App (id, args) }

handle_field: action = ID COLON value = expr { (action, value) }

effect_action: name = ID COLON ttype = type_exp { (name, ttype) }

effect: EFFECT name = ID LBRACE actions = separated_list(COMMA, effect_action) COMMA? RBRACE SEMICOLON? next = expr { Effect (name, actions, next) }

expr:
| INT { Int $1 }
| CONST id = ID COLON ttype = type_exp EQUALS value = expr SEMICOLON? e = expr { Bound (id, Some ttype, value, e) }
| CONST id = ID EQUALS value = expr SEMICOLON? e = expr { Bound (id, None, value, e) }
| PERFORM effect = ID DOT action = ID LPAREN args = separated_list(COMMA, expr) RPAREN { Perform (effect, action, args) }
| effect { $1 }
| HANDLE e = expr WITH effect = ID LBRACE actions = separated_list(COMMA, handle_field) RBRACE { Handle (e, effect, actions) }
| expr PLUS expr { Add ($1, $3) }
| expr ASTERISK expr { Mult ($1, $3) }
| lambda { $1 }
| app { $1 }
| ID { Ref $1 }

%%
