%token Eof
%token<int> Int
%token Plus
%token<float> Float
%start<Ast.exp> main

%left Plus

%{ open Ast %}

%%

main: expr Eof { $1 }

expr:
| Int { Int $1 }
| expr Plus expr { Add ($1, $3) }
| Float { Float $1 }


%%
