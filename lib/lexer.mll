{
    open Parser
}

let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | '_')*
let whitespace = [' ' '\t' '\n' '\r']

rule token = parse
    | whitespace { token lexbuf }
    | "const" { CONST }
    | ";" { SEMICOLON }
    | "+" { PLUS }
    | "=" { EQUALS }
    | "=>" { ARROW }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "," { COMMA }
    | "*" { ASTERISK }
    | int as i { INT (int_of_string(i)) }
    | id as word { ID word }
    | eof { EOF }
