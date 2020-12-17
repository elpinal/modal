structure Token = struct
  datatype t
    = LOWER_IDENT of string loc
    | UPPER_IDENT of string loc
    | QUOTE_IDENT of string loc
    | NUMBER of int loc

    | FUN of location
    | IN of location
    | LET of location
    | WITH of location
    | VAL of location
    | UNIT of location
    | BOOL of location
    | INT of location
    | TRUE of location
    | FALSE of location
    | MATCH of location
    | BOX of location
    | DIA of location

    | LPAREN of location
    | RPAREN of location
    | LBRACK of location
    | RBRACK of location
    | LBRACE of location
    | RBRACE of location

    | COLON of location
    | RARROW of location
    | RDARROW of location
    | STAR of location
    | EQUAL of location
    | DOT of location
    | COMMA of location
    | MINUS of location
    | PLUS of location
    | UNDERSCORE of location
    | BAR of location

  val show = fn
      LOWER_IDENT s => unloc s
    | UPPER_IDENT s => unloc s
    | QUOTE_IDENT s => "'" ^ unloc s
    | NUMBER n      => Int.toString (unloc n)

    | FUN   _ => "fun"
    | IN    _ => "in"
    | LET   _ => "let"
    | WITH  _ => "with"
    | VAL   _ => "val"
    | UNIT  _ => "unit"
    | BOOL  _ => "bool"
    | INT   _ => "int"
    | TRUE  _ => "true"
    | FALSE _ => "false"
    | MATCH _ => "match"
    | BOX   _ => "box"
    | DIA   _ => "dia"

    | LPAREN _ => "("
    | RPAREN _ => ")"
    | LBRACK _ => "["
    | RBRACK _ => "]"
    | LBRACE _ => "{"
    | RBRACE _ => "}"

    | COLON      _ => ":"
    | RARROW     _ => "->"
    | RDARROW    _ => "=>"
    | STAR       _ => "*"
    | EQUAL      _ => "="
    | DOT        _ => "."
    | COMMA      _ => ","
    | MINUS      _ => "-"
    | PLUS       _ => "+"
    | UNDERSCORE _ => "_"
    | BAR        _ => "|"

  val loc = fn
      LOWER_IDENT s => get_location s
    | UPPER_IDENT s => get_location s
    | QUOTE_IDENT s => get_location s
    | NUMBER n      => get_location n

    | FUN   l => l
    | IN    l => l
    | LET   l => l
    | WITH  l => l
    | VAL   l => l
    | UNIT  l => l
    | BOOL  l => l
    | INT   l => l
    | TRUE  l => l
    | FALSE l => l
    | MATCH l => l
    | BOX   l => l
    | DIA   l => l

    | LPAREN l => l
    | RPAREN l => l
    | LBRACK l => l
    | RBRACK l => l
    | LBRACE l => l
    | RBRACE l => l

    | COLON      l => l
    | RARROW     l => l
    | RDARROW    l => l
    | STAR       l => l
    | EQUAL      l => l
    | DOT        l => l
    | COMMA      l => l
    | MINUS      l => l
    | PLUS       l => l
    | UNDERSCORE l => l
    | BAR        l => l
end
