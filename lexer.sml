structure LexerError = struct
  exception IllegalChar of position * char
end

structure Lexer : sig
  val lex : string -> Token.t Stream.stream
end = struct
  structure R = Reader (struct
    type position = position
    open Position
  end)

  open LexerError

  fun ident r : string loc =
  let
    val start = R.pos r
    fun token t = make_loc start (R.pos r) t
    val cs = [R.next r]
    fun go cs =
      let val c = R.peek r handle R.EOF => #" " in
        if Char.isAlphaNum c orelse c = #"_"
        then go (c :: cs before R.proceed r c)
        else token (String.implode (rev cs))
      end
  in
    go cs
  end

  fun lower r =
  let
    val Loc{location, content = s} = ident r
    val t =
      case s of
           "fun"   => Token.FUN
         | "in"    => Token.IN
         | "let"   => Token.LET
         | "val"   => Token.VAL
         | "unit"  => Token.UNIT
         | "bool"  => Token.BOOL
         | "int"   => Token.INT
         | "true"  => Token.TRUE
         | "false" => Token.FALSE
         | "match" => Token.MATCH
         | "box"   => Token.BOX
         | "dia"   => Token.DIA
         | _       => fn (l : location) => Token.LOWER_IDENT(Loc{location = l, content = s})
  in
    t location
  end

  val upper = Token.UPPER_IDENT o ident

  val ASCII_DIGIT_START = 48

  fun char_to_int c =
    Char.ord c - ASCII_DIGIT_START

  fun num r =
  let
    val start = R.pos r
    fun token t x = t (make_loc start (R.pos r) x)
    val n = char_to_int (R.next r)
    fun go n =
      let val c = R.peek r handle R.EOF => #" " in
        if Char.isDigit c
        then go (char_to_int c + n * 10 before R.proceed r c)
        else token Token.NUMBER n
      end
  in
    go n
  end

  (* Assume '-' is already consumed. *)
  fun hyphen token r =
    case R.peek_option r of
         SOME #">" => (R.proceed r #">"; token Token.RARROW)
       | _         => token Token.MINUS

  (* Assume '=' is already consumed. *)
  fun equal token r =
    case R.peek_option r of
         SOME #">" => (R.proceed r #">"; token Token.RDARROW)
       | _         => token Token.EQUAL

  fun lex1 r =
  let
    val start = R.pos r
    fun token t = t {from = start, to = (R.pos r)}
    val c = R.peek r
  in
    case c of
         #" "  => (R.proceed r c; lex1 r)
       | #"\n" => (R.proceed r c; lex1 r)
       | #"\t" => (R.proceed r c; lex1 r)
       | #"\r" => (R.proceed r c; lex1 r)

       | #"(" => (R.proceed r c; token Token.LPAREN)
       | #")" => (R.proceed r c; token Token.RPAREN)
       | #"[" => (R.proceed r c; token Token.LBRACK)
       | #"]" => (R.proceed r c; token Token.RBRACK)
       | #"{" => (R.proceed r c; token Token.LBRACE)
       | #"}" => (R.proceed r c; token Token.RBRACE)

       | #":" => (R.proceed r c; token Token.COLON)
       | #"*" => (R.proceed r c; token Token.STAR)
       | #"." => (R.proceed r c; token Token.DOT)
       | #"," => (R.proceed r c; token Token.COMMA)
       | #"+" => (R.proceed r c; token Token.PLUS)
       | #"_" => (R.proceed r c; token Token.UNDERSCORE)
       | #"|" => (R.proceed r c; token Token.BAR)
       | #"-" => (R.proceed r c; hyphen token r)
       | #"=" => (R.proceed r c; equal token r)
       | _    =>
           if Char.isLower c
           then lower r
           else if Char.isUpper c
           then upper r
           else if Char.isDigit c
           then num r
           else raise IllegalChar(start, c)
  end

  fun lex s =
  let
    val r = R.new s
    fun go acc = go (lex1 r :: acc)
      handle R.EOF => acc
  in
    Stream.fromList (rev (go []))
  end
end
