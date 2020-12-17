structure Modal : sig
  val parse_string : string -> term
  val parse_file : string -> term
  val typecheck : term -> Type.t
end = struct
  fun fail s =
    ( TextIO.output (TextIO.stdErr, s ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )

  fun parse_string s =
  let
    val s = Lexer.lex s
    val t = #1 (Parser.parse s)
  in
    t
  end handle
      LexerError.IllegalChar(p, c) =>
        fail (Position.show p ^ ": illegal character: " ^ Char.toString c)
    | ParserError.UnexpectedEOF => fail "unexpected end of file"
    | ParserError.UnexpectedToken t =>
        fail (show_location (Token.loc t) ^ ": unexpected token: " ^ Token.show t)

  fun parse_file name =
  let
    val ins = TextIO.openIn name handle IO.Io _ => fail ("cannot open: " ^ name)
    val s = TextIO.inputAll ins
    val () = TextIO.closeIn ins
  in
    parse_string s
  end

  fun typecheck t =
    Statics.type_of Env.initial t handle
        Env.Valid.Unbound v =>
          fail (show_location (ValidVar.loc v) ^ ": unbound valid variable: " ^ ValidVar.show v)
      | Env.Truth.Unbound v => fail (show_location (Var.loc v) ^ ": unbound variable: " ^ Var.show v)
      | Type.BaseMismatch(b1, b2) =>
          let
            val s1 = Type.show_base (unloc b1)
            val s2 = Type.show_base (unloc b2)
            val l1 = get_location b1
            val l2 = get_location b2
            val x = s1 ^ " (at " ^ show_location l1 ^ ")"
            val y = s2 ^ " (at " ^ show_location l2 ^ ")"
          in
            fail ("base type mismatch: " ^ x ^ " vs " ^ y)
          end
      | Type.TypeMismatch(t1, t2) =>
          let
            val s1 = Type.show t1
            val s2 = Type.show t2
            val l1 = get_location t1
            val l2 = get_location t2
            val x = s1 ^ " (at " ^ show_location l1 ^ ")"
            val y = s2 ^ " (at " ^ show_location l2 ^ ")"
          in
            fail ("type mismatch: " ^ x ^ " vs " ^ y)
          end
      | Statics.NotArrow(loc, ty) =>
          let
            val l = get_location ty
            val x = Type.show ty ^ " (at " ^ show_location l ^ ")"
          in
            fail (show_location loc ^ ": not function type: " ^ x)
          end
      | Statics.NotBox(loc, ty) =>
          let
            val l = get_location ty
            val x = Type.show ty ^ " (at " ^ show_location l ^ ")"
          in
            fail (show_location loc ^ ": not box type: " ^ x)
          end
      | Statics.NotDiamond(loc, ty) =>
          let
            val l = get_location ty
            val x = Type.show ty ^ " (at " ^ show_location l ^ ")"
          in
            fail (show_location loc ^ ": not diamond type: " ^ x)
          end
end
