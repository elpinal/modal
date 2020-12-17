structure ParserError = struct
  exception UnexpectedEOF
  exception UnexpectedToken of Token.t
end

structure Parser = MakeParser (
  structure Streamable = StreamStreamable

  structure Arg = struct
    open Syntax
    open ParserError

    datatype terminal = datatype Token.t

    type location = location
    type string = string
    type int = int
    type string_loc = string loc
    type int_loc = int loc
    type ty = Type.t
    type expr = expr
    type term = term
    type var = var
    type valid_var = valid_var

    val true_var = Var.from_string

    val vvar = ValidVar.from_string

    fun term_id x = x

    fun tvar v = Loc
      { location = Var.loc v
      , content = Var v
      }

    fun tvvar v = Loc
      { location = ValidVar.loc v
      , content = ValidVar v
      }

    fun tabs (l, v, ty, x) = Loc
      { location = connect_locations l (get_location x)
      , content = Abs(v, ty, x)
      }

    fun tapp (x, y) = Loc
      { location = connect_locations (get_location x) (get_location y)
      , content = App(x, y)
      }

    fun tbox (l, x) = Loc
      { location = connect_locations l (get_location x)
      , content = Box x
      }

    fun tdia (l, x) = Loc
      { location = connect_locations l (get_location x)
      , content = Dia x
      }

    fun tletbox (l, v, t, x) = Loc
      { location = connect_locations l (get_location x)
      , content = LetBox(v, t, x)
      }

    fun expr_id x = x

    fun eletbox (l, v, t, e) = Loc
      { location = connect_locations l (get_location e)
      , content = LetBoxD(v, t, e)
      }

    fun eletdia (l, v, t, e) = Loc
      { location = connect_locations l (get_location e)
      , content = LetDia(v, t, e)
      }

    fun eterm (x, t, y) = Loc
      { location = connect_locations x y
      , content = Term t
      }

    fun type_id x = x

    fun tyarrow (x, y) = Loc
      { location = connect_locations (get_location x) (get_location y)
      , content = Type.Arrow(x, y)
      }

    fun tybool l = Loc
      { location = l
      , content = Type.Base Type.Bool
      }

    fun tyint l = Loc
      { location = l
      , content = Type.Base Type.Int
      }

    fun tybox (l, x) = Loc
      { location = connect_locations l (get_location x)
      , content = Type.Box x
      }

    fun tydia (l, x) = Loc
      { location = connect_locations l (get_location x)
      , content = Type.Diamond x
      }

    fun error (s : Token.t Stream.stream) =
    let open Stream in
      case front s of
           Nil        => UnexpectedEOF
         | Cons(t, _) => UnexpectedToken t
    end
  end
)
