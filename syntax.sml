structure Syntax = struct
  structure Type = struct
    datatype base
      = Int
      | Bool

    datatype t'
      = Base of base
      | Arrow of t * t
      | Box of t
      | Diamond of t

    withtype t = t' loc

    exception BaseMismatch of base loc * base loc
    exception TypeMismatch of t * t

    fun get_arrow f (ty : t) =
      case unloc ty of
           Arrow p => p
         | _       => raise (f ty)

    fun get_box f (ty : t) : t =
      case unloc ty of
           Box ty => ty
         | _      => raise (f ty)

    fun get_diamond f (ty : t) : t =
      case unloc ty of
           Diamond ty => ty
         | _          => raise (f ty)

    fun equal x y =
    let
      val loc1 = get_location x
      val loc2 = get_location y
    in
      case (unloc x, unloc y) of
           (Base b1, Base b2) =>
             if b1 = b2
             then ()
             else raise BaseMismatch
               ( Loc{location = loc1, content = b1}
               , Loc{location = loc2, content = b2}
               )
         | (Arrow(x1, x2), Arrow(y1, y2)) => equal x1 y1 before equal x2 y2
         | (Box x, Box y)                 => equal x y
         | (Diamond x, Diamond y)         => equal x y
         | _                              => raise TypeMismatch(x, y)
    end

    val show_base =
      fn Int  => "int"
       | Bool => "bool"

    val prec =
      fn Base _    => 0
       | Arrow _   => 5
       | Box _     => 3
       | Diamond _ => 3

    fun walk' p (w : t') =
      case w of
           Base b      => show_base b
         | Arrow(x, y) => right p w x " -> " y
         | Box x       => prefix p w "box " x
         | Diamond x   => prefix p w "dia " x

    and walk p (w : t) = walk' p (unloc w)

    and paren p (w : t') = "(" ^ walk' p w ^ ")"

    and special' p w =
      if p = prec w
      then paren p w
      else walk' p w

    and special p w = special' p (unloc w)

    and prefix p w i a =
    let val q = prec w in
      if p < q
      then paren q w
      else i ^ special q a
    end

    and left (p : int) (w : t') a i b =
    let val q = prec w in
      if p < q
      then paren q w
      else walk q a ^ i ^ special q b
    end

    and right (p : int) (w : t') (a : t) i (b : t) =
    let val q = prec w in
      if p < q
      then paren q w
      else special q a ^ i ^ walk q b
    end

    fun show ty = walk 100 ty
  end

  structure Term = struct
    structure Var :> sig
      type t

      val compare : t * t -> order

      val from_string : string loc -> t

      val loc : t -> location

      val show : t -> string
    end = struct
      type t = string loc

      fun compare (x, y) = String.compare (unloc x, unloc y)

      fun from_string s = s

      val loc = get_location

      val show = unloc
    end
    type var = Var.t

    structure ValidVar :> sig
      type t

      val compare : t * t -> order

      val from_string : string loc -> t

      val loc : t -> location

      val show : t -> string
    end = struct
      type t = string loc

      fun compare (x, y) = String.compare (unloc x, unloc y)

      fun from_string s = s

      val loc = get_location

      val show = unloc
    end
    type valid_var = ValidVar.t

    datatype term'
      = Var of var
      | Abs of var * Type.t * term
      | App of term * term
      | ValidVar of valid_var
      | Box of term
      | LetBox of valid_var * term * term
      | Dia of expr

    and expr'
      = Term of term
      | LetDia of var * term * expr
      | LetBoxD of valid_var * term * expr

    withtype term = term' loc
    and expr = expr' loc
  end

  open Term
end

open Syntax
