structure Position :> sig
  type t

  val initial : t
  val next : char -> t -> t

  val show : t -> string
end = struct
  type t = int * int

  val initial = (1, 1)

  fun newline (l, _) =
    (l + 1, 1)

  fun next c x =
    case c of
         #"\n" => newline x
       | _     => (#1 x, #2 x + 1)

  fun show (l, c) =
    Int.toString l ^ "." ^ Int.toString c
end

type position = Position.t

structure Loc :> sig
  type location =
    { from : position
    , to : position
    }

  datatype 'a loc = Loc of
    { content : 'a
    , location : location
    }

  val unloc : 'a loc -> 'a
  val get_location : 'a loc -> location
  val make_loc : position -> position -> 'a -> 'a loc
  val map_loc : ('a -> 'b) -> 'a loc -> 'b loc

  val connect_locations : location -> location -> location
  val show_location : location -> string
end = struct
  type location =
    { from : position
    , to : position
    }

  datatype 'a loc = Loc of
    { content : 'a
    , location : location
    }

  fun unloc (Loc r) = #content r

  fun get_location (Loc r) = #location r

  fun make_loc f t c = Loc {location = {from = f, to = t}, content = c}

  fun map_loc f (Loc r) = Loc {location = #location r, content = f (#content r)}

  fun connect_locations (x : location) (y : location) = {from = #from x, to = #to y}

  fun show_location {from, to} = Position.show from ^"-"^ Position.show to
end

open Loc
