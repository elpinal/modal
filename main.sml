open Modal

fun main () =
let
  val t = parse_file ",example.modal"
  val ty = typecheck t
  val s = Type.show ty ^ "\n"
in
  print s
end

val () = main ()
