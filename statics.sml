structure Statics : sig
  exception NotArrow of location * Type.t
  exception NotBox of location * Type.t
  exception NotDiamond of location * Type.t

  val type_of : env -> term -> Type.t
  val poss_type_of : env -> expr -> Type.t
end = struct
  exception NotArrow of location * Type.t
  exception NotBox of location * Type.t
  exception NotDiamond of location * Type.t

  infix 0 |>
  fun x |> f = f x

  fun type_of env t =
  let
    fun with_current_loc ty = Loc { location = get_location t, content = ty }
  in
    case unloc t of
         Var v          => Env.Truth.lookup env v
       | Abs(v, ty1, x) => Type.Arrow(ty1, type_of (Env.Truth.insert v ty1 env) x) |> with_current_loc
       | App(x, y)      =>
           let
             val (ty11, ty12) = type_of env x |> Type.get_arrow (fn ty => NotArrow(get_location x, ty))
             val ty2 = type_of env y
           in
             ty12 before Type.equal ty11 ty2
           end
       | ValidVar v      => Env.Valid.lookup env v
       | Box x           => Type.Box(type_of (Env.Truth.clear env) x) |> with_current_loc
       | LetBox(v, x, y) =>
           let val ty1 = type_of env x |> Type.get_box (fn ty => NotBox(get_location x, ty)) in
             type_of (Env.Valid.insert v ty1 env) y
           end
       | Dia e => Type.Diamond(poss_type_of env e) |> with_current_loc
  end

  and poss_type_of env e =
    case unloc e of
         Term t          => type_of env t
       | LetDia(v, t, x) =>
           let val ty1 = type_of env t |> Type.get_diamond (fn ty => NotDiamond(get_location t, ty)) in
             poss_type_of (Env.Truth.only v ty1 env) x
           end
       | LetBoxD(v, t, x) =>
           let val ty1 = type_of env t |> Type.get_box (fn ty => NotBox(get_location t, ty)) in
             poss_type_of (Env.Valid.insert v ty1 env) x
           end
end
