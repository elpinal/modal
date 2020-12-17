structure Env :> sig
  type t

  val initial : t

  structure Valid : sig
    exception Unbound of valid_var

    val lookup : t -> valid_var -> Type.t
    val insert : valid_var -> Type.t -> t -> t
  end

  structure Truth : sig
    exception Unbound of var

    val lookup : t -> var -> Type.t
    val insert : var -> Type.t -> t -> t

    val clear : t -> t
    val only : var -> Type.t -> t -> t
  end
end = struct
  structure ValidCtx = Map (ValidVar)
  structure TrueCtx = Map (Var)

  type t =
    { valid : Type.t ValidCtx.t
    , truth : Type.t TrueCtx.t
    }

  val initial =
    { valid = ValidCtx.empty
    , truth = TrueCtx.empty
    }

  structure Valid = struct
    exception Unbound of valid_var

    fun lookup (env : t) v =
      valOf (ValidCtx.lookup v (#valid env))
      handle Option => raise Unbound v

    fun insert v ty (env : t) =
      { valid = ValidCtx.insert v ty (#valid env)
      , truth = #truth env
      }
  end

  structure Truth = struct
    exception Unbound of var

    fun lookup (env : t) v =
      valOf (TrueCtx.lookup v (#truth env))
      handle Option => raise Unbound v

    fun insert v ty (env : t) =
      { truth = TrueCtx.insert v ty (#truth env)
      , valid = #valid env
      }

    fun clear (env : t) =
      { truth = TrueCtx.empty
      , valid = #valid env
      }

    fun only v ty (env : t) =
      { truth = TrueCtx.singleton v ty (* Discard the current true context. *)
      , valid = #valid env
      }
  end
end

type env = Env.t
