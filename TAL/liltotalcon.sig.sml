signature LILTOTALCON = 
  sig
    val chatlev : int ref
    val debuglev : int ref
    val ktrans : Lil.kind -> Tal.kind
    val ctrans : LilToTalEnv.env -> Lil.con -> Tal.con
  end