signature LILTYPECHECK = 
  sig
    val diag : bool ref
    val chatlev : int ref
    val limit : int ref

    structure K :
      sig
	val check : LilContext.context -> Lil.kind -> unit
      end

    structure C :
      sig
	val check : LilContext.context -> Lil.con -> Lil.kind -> unit
	val synth : LilContext.context -> Lil.con -> Lil.kind 
      end

    structure T :
      sig
	val check32 : LilContext.context -> Lil.con -> unit
	val check64 : LilContext.context -> Lil.con -> unit
      end

    structure E :
      sig
	val check : LilContext.context -> Lil.exp -> Lil.con -> unit
	val synth : LilContext.context -> Lil.exp -> unit
      end

    structure M : 
      sig
	val check : Lil.module -> unit
      end

  end