signature LILFREES = 
  sig
    (* returns exp and con free variable sets *)
    structure EC : 
      sig
	(* 32bit, 64bit, cons *)
	val freeInExp  : Lil.exp -> (Name.VarSet.set * Name.VarSet.set * Name.VarSet.set)
      end
    (* returns exp free variable set *)
    structure E : 
      sig 
	(* 32 bit and 64 bit together *)
	val freeInExp  : Lil.exp -> Name.VarSet.set 
	val freeInFunction  : Lil.function -> Name.VarSet.set 
      end
  end