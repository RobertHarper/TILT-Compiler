signature LILFREES = 
  sig
    (* returns exp and con free variable sets *)
    structure EC : 
      sig
	val freeInExp  : Lil.exp -> (Name.VarSet.set * Name.VarSet.set * Name.VarSet.set)
      end
    (* returns exp free variable set *)
    structure E : 
      sig 
	val freeInExp  : Lil.exp -> Name.VarSet.set 
	val freeInFunction  : Lil.function -> Name.VarSet.set 
      end
  end