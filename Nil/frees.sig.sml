signature FREES = 
  sig
    (* returns exp and con free variable sets *)
    structure EC : 
      sig
	val freeInExp  : Nil.exp -> (Name.VarSet.set * Name.VarSet.set)
	val freeInCon  : Nil.con -> (Name.VarSet.set * Name.VarSet.set)
	val freeInKind : Nil.kind -> (Name.VarSet.set * Name.VarSet.set)
      end
    (* returns exp free variable set *)
    structure E : 
      sig 
	val freeInExp  : Nil.exp -> Name.VarSet.set 
	val freeInFunction  : Nil.function -> Name.VarSet.set 
      end
  end