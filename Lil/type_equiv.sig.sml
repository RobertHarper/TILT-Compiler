signature LILTYPEEQUIV = 
  sig
    structure K :
      sig 
	val equal : Lil.kind -> Lil.kind -> bool
      end

    structure C :
      sig 
	val equal : Lil.con -> Lil.con -> bool
      end

  end