signature EVAL = 
  sig 
    structure Il : JL
    val eval_exp : Il.exp -> Il.exp
  end

