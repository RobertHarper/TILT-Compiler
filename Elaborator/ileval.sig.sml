(* The IL interpreter *)
signature ILEVAL = 
  sig
    structure Il : IL

    val debug : bool ref

    (* Valuability *)
    val con_isval : Il.con -> bool
    val exp_isval : Il.exp -> bool
    val mod_isval : Il.mod -> bool

    (* Evaluations *)
    val eval_con : Il.con -> Il.con
    val eval_exp : Il.exp -> Il.exp
    val eval_mod : Il.mod -> Il.mod

  end;
