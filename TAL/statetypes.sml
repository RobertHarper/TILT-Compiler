structure StateTypes =
  struct
      
    datatype con = LilCon of Lil.con | TalCon of Tal.con

    datatype rcont = 
      Junk  (* Empty or reserved for temp (not live across calls).  
	     * If we want to allow temps live across calls, must allow 
	     * a type here so that we can get the correct machine state. *)
    | Full of (con * Lil.var)  (* Full (c,x) holds variable x : c *)
      


    (* Reserved registers are locked down (usually because they have
     * been allocated, but not yet initialized).  Available registers
     * are available for spilling or use (depending on the rcont) *)
    datatype rstat = Reserved of rcont | Avail of rcont


    fun rcont2string c = 
      (case c
	 of Junk => "junk"
	  | Full(c,x) => ("Contains " ^ (Name.var2string x)))
    fun rstat2string r = 
      (case r
	 of Avail c => ("Avail (" ^ (rcont2string c)^ ")")
	  | Reserved c => ("Reserved (" ^ (rcont2string c) ^ ")"))

    val print_rcont = print o rcont2string
    val print_rstat = print o rstat2string


    fun eq_rcont (rc1,rc2) = 
      case (rc1,rc2) 
	of (Junk,Junk) => true
	 | (Full(c1,x1),Full(c2,x2)) => Name.eq_var (x1,x2)
	 | _ => false
      
    fun eq_rstat (r1,r2) = 
      (case (r1,r2)
	 of (Avail rc1, Avail rc2) => eq_rcont (rc1,rc2)
	  | (Reserved rc1, Reserved rc2) => eq_rcont (rc1,rc2)
	  | _ => false)

    fun rstat2rcont rstat = 
      (case rstat
	 of Avail rc => rc
	  | Reserved rc => rc)

  end

