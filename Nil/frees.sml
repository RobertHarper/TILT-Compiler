structure Frees :> FREES = 
  struct
    open Nil
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet


    local
      open NilRewrite

      val efree = ref VarSet.empty
      val cfree = ref VarSet.empty

      type state = {bound : VarSet.set}

      fun exp_var_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)

      fun con_var_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)

      fun conhandler (state as {bound},con : con) =
	(case con
	   of Var_c var =>
	     if not(VarSet.member (bound, var)) then
	       (cfree := VarSet.add(!cfree, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)
	   
      fun exphandler (state as {bound}, exp : exp) =
	(case exp
	   of Var_e var =>
	     if not(VarSet.member (bound, var)) then
	       (efree := VarSet.add(!efree, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)
	   
      val exp_con_handler =
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_exphandler h  exphandler
	  val h = set_con_binder h con_var_bind
	  val h = set_exp_binder h exp_var_bind
	in h
	end

      val {rewrite_con = freeInCon',
	   rewrite_exp = freeInExp',
	   rewrite_kind = freeInKind',...} = rewriters exp_con_handler

      val empty_state  : state = {bound = VarSet.empty}

      fun freeInXXX freeer item =
	let
          val _ = (efree := VarSet.empty;
	           cfree := VarSet.empty)
	  val _  = freeer empty_state item
          val answer = (!efree, !cfree)
          val _ = (efree := VarSet.empty;
	           cfree := VarSet.empty)
	in
	  answer
	end

    in
      structure EC = 
	struct
	  val freeInCon = freeInXXX freeInCon'
	  val freeInExp = freeInXXX freeInExp'
	  val freeInKind = freeInXXX freeInKind'
	end
    end

    local
      open NilRewrite

      val efree = ref VarSet.empty

      type state = {bound : VarSet.set}

      fun exp_var_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)
	
      fun conhandler _ = NORECURSE
      fun kindhandler _ = NORECURSE

      fun exphandler (state as {bound}, exp : exp) =
	(case exp
	   of Var_e var =>
	     if not(VarSet.member (bound, var)) then
	       (efree := VarSet.add(!efree, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)
	   
      val exp_con_handler =
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_kindhandler h kindhandler
	  val h = set_exphandler  h exphandler
	  val h = set_exp_binder  h exp_var_bind
	in h
	end

      val {rewrite_exp = freeInExp',...} = rewriters exp_con_handler
	
      val empty_state  : state = {bound = VarSet.empty}

      fun freeInXXX freeer item =
	let
          val _ = efree := VarSet.empty
	  val _  = freeer empty_state item
          val answer = !efree
          val _ = efree := VarSet.empty
	in
	  answer
	end
      
    in
      structure E = 
	struct
	  val freeInExp = freeInXXX freeInExp'
	  fun freeInFunction (Function{eFormals    : (var * niltrace) list,
				       fFormals    : (var list),
				       body        : exp,
				       ...}) = 
	    let
	      val bfrees = freeInExp body
	      val bound = VarSet.union(VarSet.addList (VarSet.empty,map #1 eFormals),
				       VarSet.addList (VarSet.empty,fFormals))
	    in VarSet.difference(bfrees,bound)
	    end
	end
    end

  end