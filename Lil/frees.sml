structure LilFrees :> LILFREES = 
  struct
    open Lil
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet


    local
      open LilRewrite

      val e32free = ref VarSet.empty
      val e64free = ref VarSet.empty
      val cfree = ref VarSet.empty

      type state = {bound : VarSet.set}

      fun exp_var32_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)
      fun exp_var64_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)

      fun con_var_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)

      fun kindhandler (state,kind : kind) = NORECURSE

      fun conhandler (state as {bound},con : con) =
	(case #c con
	   of Var_c var =>
	     if not(VarSet.member (bound, var)) then
	       (cfree := VarSet.add(!cfree, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)
	   
      fun sv32handler (state as {bound}, sv32 : sv32) =
	(case sv32
	   of Var_32 var =>
	     if not(VarSet.member (bound, var)) then
	       (e32free := VarSet.add(!e32free, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)

      fun sv64handler (state as {bound}, sv64 : sv64) =
	(case sv64
	   of Var_64 var =>
	     if not(VarSet.member (bound, var)) then
	       (e64free := VarSet.add(!e64free, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)
	   
      val exp_con_handler =
	let
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler = _,
	     exphandler,
	     sv32handler = _,
	     sv64handler = _,
	     kindhandler = _,
	     kind_var_bind,
	     con_var_bind,
	     exp_var32_bind = _,
	     exp_var64_bind = _
	     } : state handler = default_handler
	in
	  HANDLER 
	  {
	   bndhandler = bndhandler,
	   conhandler = conhandler,
	   exphandler = exphandler,
	   sv32handler = sv32handler,
	   sv64handler = sv64handler,
	   kindhandler = kindhandler,
	   kind_var_bind = kind_var_bind,
	   con_var_bind = con_var_bind,
	   exp_var32_bind = exp_var32_bind,
	   exp_var64_bind = exp_var64_bind
	   }
	end

      val {rewrite_exp = freeInExp',...} = rewriters exp_con_handler

      val empty_state  : state = {bound = VarSet.empty}

      fun freeInXXX freeer item =
	let
          val _ = (e32free := VarSet.empty;
		   e64free := VarSet.empty;
	           cfree := VarSet.empty)
	  val _  = freeer empty_state item
          val answer = (!e32free, !e64free, !cfree)
          val _ = (e32free := VarSet.empty;
		   e64free := VarSet.empty;
	           cfree := VarSet.empty)
	in
	  answer
	end

    in
      structure EC = 
	struct
	  val freeInExp = freeInXXX freeInExp'
	end
    end

    local
      open LilRewrite

      val efree = ref VarSet.empty

      type state = {bound : VarSet.set}

      fun exp_var32_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)
      fun exp_var64_bind (state : state as {bound},var) =
        ({bound = VarSet.add (bound, var)}, NONE)
	
      fun conhandler _ = NORECURSE
      fun kindhandler _ = NORECURSE

      fun sv32handler (state as {bound}, sv32 : sv32) =
	(case sv32
	   of Var_32 var =>
	     if not(VarSet.member (bound, var)) then
	       (efree := VarSet.add(!efree, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)

      fun sv64handler (state as {bound}, sv64 : sv64) =
	(case sv64
	   of Var_64 var =>
	     if not(VarSet.member (bound, var)) then
	       (efree := VarSet.add(!efree, var); NOCHANGE)
	     else
	       NOCHANGE
	    | _ => NOCHANGE)
	   
      val exp_con_handler =
	let
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler = _,
	     exphandler,
	     sv32handler = _,
	     sv64handler = _,
	     kindhandler = _,
	     kind_var_bind,
	     con_var_bind,
	     exp_var32_bind = _,
	     exp_var64_bind = _
	     } : state handler = default_handler
	in
	  HANDLER 
	  {
	   bndhandler = bndhandler,
	   conhandler = conhandler,
	   exphandler = exphandler,
	   sv32handler = sv32handler,
	   sv64handler = sv64handler,
	   kindhandler = kindhandler,
	   kind_var_bind = kind_var_bind,
	   con_var_bind = con_var_bind,
	   exp_var32_bind = exp_var32_bind,
	   exp_var64_bind = exp_var64_bind
	   }
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
	  fun freeInFunction (Function{eFormals, fFormals, body,...}) = 
	    let
	      val bfrees = freeInExp body
	      val bound = VarSet.union(VarSet.addList (VarSet.empty,map #1 eFormals),
				       VarSet.addList (VarSet.empty,map # 1 fFormals))
	    in VarSet.difference(bfrees,bound)
	    end
	end
    end
  
  end