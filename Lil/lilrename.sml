(*$import Sequence HashTable Alpha Pplil Util Name Lil LILRENAME Stats LilError LilRewrite Option *)

structure LilRename :> LILRENAME = 
  struct
    open Lil
      
    val lprintl = Util.lprintl
    val printl = Util.printl

    fun error s = Util.error "lilrename.sml" s

    val isSome = Option.isSome

    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k


    type 'a map = 'a Name.VarMap.map

    (* Normal renaming *)
    local
      open LilRewrite

      type state = {exp_subst : var map,con_subst : var map,kind_subst : var map}

      fun exp_var_bind ({exp_subst,con_subst, kind_subst} : state,var) = 
	let
	  val var' = Name.derived_var var
	  val exp_subst = VarMap.insert (exp_subst,var,var')
	in
	  ({exp_subst = exp_subst,con_subst = con_subst,kind_subst=kind_subst},SOME var')
	end

      fun con_var_bind ({exp_subst,con_subst,kind_subst} : state,var) = 
	let
	  val var' = Name.derived_var var
	  val con_subst = VarMap.insert (con_subst,var,var')
	in
	  ({exp_subst = exp_subst,con_subst = con_subst,kind_subst=kind_subst},SOME var')
	end

      fun kind_var_bind ({exp_subst,con_subst,kind_subst} : state,var) = 
	let
	  val var' = Name.derived_var var
	  val kind_subst = VarMap.insert (kind_subst,var,var')
	in
	  ({exp_subst = exp_subst,con_subst = con_subst,kind_subst=kind_subst},SOME var')
	end

      fun kindhandler (state as {kind_subst,...} : state,kind : kind) =
	(case kout kind
	   of Var_k j => 
	     (case VarMap.find (kind_subst,j)
		of SOME j => (CHANGE_NORECURSE (state,mk_kind (Var_k j)))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val x = ref 0
      fun conhandler (state as {con_subst,...} : state,con : con) =
	(case cout con
	   of Var_c var => 
	     (case VarMap.find (con_subst,var)
		of SOME var => (CHANGE_NORECURSE (state,mk_con (Var_c var)))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      fun sv32handler (state as {exp_subst,...} : state,sv32 : sv32) =
	(case sv32
	   of Var_32 var => 
	     (case VarMap.find (exp_subst,var)
		of SOME var => (CHANGE_NORECURSE (state,Var_32 var))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      fun sv64handler (state as {exp_subst,...} : state,sv64 : sv64) =
	(case sv64
	   of Var_64 var => 
	     (case VarMap.find (exp_subst,var)
		of SOME var => (CHANGE_NORECURSE (state,Var_64 var))
		 | _ => NORECURSE)
	    | _ => NOCHANGE)

      val exponly_handlers = 
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
	   conhandler = fn _ => NORECURSE,
	   exphandler = exphandler,
	   sv32handler = sv32handler,
	   sv64handler = sv64handler,
	   kindhandler = fn _ => NORECURSE,
	   kind_var_bind = kind_var_bind,
	   con_var_bind = con_var_bind,
	   exp_var32_bind = exp_var_bind,
	   exp_var64_bind = exp_var_bind
	   }
	end

      val {rewrite_exp = renameEVarsExp',...} = rewriters exponly_handlers

      val con_handlers = 
	let   
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler = _,
	     exphandler,
	     sv32handler,
	     sv64handler,
	     kindhandler = _,
	     kind_var_bind,
	     con_var_bind = _,
	     exp_var32_bind,
	     exp_var64_bind
	     } : state handler = default_handler
	in
	  HANDLER 
	  {
	   bndhandler = bndhandler,
	   conhandler = conhandler,
	   exphandler = exphandler,
	   sv32handler = sv32handler,
	   sv64handler = sv64handler,
	   kindhandler = fn _ => NORECURSE,
	   kind_var_bind = kind_var_bind,
	   con_var_bind = con_var_bind,
	   exp_var32_bind = exp_var_bind,
	   exp_var64_bind = exp_var_bind
	   }
	end

      val {rewrite_con = renameCVarsCon',...} = rewriters con_handlers

      val kind_handlers = 
	let   
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler,
	     exphandler,
	     sv32handler,
	     sv64handler,
	     kindhandler = _,
	     kind_var_bind = _,
	     con_var_bind,
	     exp_var32_bind,
	     exp_var64_bind
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
	   exp_var32_bind = exp_var_bind,
	   exp_var64_bind = exp_var_bind
	   }
	end

      val {rewrite_kind = renameKVarsKind',...} = rewriters kind_handlers

      val all_handlers =  
	let   
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler = _,
	     exphandler,
	     sv32handler = _,
	     sv64handler = _,
	     kindhandler = _,
	     kind_var_bind = _,
	     con_var_bind = _,
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
	   exp_var32_bind = exp_var_bind,
	   exp_var64_bind = exp_var_bind
	   }
	end 

      val {rewrite_exp = renameExp',
	   rewrite_sv32 = renameSv32',
	   rewrite_sv64 = renameSv64',
	   rewrite_con = renameCon',
	   rewrite_kind = renameKind',
	   rewrite_bnd = renameBnd',...} = rewriters all_handlers

      fun empty_state () = {exp_subst = VarMap.empty,con_subst = VarMap.empty,kind_subst = VarMap.empty}


    in
      val renameEVarsExp = renameEVarsExp' (empty_state())
      val renameCVarsCon = renameCVarsCon' (empty_state())
      val renameKVarsKind = renameKVarsKind' (empty_state())

      val renameExp = renameExp' (empty_state())
      val renameSv32 = renameSv32' (empty_state())
      val renameSv64 = renameSv64' (empty_state())
      val renameCon = renameCon' (empty_state())
      val renameKind = renameKind' (empty_state())

      fun renameBnd bnd = 
	let
	  val ({con_subst,exp_subst,kind_subst},bnds) = renameBnd' (empty_state ()) bnd
	in
	  (hd bnds,(exp_subst,con_subst,kind_subst))
	end

    end
  
    (*Is renamed predicate *)
    local 
      open LilRewrite

      val ExpGlobalRename = ref true
      val ConGlobalRename = ref false
      val KindGlobalRename = ref false

      val continue = Stats.ff "LilRenameContinue"

      exception Rebound of var
      exception Unbound 

      datatype varset = HASH of (var,unit) HashTable.hash_table | SET of VarSet.set
      type state = {kpred : var -> bool,
		    cpred : var -> bool,
		    epred : var -> bool,
		    kbound : varset,
		    cbound : varset,
		    ebound : varset}

      fun member s v = (case s 
			of HASH h => isSome(HashTable.find h v)
			 | SET s => VarSet.member (s,v))
      fun insert s v = (case s
			  of HASH h => (HashTable.insert h (v,());s)
			   | SET s => SET (VarSet.add (s,v)))

      fun exp_var_bind (state : state as {epred,cpred,kpred,ebound,cbound,kbound},var) = 
	let 
	  val ebound =
	    if member ebound var orelse (epred var) then 
	      if !continue then 
		(lprintl ("Expression variable "^(Name.var2string var)^" rebound: continuing");ebound)
	      else raise Rebound var
	    else insert ebound var
	  val state = {epred=epred,cpred=cpred,kpred=kpred,ebound=ebound,cbound=cbound,kbound=kbound}
	in (state,NONE)
	end

      fun con_var_bind (state : state as {epred,cpred,kpred,ebound,cbound,kbound},var) = 
	let 
	  val cbound = if member cbound var orelse (cpred var)
			 then if !continue then (lprintl ("Constructor variable "^(Name.var2string var)^" rebound: continuing");cbound)
			      else raise Rebound var
		      else insert cbound var
	  val state = {epred=epred,cpred=cpred,kpred=kpred,ebound=ebound,cbound=cbound,kbound=kbound}
	in (state,NONE)
	end

      fun kind_var_bind (state : state as {epred,cpred,kpred,ebound,cbound,kbound},var) = 
	let 
	  val kbound = if member kbound var orelse (kpred var)
			 then if !continue then (lprintl ("Kind variable "^(Name.var2string var)^" rebound: continuing");kbound)
			      else raise Rebound var
		       else insert kbound var
	  val state = {epred=epred,cpred=cpred,kpred=kpred,ebound=ebound,cbound=cbound,kbound=kbound}
	in (state,NONE)
	end

      val all_handlers =  
	let   
	  val HANDLER 
	    {
	     bndhandler,
	     conhandler,
	     exphandler,
	     sv32handler,
	     sv64handler,
	     kindhandler,
	     kind_var_bind = _,
	     con_var_bind = _,
	     exp_var32_bind = _ ,
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
	   exp_var32_bind = exp_var_bind,
	   exp_var64_bind = exp_var_bind
	   }
	end 

      val {rewrite_exp = checkExp,
	   rewrite_con = checkCon,
	   rewrite_kind = checkKind,...} = rewriters all_handlers

      fun isRenamedXXX checker (epred,cpred,kpred) item = 
	let
	  val kbound = if !KindGlobalRename 
			 then HASH (Name.mk_var_hash_table(20,Unbound)) 
		       else SET (VarSet.empty)
	  val cbound = if !ConGlobalRename 
			 then HASH (Name.mk_var_hash_table(20,Unbound)) 
		       else SET (VarSet.empty)
	  val ebound = if !ExpGlobalRename 
			 then HASH (Name.mk_var_hash_table(20,Unbound)) 
		       else SET (VarSet.empty)
	in 
	  ((checker {kpred = kpred, kbound = kbound,cpred = cpred,epred = epred,cbound = cbound,ebound = ebound} item; 
	    true)
	   handle Rebound var => 
	     (lprintl ("Variable "^(Name.var2string var)^" rebound");
	      false))
	end

      fun ff _ = false
    in
      val isRenamedExp = isRenamedXXX checkExp (ff,ff,ff)
      val isRenamedCon = isRenamedXXX checkCon (ff,ff,ff)
      val isRenamedKind = isRenamedXXX checkKind (ff,ff,ff)

      val isRenamedExpWRT = isRenamedXXX checkExp
      val isRenamedConWRT = isRenamedXXX checkCon
      val isRenamedKindWRT = isRenamedXXX checkKind
    end

  end
