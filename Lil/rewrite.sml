(*$import Name List Sequence Prim Array TraceInfo Listops Util Lil LILREWRITE *)

(*
 * A simple rewriter that does not try to synthesize types.
 *
 * The idea is to provide a generic traversal algorithm that crawls over 
 * a parse tree in simple block structured fashion.  At every node, it
 * provides the client code with the current node under consideration 
 * and a current state.  The client then returns 
 * 
 * NOCHANGE        if the node was not changed, and should be
 *                 recursively traversed
 * NORECURSE       if the node was not changed, and should not be 
 *                 recursively traversed
 * CHANGE_RECURSE (state,term)
 *                 if the node was changed to "term" with new state "state",
 *                 and the new term should be recursively traversed
 * CHANGE_NORECURSE (state,term)
 *                 if the node was changed to "term" with new state "state",
 *                 and the new term should not be recursively traversed
 *
 * This version of the code does not try to preserve physical sharing.
 *)

structure LilRewrite :> LILREWRITE = 
  struct
    open Lil

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

      
    val foldl_acc = Listops.foldl_acc
    val foldl_acc2 = Listops.foldl_acc2
    val map_second = Listops.map_second
    val mapopt = Util.mapopt
    fun error s = Util.error "lilrewrite.sml" s
    val lprintl = Util.lprintl

    val getOpt = Option.getOpt

    fun flip (a,b) = (b,a)

    datatype 'a changeopt = NOCHANGE | NORECURSE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a

    (* A term handler is a client function which takes a state and a term,
     * and possibly returns a new term and state.
     *
     * A bnd handler does the same for bnds.  For generality however,
     * the handler has the option of returning a list of bnds.
     * If one of the RECURSE options is returned, then the bnds will
     * be bound in the state returned.  Otherwise, they will not.
     *
     * A binder is a function which takes a variable and a state, and returns 
     * a new state and possibly a new variable.  
     *)

    type ('state,'term) termhandler = 'state * 'term -> ('state * 'term) changeopt
    type ('state, 'bnd) bndhandler  = 'state * 'bnd  -> ('state * 'bnd list) changeopt
    type 'state binder              = 'state * var   -> ('state * var option)

    datatype 'state handler =
      HANDLER of {
		  bndhandler : ('state,bnd) bndhandler,
		  conhandler   : ('state,con) termhandler,
		  exphandler   : ('state,exp) termhandler,
		  sv32handler  : ('state,sv32) termhandler,
		  sv64handler  : ('state,sv64) termhandler,
		  kindhandler  : ('state,kind) termhandler,
		  kind_var_bind    : 'state binder,
		  con_var_bind     : 'state binder,
		  exp_var32_bind   : 'state binder,
		  exp_var64_bind   : 'state binder
		  }

      
    fun rewriters (handler : 'state handler) 
      : {
	 rewrite_kind  : 'state -> Lil.kind -> Lil.kind,
	 rewrite_con   : 'state -> Lil.con -> Lil.con,
	 rewrite_exp   : 'state -> Lil.exp -> Lil.exp,
	 rewrite_op32  : 'state -> Lil.op32 -> Lil.op32,
	 rewrite_op64  : 'state -> Lil.op64 -> Lil.op64,
	 rewrite_sv32  : 'state -> Lil.sv32 -> Lil.sv32,
	 rewrite_sv64  : 'state -> Lil.sv64 -> Lil.sv64,
	 rewrite_bnd   : 'state -> Lil.bnd -> ('state * (Lil.bnd list))
	 }
      =
      let

	val (HANDLER{bndhandler,
		     exphandler,
		     sv32handler,
		     sv64handler,
		     conhandler,
		     kindhandler,
		     kind_var_bind,
		     con_var_bind,
		     exp_var32_bind,
		     exp_var64_bind}) = handler

	fun bind_var binder (state,v) = 
	  let
	    val (state,v_o) = binder (state,v)
	    val v = getOpt (v_o,v)
	  in (state,v)
	  end

	val kind_var_bind   = bind_var kind_var_bind
	val con_var_bind    = bind_var con_var_bind
	val exp_var32_bind    = bind_var exp_var32_bind
	val exp_var64_bind  = bind_var exp_var64_bind

	fun call_client_code itemhandler doitem state item = 
	  (case (itemhandler (state,item))         	    (* First pass the item to the client  *)
	     of CHANGE_NORECURSE (state,i) => i             (* This item is finished *)
	      | CHANGE_RECURSE (state,i)   => doitem (state,i)
                                                            (* Recursively traverse the new item. *)
	      | NOCHANGE  => doitem (state,item)            (* Recursively traverse the original item *)
	      | NORECURSE => item)                          (* We're done! *)

	fun rewrite_con (state : 'state) (con : con) : con =  
	  let 
	    fun docon (state,con : con) = 
	      let 
		val recur = rewrite_con state
		val recur_k = rewrite_kind state
	      in
		case cout con of
		  Var_c v => con
		 | Nat_c w => con
		 | App_c (c1,c2) => mk_con(App_c (recur c1,recur c2))
		 | APP_c (c,k) => mk_con(APP_c (recur c,recur_k k))
		 | Pi1_c c => mk_con(Pi1_c (recur c))
		 | Pi2_c c => mk_con(Pi2_c (recur c))
		 | Prim_c p => con
		 | Pr_c (j,(a,k),k',r,body) =>
		   let
		     val (state,j) = kind_var_bind(state,j)
		     val k' = rewrite_kind state k'
		     val k  = rewrite_kind state k
		     val (state,a) = con_var_bind(state,a)
		     val (state,r) = con_var_bind(state,r)
		     val body = rewrite_con state body
		   in mk_con(Pr_c (j,(a,k),k',r,body))
		   end

		 | Case_c (c,arms,default)=> 
		   let
		     val c = recur c
		     fun do_arm (w,(a,c)) =
		       let
			 val (state,a) = con_var_bind (state,a)
		       in (w,(a,rewrite_con state c))
		       end
		     val arms = map do_arm arms
		     val default = mapopt recur default
		   in mk_con(Case_c (c,arms,default))
		   end
		 | LAM_c (j,c) =>
		   let 
		     val (state,j) = kind_var_bind (state,j)
		     val c = rewrite_con state c
		   in mk_con(LAM_c (j,c))
		   end
		 | Lam_c ((a,k),c) =>
		   let 
		     val k = recur_k k
		     val (state,a) = con_var_bind (state,a)
		     val c = rewrite_con state c
		   in mk_con(Lam_c ((a,k),c))
		   end
		 | Pair_c (c1,c2) => mk_con(Pair_c (recur c1,recur c2))
		 | Star_c => con
		 | Inj_c (i,k,c) => mk_con(Inj_c (i,recur_k k,recur c))
		 | Fold_c (k,c) => mk_con(Fold_c (recur_k k,recur c))
		 | Ptr_c c => mk_con(Ptr_c (recur c))
		   
	      end
	  in call_client_code conhandler docon state con
	  end
	

	and rewrite_kind (state : 'state) (kind : kind) : kind = 
	  let 
	    fun dokind (state,kind : kind) = 
	      let
		val recur = rewrite_kind state
	      in
		mk_kind 
		(case kout kind 
		   of T s => T s      
		    | Tmem => Tmem
		    | Unit_k => Unit_k
		    | Nat_k => Nat_k
		    | Arrow_k (k1,k2) => Arrow_k (recur k1,recur k2)
		    | Prod_k (k1,k2)  => Prod_k (recur k1,recur k2)
		    | Sum_k ks   => Sum_k (map recur ks)
		    | Var_k j => Var_k j
		    | Mu_k (j,k) => 
		     let
		       val (state,j) = kind_var_bind (state,j)
		       val k = rewrite_kind state k
		     in Mu_k (j,k)
		     end
		    | All_k (j,k) =>
		     let
		       val (state,j) = kind_var_bind (state,j)
		       val k = rewrite_kind state k
		     in All_k (j,k)
		     end)
	      end
	  in call_client_code kindhandler dokind state kind
	  end
	
	and rewrite_exp (state : 'state) (exp : exp) : exp =
	  let 
	    fun doexp (state,e : exp) = 
	      let
	      in
		mk_exp
		(case #e e of
		   Val32_e sv32 => Val32_e (rewrite_sv32 state sv32)
		 | Let_e (bnds,e) => 
		     let
		       val (state,bnds) = rewrite_bnds state bnds
		       val e = rewrite_exp state e
		     in Let_e (bnds,e)
		     end)
	      end
	    
	  in call_client_code exphandler doexp state exp
	  end
	and rewrite_sv32 (state : 'state) (sv32 : sv32) : sv32 = 
	  let 
	    fun dosv32 (state,sv) = 
	      let
	      in
		case sv32 
		  of Var_32 v => Var_32 v
		   | Label l => Label l
		   | Coercion (c,args) => Coercion (c,map (rewrite_con state) args)
		   | Coerce (q,sv) => Coerce (rewrite_sv32 state q,rewrite_sv32 state sv)
		   | Tabs ((a,k),sv) => 
		    let 
		      val k = rewrite_kind state k
		      val (state,a) = con_var_bind (state,a)
		      val sv = rewrite_sv32 state sv
		    in Tabs ((a,k),sv)
		    end
		   | TApp (sv,c) => 
		    let 
		      val sv = rewrite_sv32 state sv
		      val c  = rewrite_con state c
		    in TApp (sv,c)
		    end
		   | Tag i => Tag i
		   | Unit => Unit
		   | Const_32 v => 
		    Const_32
		    (case v of
		       (Prim.int _) => v
		     | (Prim.uint _) => v
		     | (Prim.float _) => v
		     | (Prim.array (c,array)) =>
			 let
			   val _ = Array.modify (rewrite_primarg state) array
			   val c = rewrite_con state c
			 in Prim.array(c,array)
			 end
		      | (Prim.vector (c,array)) =>
			 let
			   val _ = Array.modify (rewrite_primarg state) array
			   val c = rewrite_con state c
			 in Prim.vector(c,array)
			 end
		      | Prim.refcell (r as (ref e)) => 
			 (r := (rewrite_primarg state e); v)
		      | Prim.tag (t,c) => 
			 Prim.tag(t,rewrite_con state c))
	      end
	    
	  in call_client_code sv32handler dosv32 state sv32
	  end
	and rewrite_bnds (state : 'state) (bnds : bnd list) : ('state * (bnd list)) = 
	  let
	    fun folder (bnd,state) = flip (rewrite_bnd state bnd)
	    val (bndslist,state) = foldl_acc folder state bnds
	    val bnds = List.concat bndslist 
	  in (state,bnds)
	  end
	and rewrite_bnd (state : 'state) (bnd : bnd) : ('state * (bnd list)) = 
	  let
	    
	    
	    fun do_function state (Function {tFormals    : (var * kind) list,
					     eFormals    : (var * con) list,
					     fFormals    : (var * con) list,
					     rtype       : con,
					     body        : exp}) = 
	      let
		fun bind_list binder rewriter state list = 
		  let 
		    fun folder ((v,class),state) = 
		      let 
			val class = rewriter state class
			val (state,v) = binder (state,v)
		      in ((v,class),state)
		      end
		  in foldl_acc folder state list
		  end

		val (tFormals,state) = bind_list con_var_bind rewrite_kind state tFormals
		val (eFormals,state) = bind_list exp_var32_bind rewrite_con state eFormals
		val (fFormals,state) = bind_list exp_var64_bind rewrite_con state fFormals
		val rtype = rewrite_con state rtype
		val body = rewrite_exp state body
	      in Function {tFormals = tFormals, eFormals = eFormals, fFormals = fFormals, rtype = rtype, body = body }
	      end

	    fun do_functions (state,vfs) = map_second (do_function state) vfs

	    fun do_bnd recur (state,bnd) = 
	      (case bnd
		 of Fixcode_b (vfs) => 
		   let
		     fun folder ((v,f),state2) = 
		       let 
			 val (state2,v) = exp_var32_bind (state2,v)
		       in ((v,f),state2)
		       end
		     
		     val (vfs,state) = foldl_acc folder state vfs
		     val vfs = if recur then do_functions (state,vfs) else vfs
		   in
		     (state,Fixcode_b (vfs))
		   end
		  | Exp32_b (v,op32) => 
		   let
		     val op32 = if recur then rewrite_op32 state op32 else op32
		     val (state,v) = exp_var32_bind (state,v)
		   in (state,Exp32_b (v,op32))
		   end
		  | Exp64_b (v,op64) => 
		   let
		     val op64 = if recur then rewrite_op64 state op64 else op64
		     val (state,v) = exp_var64_bind (state,v)
		   in (state,Exp64_b (v,op64))
		   end
		  | Unpack_b (a,x,sv32) => 
		   let
		     val sv32 = if recur then rewrite_sv32 state sv32 else sv32
		     val (state,a) = con_var_bind (state,a)
		     val (state,x) = exp_var32_bind (state,x)
		   in (state,Unpack_b (a,x,sv32))
		   end

		  | Split_b (a1,a2,c) => 
		   let
		     val c = if recur then rewrite_con state c else c
		     val (state,a1) = con_var_bind (state,a1)
		     val (state,a2) = con_var_bind (state,a2)
		   in (state,Split_b (a1,a2,c))
		   end
		  | Unfold_b (a,c) => 
		   let
		     val c = if recur then rewrite_con state c else c
		     val (state,a) = con_var_bind (state,a)
		   in (state,Unfold_b (a,c))
		   end
		  | Inj_b (w,a,arg,sv) => 
		   let
		     val arg = if recur then rewrite_con state arg else arg
		     val (state,a) = con_var_bind (state,a)
		     val sv = if recur then rewrite_sv32 state sv else sv
		   in (state,Inj_b (w,a,arg,sv))
		   end)
		 
	    fun do_bnds recur (state,bnds) = 
	      let 
		fun do_bnd' (bnd,state) = flip (do_bnd recur (state,bnd))
	      in flip (foldl_acc do_bnd' state bnds)
	      end
	    
	  in	    
	    case (bndhandler (state,bnd)) 	    
	      of CHANGE_NORECURSE (state,bnds) => do_bnds false (state,bnds)
	       | CHANGE_RECURSE (state,bnds)   => do_bnds true (state,bnds)
	       | NOCHANGE => let val (state,bnd) = do_bnd true (state,bnd) 
			     in (state,[bnd])
			     end
	       | NORECURSE => let val (state,bnd) = do_bnd false (state,bnd) 
			      in (state,[bnd])
			      end
	  end

	and rewrite_op32 (state : 'state) (op32 : op32) : op32 = 
	  let

	    val recur_sv32 = rewrite_sv32 state
	    val recur_sv64 = rewrite_sv64 state
	    val recur_primarg = rewrite_primarg state
	    val recur_k = rewrite_kind state
	    val recur_c = rewrite_con state
	    val recur_e = rewrite_exp state

	    fun do_op32 (state,op32) = 
	      (case op32
		 of Val sv32 => Val (recur_sv32 sv32)
		  | Prim32 (p,cs,primargs) => 
		   Prim32 (p,map recur_c cs, map recur_primarg primargs)
		  | LilPrimOp32 (lp,cs,sv32s,sv64s) => 
		   LilPrimOp32 (lp,
				map recur_c cs, 
				map recur_sv32 sv32s,
				map recur_sv64 sv64s)
		  | ExternApp (sv,sv32s,sv64s) =>  ExternApp (recur_sv32 sv,map recur_sv32 sv32s,map recur_sv64 sv64s)
		  | App (f,sv32s,sv64s) => App (recur_sv32 f,map recur_sv32 sv32s,map recur_sv64 sv64s)
		  | Call (f,sv32s,sv64s) => Call (recur_sv32 f,map recur_sv32 sv32s,map recur_sv64 sv64s)
		  | Switch switch => Switch (rewrite_switch state switch)
		  | Raise (c,sv32) => Raise (recur_c c,recur_sv32 sv32)
		  | Handle (t,e1,(v,e2)) => 
		   let
		     val t = recur_c t
		     val e1 = recur_e e1
		     val (state,v) = exp_var32_bind (state,v)
		     val e2 = rewrite_exp state e2
		   in Handle (t,e1,(v,e2))
		   end)
	  in do_op32 (state,op32)
	  end
	and rewrite_sv64 (state : 'state) (sv64 : sv64) : sv64 = 
	  let
	    fun dosv64 (state,sv64) = sv64
	  in call_client_code sv64handler dosv64 state sv64
	  end
	and rewrite_op64 (state : 'state) (op64 : op64) : op64 = 
	  let
	    val recur_sv32 = rewrite_sv32 state
	    val recur_sv64 = rewrite_sv64 state
	    val recur_primarg = rewrite_primarg state
	    val recur_c = rewrite_con state
	  in
	    case op64
	      of Val_64 sv64 => Val_64 (recur_sv64 sv64)
	       | ExternAppf (sv,sv32s,sv64s) =>  ExternAppf (recur_sv32 sv,map recur_sv32 sv32s,map recur_sv64 sv64s)
	       | Unbox sv32 => Unbox (recur_sv32 sv32)
	       | Prim64 (p,primargs) =>
		Prim64 (p,map recur_primarg primargs)
	  end
	and rewrite_primarg (state : 'state) (primarg : primarg) : primarg = 
	  (case primarg 
	     of arg32 sv32 => arg32 (rewrite_sv32 state sv32)
	      | arg64 sv64 => arg64 (rewrite_sv64 state sv64))

	and rewrite_cc (state : 'state) (cc : conditionCode) : conditionCode = 
	  let
	    val recur_cc = rewrite_cc state
	  in
	    case cc
	      of Exp_cc e => Exp_cc (rewrite_exp state e)
	       | And_cc  (cc1,cc2) => And_cc (recur_cc cc1,recur_cc cc2)
	       | Or_cc  (cc1,cc2) => Or_cc (recur_cc cc1,recur_cc cc2)
	       | Not_cc cc => Not_cc (recur_cc cc)
	  end

	and rewrite_switch (state : 'state) (sw : switch) : switch = 
	  let
	    val recur_sv32 = rewrite_sv32 state 
	    val recur_exp  = rewrite_exp state
	    val recur_con  = rewrite_con state
	    val recur_cc   = rewrite_cc state
	    fun do_switch (state,sw) = 
	      (case sw
		 of Sumcase {arg : sv32,arms :(w32 * var * exp) list, default: exp option, rtype : con} => 
		   let
		     val arg = recur_sv32 arg
		     val default = mapopt recur_exp default
		     val rtype = recur_con rtype
		     fun mapper (w,v,e) = 
		       let 
			 val(state,v) = exp_var32_bind (state,v)
		       in (w,v,rewrite_exp state e)
		       end
		     val arms = map mapper arms
		   in Sumcase {arg = arg,arms = arms,default = default, rtype = rtype}
		   end
		  | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} =>
		   let
		     val arg = recur_sv32 arg
		     val default = recur_exp default
		     val rtype = recur_con rtype
		     fun mapper (sv,(v,t),e) =
		       let 
			 val t = recur_con t
			 val sv = recur_sv32 sv
			 val(state,v) = exp_var32_bind (state,v)
		       in (sv,(v,t),rewrite_exp state e)
		       end
		     val arms = map mapper arms
		   in Dyncase {arg = arg,arms = arms,default = default, rtype = rtype}
		   end

	      | Intcase {arg : sv32,arms :(w32 * exp) list, default: exp,rtype : con} =>
		   let
		     val arg = recur_sv32 arg
		     val default = recur_exp default
		     val rtype = recur_con rtype
		     val arms = map_second recur_exp arms
		   in Intcase {arg = arg,arms = arms,default = default, rtype = rtype}
		   end
	      | Ifthenelse {arg,thenArm : exp, elseArm : exp, rtype : con} =>
		   let
		     val arg = recur_cc arg
		     val thenArm = recur_exp thenArm
		     val elseArm = recur_exp elseArm
		     val rtype = recur_con rtype
		   in Ifthenelse {arg = arg, thenArm = thenArm, elseArm = elseArm, rtype = rtype}
		   end)
	  in do_switch (state,sw)
	  end

      in
	{
	 rewrite_kind  = rewrite_kind,
	 rewrite_con   = rewrite_con,
	 rewrite_exp   = rewrite_exp,
	 rewrite_bnd   = rewrite_bnd,
	 rewrite_sv32  = rewrite_sv32,
	 rewrite_sv64  = rewrite_sv64,
	 rewrite_op32  = rewrite_op32,
	 rewrite_op64  = rewrite_op64
	 }
      end

      fun null_binder (state,_) = (state,NONE)

      fun null_handler _ = NOCHANGE

      fun null_label_binder (state,_,_) = state

      val default_handler =  
	HANDLER {
		 bndhandler     = null_handler,
		 conhandler     = null_handler,
		 exphandler     = null_handler,
		 sv32handler    = null_handler,
		 sv64handler    = null_handler,
		 kindhandler    = null_handler,
		 kind_var_bind  = null_binder,
		 con_var_bind   = null_binder,
		 exp_var32_bind = null_binder,
		 exp_var64_bind = null_binder
		 }


  end
