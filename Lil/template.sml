(*$import Name List Sequence Prim Array TraceInfo Listops Util Lil LILREWRITE *)

(* A simple template for lil passes *)


structure LilPass (*:> LILPASS *)= 
  struct
    open Lil


    val foldl_acc = Listops.foldl_acc
    val foldl_acc2 = Listops.foldl_acc2
    val map_second = Listops.map_second
    val mapopt = Util.mapopt
    fun error s = Util.error "lilrewrite.sml" s
    val lprintl = Util.lprintl

    val getOpt = Option.getOpt

    fun flip (a,b) = (b,a)

    structure Typeof = 
      struct
	fun op32 env oper = error "Unimplemented"
	fun op64 env oper = error "Unimplemented"
      end
      
    type env = unit

    fun exp_var32_bind (env,(var,con)) = ()
    fun exp_var64_bind (env,(var,con)) = ()
    fun con_var_bind (env,(var,kind)) = ()
    fun kind_var_bind (env,var) = ()

    fun rewrite_con (env : env) (con : con) : con =  
      let 
	fun docon (env,con : con) = 
	  let 
	    val recur = rewrite_con env
	    val recur_k = rewrite_kind env
	  in
	    mk_con 
	    (case #c con of
	       Var_c v => Var_c v
	     | Nat_c w => Nat_c w
	     | App_c (c1,c2) => App_c (recur c1,recur c2)
	     | APP_c (c,k) => APP_c (recur c,recur_k k)
	     | Pi1_c c => Pi1_c (recur c)
	     | Pi2_c c => Pi2_c (recur c)
	     | Prim_c p => Prim_c p
	     | Pr_c _ => error "Pr_c unimplemented"
	     | Case_c (k,c,((a1,k1),c1),((a2,k2),c2)) => 
		 let
		   val k = recur_k k
		   val k1 = recur_k k1
		   val k2 = recur_k k2
		   val c = recur c
		   val env1 = con_var_bind (env,(a1,k1))
		   val env2 = con_var_bind (env,(a2,k2))
		   val c1 = rewrite_con env1 c1
		   val c2 = rewrite_con env2 c2
		 in Case_c (k,c,((a1,k1),c1),((a2,k2),c2))
		 end
	     | LAM_c (j,c) =>
		 let 
		   val env = kind_var_bind (env,j)
		   val c = rewrite_con env c
		 in LAM_c (j,c)
		 end
	     | Lam_c ((a,k),c) =>
		 let 
		   val k = recur_k k
		   val env = con_var_bind (env,(a,k))
		   val c = rewrite_con env c
		 in Lam_c ((a,k),c)
		 end
	     | Pair_c (c1,c2) => Pair_c (recur c1,recur c2)
	     | Inr_c (k,c) => Inr_c (recur_k k,recur c)
	     | Inl_c (k,c) => Inl_c (recur_k k,recur c)
	     | Fold_c (k,c) => Fold_c (recur_k k,recur c))
	       
	  end
      in  docon (env,con)
      end
    

    and rewrite_kind (env : env) (kind : kind) : kind = 
      let 
	fun dokind (env,kind : kind) = 
	  let
	    val recur = rewrite_kind env
	  in
	    mk_kind 
	    (case #k kind 
	       of T s => T s      
		| TD => TD
		| TU => TU
		| Unit_k => Unit_k
		| Nat_k => Nat_k
		| Arrow_k (k1,k2) => Arrow_k (recur k1,recur k2)
		| Prod_k (k1,k2)  => Prod_k (recur k1,recur k2)
		| Sum_k (k1,k2)   => Sum_k (recur k1,recur k2)
		| Var_k j => Var_k j
		| Mu_k (j,k) => 
		 let
		   val env = kind_var_bind (env,j)
		   val k = rewrite_kind env k
		 in Mu_k (j,k)
		 end
		| All_k (j,k) =>
		 let
		   val env = kind_var_bind (env,j)
		   val k = rewrite_kind env k
		 in All_k (j,k)
		 end)
	  end
      in dokind (env,kind)
      end
	
    and rewrite_exp (env : env) (exp : exp) : exp =
      let 
	fun doexp (env,e : exp) = 
	  let
	  in
	    mk_exp
	    (case #e e of
	       Val32_e sv32 => Val32_e (rewrite_sv32 env sv32)
	     | Let_e (bnds,e) => 
		 let
		   val (env,bnds) = rewrite_bnds env bnds
		   val e = rewrite_exp env e
		 in Let_e (bnds,e)
		 end)
	  end
	    
      in doexp (env,exp)
      end
    and rewrite_sv32 (env : env) (sv32 : sv32) : sv32 = 
      let 
	fun dosv32 (env,sv) = 
	  let

	    val recur_sv32 = rewrite_sv32 env
	    val recur_sv64 = rewrite_sv64 env
	    val recur_c = rewrite_con env
	    val recur_e = rewrite_exp env

	  in
	    case sv32 
	      of Var_32 v => Var_32 v
	       | Coercion (c,args) => Coercion (c,map recur_c args)
	       | Coerce (q,sv) => Coerce (recur_sv32 q,recur_sv32 sv)
	       | Tabs ((a,k),sv) => 
		let 
		  val k = rewrite_kind env k
		  val env = con_var_bind (env,(a,k))
		  val sv = rewrite_sv32 env sv
		in Tabs ((a,k),sv)
		end
	       | TApp (sv,c) => 
		let 
		  val sv = recur_sv32 sv
		  val c  = recur_c c
		in TApp (sv,c)
		end
	       | Const_32 v => 
		Const_32
		(case v of
		   (Prim.int _) => v
		 | (Prim.uint _) => v
		 | (Prim.float _) => v
		 | (Prim.array (c,array)) =>
		     let
		       val _ = Array.modify recur_sv32 array
		       val c = recur_c c
		     in Prim.array(c,array)
		     end
		 | (Prim.vector (c,array)) =>
		     let
		       val _ = Array.modify recur_sv32 array
		       val c = recur_c c
		     in Prim.vector(c,array)
		     end
		 | Prim.refcell (r as (ref e)) => 
		     (r := recur_sv32 e; v)
		 | Prim.tag (t,c) => 
		     Prim.tag(t,recur_c c))
	  end
	
      in dosv32 (env,sv32)
      end
    and rewrite_bnds (env : env) (bnds : bnd list) : (env * (bnd list)) = 
      let
	fun folder (bnd,env) = flip (rewrite_bnd env bnd)
	val (bnds,env) = foldl_acc folder env bnds
      in (env,bnds)
      end
    and rewrite_bnd (env : env) (bnd : bnd) : (env * bnd) = 
      let
	
	
	fun do_function env (Function {tFormals    : (var * kind) list,
				       eFormals    : (var * con) list,
				       fFormals    : (var * con) list,
				       rtype       : con,
				       body        : exp}) = 
	  let
	    val tFormals = map_second (rewrite_kind env) tFormals
	    val env = foldl (con_var_bind o flip) env tFormals
	    val eFormals = map_second (rewrite_con env) eFormals
	    val env = foldl (exp_var32_bind o flip) env eFormals
	    val fFormals = map_second (rewrite_con env) fFormals
	    val env = foldl (exp_var64_bind o flip) env fFormals
	    val rtype = rewrite_con env rtype
	    val body = rewrite_exp env body
	  in Function {tFormals = tFormals, eFormals = eFormals, fFormals = fFormals, rtype = rtype, body = body }
	  end
	
	fun do_functions (env,fs) = map (do_function env) fs
	  
	fun do_bnd (env,bnd) = 
	  (case bnd
	     of Fixcode_b (vts,fs) => 
	       let
		 fun folder ((v,c),env2) = 
		   let 
		     val c = rewrite_con env c 
		     val env2 = exp_var32_bind (env2,(v,c))
		   in ((v,c),env2)
		   end
		 
		 val (vts,env) = foldl_acc folder env vts
		 val fs = do_functions (env,fs) 
	       in
		 (env,Fixcode_b (vts,fs))
	       end
	      | Exp32_b (v,op32) => 
	       let
		 val op32 = rewrite_op32 env op32 
		 val t = Typeof.op32 env op32
		 val env = exp_var32_bind (env,(v,t))
	       in (env,Exp32_b (v,op32))
	       end
	      | Exp64_b (v,op64) => 
	       let
		 val op64 = rewrite_op64 env op64 
		 val t = Typeof.op64 env op64
		 val env = exp_var64_bind (env,(v,t))
	       in (env,Exp64_b (v,op64))
	       end
	      | Unpack_b (a,x,sv32) => 
	       let
		 val t = rewrite_con env t
		 val k = rewrite_kind env k
		 val sv32 = rewrite_sv32 env sv32 
		 val env = con_var_bind (env,(a,k))
		 val env = exp_var32_bind (env,(x,t))
	       in (env,Unpack_b ((a,k),(x,t),sv32))
	       end
	      | Split_b ((a1,k1),(a2,k2),c) => 
	       let
		 val c  = rewrite_con env c 
		 val k1 = rewrite_kind env k1
		 val k2 = rewrite_kind env k2
		 val env = con_var_bind (env,(a1,k1))
		 val env = con_var_bind (env,(a2,k2))
	       in (env,Split_b ((a1,k1),(a2,k2),c))
	       end
	      | Unfold_b ((a,k),c) => 
	       let
		 val c = rewrite_con env c 		 
		 val k = rewrite_kind env k
		 val env = con_var_bind (env,(a,k))
	       in (env,Unfold_b ((a,k),c))
	       end)
	     
      in	    
	do_bnd (env,bnd)
      end

    and rewrite_op32 (env : env) (op32 : op32) : op32 = 
      let
	
	val recur_sv32 = rewrite_sv32 env
	val recur_sv64 = rewrite_sv64 env
	val recur_primarg = rewrite_primarg env
	val recur_k = rewrite_kind env
	val recur_c = rewrite_con env
	val recur_e = rewrite_exp env
	  
	fun do_op32 (env,op32) = 
	  (case op32
	     of Val sv32 => Val (recur_sv32 sv32)
	      | Prim32 (p,cs,primargs) => 
	       Prim32 (p,map recur_c cs, map recur_primarg primargs)
	      | LilPrimOp32 (lp,cs,sv32s,sv64s) => 
	       LilPrimOp32 (lp,
			    map recur_c cs, 
			    map recur_sv32 sv32s,
			    map recur_sv64 sv64s)
	      | ExternApp (sv,svs) =>  ExternApp (recur_sv32 sv,map recur_sv32 svs)
	      | App (f,sv32s,sv64s) => App (recur_sv32 f,map recur_sv32 sv32s,map recur_sv64 sv64s)
	      | Switch switch => Switch (rewrite_switch env switch)
	      | Raise (c,sv32) => Raise (recur_c c,recur_sv32 sv32)
	      | Handle (e1,(v,e2)) => 
	       let
		 val e1 = recur_e e1
		 val env = exp_var32_bind (env,(v,Lil.mk_pcon Lil.Dyn_c))
		 val e2 = rewrite_exp env e2
	       in Handle (e1,(v,e2))
	       end
	      | Vcasel (t,(arg,((a1,k1),exp),((a2,k2),sv32))) => 
	       let
		 val t = recur_c t
		 val k1 = recur_k k1
		 val k2 = recur_k k2
		 val arg = recur_c arg
		 val env1 = con_var_bind (env,(a1,k1))
		 val env2 = con_var_bind (env,(a2,k2))
		 val exp  = rewrite_exp  env1 exp
		 val sv32 = rewrite_sv32 env2 sv32
	       in Vcasel (t,(arg,((a1,k1),exp),((a2,k2),sv32))) 
	       end		  
	      | Vcaser (t,(arg,((a1,k1),sv32),((a2,k2),exp))) => 
	       let
		 val t = recur_c t
		 val arg = recur_c arg
		 val k1 = recur_k k1
		 val k2 = recur_k k2
		 val env1 = con_var_bind (env,(a1,k1))
		 val env2 = con_var_bind (env,(a2,k2))
		 val sv32 = rewrite_sv32 env1 sv32
		 val exp  = rewrite_exp  env2 exp
	       in Vcaser (t,(arg,((a1,k1),sv32),((a2,k2),exp)))
	       end)
      in do_op32 (env,op32)
      end
    and rewrite_sv64 (env : env) (sv64 : sv64) : sv64 = 
      let
	fun dosv64 (env,sv64) = 
	  (case sv64
	     of Var_64 x => Var_64 x 
	      | Const_64 v => Const_64 v)
      in dosv64 (env,sv64)
      end
    and rewrite_op64 (env : env) (op64 : op64) : op64 = 
      let
	val recur_sv32 = rewrite_sv32 env
	val recur_sv64 = rewrite_sv64 env
	val recur_primarg = rewrite_primarg env
	val recur_c = rewrite_con env
      in
	case op64
	  of Val_64 sv64 => Val_64 (recur_sv64 sv64)
	   | Unbox sv32 => Unbox (recur_sv32 sv32)
	   | Prim64 (p,cs,primargs) =>
	    Prim64 (p,map recur_c cs,map recur_primarg primargs)
      end
    and rewrite_primarg (env : env) (primarg : primarg) : primarg = 
      (case primarg 
	 of arg32 sv32 => arg32 (rewrite_sv32 env sv32)
	  | arg64 sv64 => arg64 (rewrite_sv64 env sv64))
	 
    and rewrite_switch (env : env) (sw : switch) : switch = 
      let
	val recur_sv32 = rewrite_sv32 env 
	val recur_exp  = rewrite_exp env
	val recur_con  = rewrite_con env
	  
	fun do_switch (env,sw) = 
	  (case sw
	     of Sumcase {arg : sv32,arms :(w32 * (var * con) * exp) list, default: exp option, tipe : con} => 
	       let
		 val arg = recur_sv32 arg
		 val default = mapopt recur_exp default
		 val tipe = recur_con tipe
		 fun mapper (w,(v,t),e) = 
		   let 
		     val t = rewrite_con env t
		     val env = exp_var32_bind (env,(v,t))
		   in (w,(v,t),rewrite_exp env e)
		   end
		 val arms = map mapper arms
	       in Sumcase {arg = arg,arms = arms,default = default, tipe = tipe}
	       end
	      | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        tipe : con} =>
	       let
		 val arg = recur_sv32 arg
		 val default = recur_exp default
		 val tipe = recur_con tipe
		 fun mapper (sv,(v,t),e) =
		   let 
		     val sv = recur_sv32 sv
		     val t = rewrite_con env t
		     val env = exp_var32_bind (env,(v,t))
		   in (sv,(v,t),rewrite_exp env e)
		   end
		 val arms = map mapper arms
	       in Dyncase {arg = arg,arms = arms,default = default, tipe = tipe}
	       end
	     
	      | Intcase {arg : sv32,arms :(w32 * exp) list, default: exp,tipe : con} =>
	       let
		 val arg = recur_sv32 arg
		 val default = recur_exp default
		 val tipe = recur_con tipe
		 val arms = map_second recur_exp arms
	       in Intcase {arg = arg,arms = arms,default = default, tipe = tipe}
	       end)
      in do_switch (env,sw)
      end
    

  end
