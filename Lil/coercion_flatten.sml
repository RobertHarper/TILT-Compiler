structure CFlatten :> CFLATTEN = 
  struct
    open Lil
    fun error s = Util.error "coercion_flatten.sml" s
    fun rewrite_exp  (exp : exp) : exp = 
      P.Lili.to_exp (rewrite_exp' exp)
    and rewrite_exp' (exp : exp) : sv32 P.pexp =
      let 
	fun doexp (e : exp) = 
	  (case #e e 
	     of Val32_e sv32 => rewrite_sv32 sv32
	      | Let_e (bnds,e) => 
	       P.bind (rewrite_bnds bnds)
	       (fn () => rewrite_exp' e))
      in doexp exp
      end
    and rewrite_sv32 (sv32 : sv32) : sv32 P.pexp = 
      let 
	fun dosv32 (sv) = 
	  let
	    val recur_sv32 = rewrite_sv32 
	    val recur_primarg = rewrite_primarg 
	    val recur_e = rewrite_exp 
	      
	  in
	    case sv32 
	      of Coerce (q,sv) => 
		P.bind (recur_sv32 q)
		(fn q => P.bind (recur_sv32 sv) 
		 (fn sv => P.Bind.op32 (P.ret (Val (Coerce (q,sv)))) 
		  (fn x => P.ret (Var_32 x))))
	       | Tabs ((a,k),sv) => 
		P.bind (recur_sv32 sv)
		(fn sv => P.ret (Tabs((a,k),sv)))
	       | TApp (sv,c) => 
		P.bind (recur_sv32 sv)
		(fn sv => P.ret (TApp(sv,c)))
	       | _ => P.ret sv32
	  end
	
      in dosv32 sv32
      end
	 
    and rewrite_bnds (bnds : bnd list) : unit P.pexp = 
      let
	fun do_bnd (bnd,()) = (rewrite_bnd bnd)
      in P.List.foldl_from_list do_bnd () bnds
      end
    and rewrite_bnd (bnd : bnd) : unit P.pexp =
      let

	val res = 
	  case bnd
	    of Fixcode_b args => error "No functions"
	     | Exp32_b args   => rewrite_exp32b args
	     | Exp64_b args   => rewrite_exp64b args
	     | Unpack_b args  => rewrite_unpackb args
	     | Split_b args   => rewrite_splitb args
	     | Inj_b args     => rewrite_injb args
	     | Unfold_b args  => rewrite_unfoldb args
      in	    
	res
      end

    and rewrite_exp32b (v,op32) = P.Bind.op32' v (rewrite_op32 op32) (P.ret ())
    and rewrite_exp64b (v,op64) = P.Bind.op64' v (rewrite_op64 op64) (P.ret ())
    and rewrite_unpackb (a,x,sv32) = P.Bind.unpack' (a,x) (rewrite_sv32 sv32) (P.ret ())
    and rewrite_splitb (a1,a2,c) = P.Bind.split' (a1,a2) (P.ret c) (P.ret ())
    and rewrite_injb (w,b,c,sv) = P.Bind.inj' b (P.bind (rewrite_sv32 sv) (fn sv => P.ret (w,c,sv))) (P.ret ())
    and rewrite_unfoldb (a,c) = P.Bind.unfold' a (P.ret c) (P.ret ())
    and rewrite_op32 (op32 : op32) : op32 P.pexp = 
      let
	
	val recur_sv32 = rewrite_sv32 
	val recur_sv64 = rewrite_sv64 
	val recur_primarg = rewrite_primarg 
	val recur_e = rewrite_exp 
	  
	fun do_op32 op32 = 
	  (case op32
	     of Val sv32 => P.bind (recur_sv32 sv32) (fn sv => P.ret (Val sv))
	      | Prim32 (p,cs,primargs) => 
	       let
		 val ps = P.List.map_from_list recur_primarg primargs
	       in P.bind ps (fn primargs => P.ret  (Prim32 (p,cs,primargs)))
	       end
	      | PrimEmbed (sz,p,primargs) => 
	       let
		 val ps = P.List.map_from_list recur_primarg primargs
	       in P.bind ps (fn primargs => P.ret  (PrimEmbed (sz,p,primargs)))
	       end
	      | LilPrimOp32 (lp,cs,sv32s,sv64s) => 
	       let
		 val ps = P.List.map_from_list recur_sv32 sv32s
	       in
		 P.bind ps (fn sv32s => P.ret  (LilPrimOp32 (lp,cs,sv32s,sv64s)))
	       end
	      | ExternApp (sv,args) =>
	       P.bind (recur_sv32 sv)
	       (fn sv => P.bind (P.List.map_from_list recur_primarg args)
		(fn args => P.ret (ExternApp (sv,args))))
	      | Call (sv,sv32s,sv64s) =>  
	       P.bind (recur_sv32 sv)
	       (fn sv => P.bind (P.List.map_from_list recur_sv32 sv32s)
		(fn sv32s => P.ret (Call (sv,sv32s,sv64s))))
	      | App (f,sv32s,sv64s) => error "No apps"
	      | Switch switch => 
	       P.bind (rewrite_switch switch)
	       (fn switch => P.ret(Switch switch))
	      | Raise (c,sv32) => P.bind (recur_sv32 sv32) (fn sv32 => P.ret(Raise (c,sv32)))
	      | Handle {t,e,h = {b,he}} => 
	       let
		 val e = rewrite_exp e
		 val he = rewrite_exp he

		 val handler = Handle {t = t,e = e,h = {b = b, he = he}}

	       in P.ret handler
	       end)
      in do_op32 op32
      end
    and rewrite_sv64 (sv64 : sv64) : sv64 = sv64
    and rewrite_op64 (op64 : op64) : op64 P.pexp = 
      let
	val recur_sv32 = rewrite_sv32 
	val recur_sv64 = rewrite_sv64 
	val recur_primarg = rewrite_primarg 
      in
	case op64
	  of Val_64 sv64 => P.ret (Val_64 sv64)
	   | Unbox sv32 => P.bind (recur_sv32 sv32) (fn sv32 => P.ret (Unbox sv32))
	   | ExternAppf (sv,args) =>  
	    P.bind (recur_sv32 sv)
	    (fn sv => P.bind (P.List.map_from_list recur_primarg args)
	     (fn args => P.ret (ExternAppf (sv,args))))
	   | Prim64 (p,primargs) =>
	    P.bind (P.List.map_from_list recur_primarg primargs)
	    (fn primargs => P.ret (Prim64 (p,primargs)))
      end
    and rewrite_primarg (primarg : primarg) : primarg P.pexp = 
      (case primarg 
	 of arg32 sv32 => P.bind (rewrite_sv32 sv32) (fn sv32 => P.ret (arg32 sv32))
	  | arg64 sv64 => P.ret primarg
	  | slice (sz,sv32) => P.bind (rewrite_sv32 sv32) (fn sv32 => P.ret (slice(sz, sv32))))
	 
    and rewrite_switch (sw : switch) : switch P.pexp = 
      let
	val recur_sv32 = rewrite_sv32 
	val recur_exp  = rewrite_exp
	val recur_cc   = rewrite_cc
	fun do_switch sw = 
	  (case sw
	     of Sumcase {arg : sv32,arms :(w32 * var * exp) list, default: exp option, rtype : con} => 
	       P.bind (recur_sv32 arg)
	       (fn arg => 
		let
		  val default = Util.mapopt recur_exp default
		  fun mapper (w,v,e) = (w,v,rewrite_exp e)
		  val arms = map mapper arms
		in P.ret (Sumcase {arg = arg,arms = arms,default = default, rtype = rtype})
		end)
	      | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} =>
	       P.bind (recur_sv32 arg)
	       (fn arg => 
		let
		  val default = recur_exp default
		  fun mapper (sv,(v,t),e) =
		    P.bind (recur_sv32 sv)
		    (fn sv => P.ret (sv,(v,t),rewrite_exp e))
		in 
		  P.bind (P.List.map_from_list mapper arms)
		  (fn arms => P.ret (Dyncase {arg = arg,arms = arms,default = default, rtype = rtype}))
		end)
	     
	      | Intcase {arg : sv32,arms :(w32 * exp) list, size : size, default: exp,rtype : con} =>
	       P.bind (recur_sv32 arg)
	       (fn arg => 
		let
		  val default = recur_exp default
		  val arms = Listops.map_second recur_exp arms
		in P.ret (Intcase {arg = arg,arms = arms, size = size, default = default, rtype = rtype})
		end)
	      | Ifthenelse {arg : conditionCode,thenArm : exp, elseArm : exp, rtype : con} =>
	       let
		 val arg = recur_cc arg
		 val thenArm = recur_exp thenArm
		 val elseArm = recur_exp elseArm
	       in P.ret (Ifthenelse {arg = arg,thenArm = thenArm, elseArm = elseArm, rtype = rtype})
	       end)
      in do_switch sw
      end
    and rewrite_cc cc = 
      let
	val recur_e = rewrite_exp 
	val recur_cc = rewrite_cc 
      in
	(case cc
	   of Exp_cc e => Exp_cc (recur_e e)
	    | Not_cc cc => Not_cc (recur_cc cc)
	    | And_cc(cc1,cc2) => And_cc(recur_cc cc1,recur_cc cc2)
	    | Or_cc (cc1,cc2) => Or_cc(recur_cc cc1,recur_cc cc2))
      end
  end