structure SingletonElim :> SINGLETONELIM =
  struct
    open Nil

    val error = fn s => Util.error "singleton_elim" s
    val foldl_acc = Listops.foldl_acc
    val map_second = Listops.map_second
    val unzip = Listops.unzip
    val unzip3 = Listops.unzip3

    val rename_arrow = NilUtil.rename_arrow
    datatype env = Env of {ctxt:NilContext.context}

    fun insert_kind (Env {ctxt}) (v,k)     = Env {ctxt=NilContext.insert_kind (ctxt,v,k)}
    fun insert_equation (Env {ctxt}) (v,c) = Env {ctxt=NilContext.insert_equation (ctxt,v,c)}
    fun insert_cbnd (Env {ctxt}) cb        = Env {ctxt=NilContext.insert_cbnd (ctxt,cb)}
    fun kind_of (Env {ctxt}) c             = NilContext.kind_of (ctxt,c)
    fun strip_arrow (Env {ctxt}) c         = Normalize.strip_arrow_norm ctxt c
    fun new_env()                          = Env {ctxt=NilContext.empty()}


    fun erasek env k =
      case k
	of Type_k => Type_k
	 | SingleType_k c => Type_k
	 | Single_k c => erasek env (kind_of env c)
	 | Record_k lvks =>
	  let
	    val (lvks,_) =
	      foldl_acc (fn (((l,v),k),env) => (((l,v),erasek env k),insert_kind env (v,k))) env lvks
	  in Record_k lvks
	  end
	 | Arrow_k (os,vks,k) =>
	  let
	    val (vks,env) =
	      foldl_acc (fn ((v,k),env) => ((v,erasek env k),insert_kind env (v,k))) env vks
	  in Arrow_k  (os,vks,erasek env k)
	  end

    (*PRE: con has been de-singletonized *)
    fun R_k env (arg : con * kind) : con option  =
      let
	val changed = ref false
	fun trans env (c,k) =
	  (case k
	     of Type_k => c
	      | SingleType_k c' => (changed := true; R_c env c')
	      | Single_k c' => (changed := true; R_c env c')
	      | Record_k lvks =>
	       let
		 fun folder (((l,v),k),env) =
		   let
		     val c = trans env (Proj_c(c,l),k)
		     val k = erasek env k
		     val bnd = Con_cb(v,c)
		     val field = (l,Var_c v)
		     val env = insert_kind env (v,k)
		   in ((bnd,field),env)
		   end
		 val (cbsfields,_) = foldl_acc folder env lvks
		 val (cbs,fields) = unzip cbsfields
	       in Let_c (Sequential,cbs,Crecord_c fields)
	       end
	      | Arrow_k (os,vks,k) =>
	       let
		 fun folder ((v,k),env) =
		   let
		     val newv = Name.derived_var v
		     val newc = trans env (Var_c newv,k)
		     val k = erasek env k
		     val arg = Var_c v
		     val bnd = Con_cb(v,newc)
		     val vk = (newv,k)
		     val env = insert_kind env (v,k)
		   in ((vk,bnd,arg),env)
		   end
		 val (vbas,env) = foldl_acc folder env vks
		 val (vks,bnds,args) = unzip3 vbas
		 val body = trans env (App_c(c,args),k)
		 val k = erasek env k

		 val name = Name.fresh_named_var "erasure_fun"
		 val newbody = Let_c (Sequential,bnds,body)
		 val lam = Open_cb (name,vks,newbody)

	       in Let_c (Sequential,[lam],Var_c name)
	       end)
	val res = trans env arg
      in
	if !changed then SOME res
	else NONE
      end

    and R_clist  (env : env) (cs : con list)     : con list = map (R_c env) cs
    and R_vclist (env : env) (cs : (var*con) list) : (var * con) list = map_second (R_c env) cs
    and R_c (env : env) (c : con) : con  =
      let
	val res =
	  case c of
	    Var_c _ => c
	  | Prim_c (pc,cons) => Prim_c(pc,R_clist env cons)
	  | Mu_c (flag,vc_seq) =>
	      let
		val env = foldl (fn ((v,_),env) => insert_kind env (v,Type_k)) env vc_seq
		val vcs = R_vclist env vc_seq
	      in Mu_c(flag,vcs)
	      end
	  | ExternArrow_c (clist,c) => ExternArrow_c (R_clist env clist,R_c env c)
	  | AllArrow_c {openness,effect,tFormals,eFormals,fFormals,body_type} =>
	      let
		val (vks,vcs,env) = R_vklist env tFormals
		val subst = NilSubst.C.seqFromList vcs
		val eFormals = R_clist env eFormals
		val eFormals = map (fn c => NilSubst.substConInCon subst c) eFormals
		val body_type = R_c env body_type
		val body_type = Let_c (Sequential,map Con_cb vcs,body_type)
	      in
		AllArrow_c {openness=openness,effect=effect,
			    tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
			    body_type=body_type}
	      end
	  | Let_c (letsort,cbnds,c) =>
	      let val (cbnds,env) = R_cbnds env cbnds
	      in Let_c(letsort,cbnds,R_c env c)
	      end
	  | Crecord_c lc_list => Crecord_c (map_second (R_c env) lc_list)
	  | Proj_c (c,l) => Proj_c (R_c env c,l)
	  | Closure_c (c1,c2) => Closure_c (R_c env c1,R_c env c2)
	  | App_c (c,clist) => App_c(R_c env c,R_clist env clist)
	  | Coercion_c {vars,from,to} =>
	      let val env = foldl (fn (v,env) => insert_kind env (v,Type_k)) env vars
	      in Coercion_c{vars=vars,from=R_c env from,to=R_c env to}
	      end
      in res
      end
    and R_vklist env vks =
      let
	fun folder ((v,k),env) =
	  let
	    val newv = Name.derived_var v
	    val (newc,k) =
	      case R_k env (Var_c newv,k)
		of SOME newc => (newc,erasek env k)
		 | NONE => (Var_c newv,k)
	    val env = insert_kind env (v,k)
	  in (((newv,k),(v,newc)),env)
	  end
	val (vkvcs,env) = foldl_acc folder env vks
	val (vks,vcs) = unzip vkvcs
      in (vks,vcs,env)
      end

    and R_cbnds env cbnds =  foldl_acc (fn (cb,env) => R_cbnd env cb) env cbnds
    and R_cbnd env cbnd =
      let
	fun R_confun wrapper (v,vklist,c) =
	  let
	    val (vks,vcs,env') = R_vklist env vklist
	    val c = R_c env' c
	    val c = Let_c(Sequential,map Con_cb vcs,c)
	    val cb = wrapper(v,vks,c)
	    val env = insert_cbnd env cb
	  in (cb,env)
	  end
      in (case cbnd
	    of Con_cb (v,c) =>
	      let val c = R_c env c
	      in (Con_cb(v,R_c env c),insert_equation env (v,c))
	      end
	     | Open_cb arg => R_confun Open_cb arg
	     | Code_cb arg => R_confun Code_cb arg)
      end
   and R_elist env elist = map (R_e env) elist
   and R_eopt env eopt = Option.map (R_e env) eopt
   and R_e env e =
     let
       val res =
	 case e of
	   Var_e v => e
	 | Const_e value =>
	     Const_e
	     (
	      case value of
		(Prim.array (c,array)) =>
		  let
		    val _ = Array.modify (R_e env) array
		    val c = R_c env c
		  in Prim.array (c,array)
		  end
	      | (Prim.vector (c,array)) =>
		  let
		    val _ = Array.modify (R_e env) array
		    val c = R_c env c
		  in Prim.vector (c,array)
		  end
	      | Prim.tag (t,c) => Prim.tag (t,R_c env c)
	      | _ => value)
	 | Let_e (sort,bnds,e) =>
	     let
	       val (bnds,env) = R_bnds env bnds
	     in Let_e(sort,bnds,R_e env e)
	     end
	 | Prim_e (ap,trs,clist,elist) =>
	     Prim_e (ap,trs,R_clist env clist, R_elist env elist)
	 | ExternApp_e (f,elist) => ExternApp_e (R_e env f,R_elist env elist)
	 | App_e (openness,f,clist,elist,flist) =>
	     App_e (openness,R_e env f,R_clist env clist,R_elist env elist,R_elist env flist)
	 | Raise_e (e,c) =>  Raise_e(R_e env e,R_c env c)
	 | Switch_e switch => Switch_e (R_switch env switch)
	 | Handle_e {body,bound,handler,result_type} =>
	     Handle_e{body = R_e env body, bound = bound,
		      handler = R_e env handler, result_type = R_c env result_type}
	 | Coerce_e (ccn,cons,exp) => Coerce_e (R_e env ccn,R_clist env cons,R_e env exp)
	 | ForgetKnown_e (sumcon,which) => ForgetKnown_e (R_c env sumcon,which)
	 | Fold_e (vars,from,to) => Fold_e (vars,R_c env from,R_c env to)
	 | Unfold_e (vars,from,to) => Unfold_e (vars,R_c env from,R_c env to)

     in res
     end
   and R_bnds env bnds = foldl_acc (fn (v,env) => R_bnd env v) env bnds
   and R_bnd env bnd =
     let
       val res =
	 (case bnd
	    of Con_b (p,cb) => let val (cb,env)=R_cbnd env cb in (Con_b(p,cb),env)  end
	     | Exp_b (v,niltrace,e) => (Exp_b(v,niltrace,R_e env e),env)
	     | Fixopen_b vcflist => (Fixopen_b (map (R_function env) vcflist),env)
	     | Fixcode_b vcflist => (Fixcode_b (map (R_function env) vcflist),env)
	     | Fixclosure_b (flag,vcl_set) =>
	      let
		fun mapper((v,c),{code,cenv,venv}) =
		  ((v,R_c env c),{code = code,cenv = R_c env cenv, venv = R_e env venv})
	      in (Fixclosure_b (flag, Sequence.map mapper vcl_set),env)
	      end)
     in res
     end
   and R_function env ((v,c),Function{effect,recursive,
				      tFormals,eFormals,fFormals,body}) =
     let
       val arg as {tFormals=vks,...} = rename_arrow (strip_arrow env c,tFormals)
       val c = R_c env c

       val (_,vcs,env) = R_vklist env vks
       val body = R_e env body
       val cbnds = map Con_cb vcs
       val bnds = map (fn cb => Con_b(Runtime,cb)) cbnds
       val body = Let_e(Sequential,bnds,body)

     in ((v,c), Function{effect=effect,recursive=recursive,
			 tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
			 body=body})
     end
   and R_switch env switch =
     (case switch of
	Intsw_e {size,arg,arms,default,result_type} =>
	  Intsw_e {size=size,arg=R_e env arg,arms=map_second (R_e env) arms,default=R_eopt env default,
		   result_type=R_c env result_type}
	| Sumsw_e {sumtype,arg,bound,arms,default,result_type} =>
	  Sumsw_e {sumtype=R_c env sumtype,arg=R_e env arg,
		   bound=bound,arms=map (fn (t,tr,e) => (t,tr,R_e env e)) arms,default=R_eopt env default,
		   result_type=R_c env result_type}
	| Exncase_e {arg,bound,arms,default,result_type} =>
	  Exncase_e {arg=R_e env arg, bound=bound,
		     arms=map (fn (e1,tr,e2) => (R_e env e1,tr,R_e env e2)) arms,
		     default=R_eopt env default,
		     result_type=R_c env result_type}
	| Typecase_e {arg,arms,default,result_type} => error "Typecase_e not done")

   fun R_import (ImportValue(l,v,tr,c),(s,bnds,env)) =
     (ImportValue(l,v,tr,NilSubst.substConInCon s (R_c env c)),(s,bnds,env))
     | R_import (ImportType(l,v,k),(s,bnds,env)) =
     let
       val newv = Name.derived_var v
       val (newc,k) =
	 case R_k env (Var_c newv,k)
	   of SOME newc => (newc,erasek env k)
	    | NONE => (Var_c newv,k)

       val env = insert_kind env (v,k)

     in (ImportType(l,newv,k),(NilSubst.C.addr (s,v,newc),Con_cb(v,newc)::bnds,env))
     end
   fun R_imports imports =
     let
       val (imports,(subst,revbnds,env)) = foldl_acc R_import (NilSubst.C.empty(),[],new_env()) imports
     in (imports,rev revbnds,env)
     end

   fun R_module (MODULE{bnds,imports,exports}) =
     let
       val (imports,cbnds,env) = R_imports imports
       val (bnds,_) = R_bnds env bnds
       val bnds = (map (fn cb => Con_b(Runtime,cb)) cbnds)@bnds
     in MODULE{bnds=bnds,imports=imports,exports=exports}
     end

   val erasek = fn (D : NilContext.context) => fn (k : kind) => erasek (Env {ctxt = D}) k

  end
