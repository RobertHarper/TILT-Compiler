functor ToClosure(structure Nil : NIL
		  structure NilUtil : NILUTIL
		  sharing NilUtil.Nil = Nil)
    : TOCLOSURE =
struct


    open Util NilUtil Name Nil
    structure Nil = Nil

    val error = fn s => error "toclosure.sml" s
    val float64 = Prim_c(Float_c Prim.F64,[])
    structure FidSet = VarSet
    type fid = var


    (* ---------------- Global data structure to keep track of function information ----------------- *)
    local
	val empty_table = VarMap.empty : ({escape : bool,
					   code_var : fid,
					   unpack_var : fid,
					   callee : FidSet.set,
					   frees : {freeevars : con VarMap.map,
						    freecvars : kind VarMap.map}} ref VarMap.map)
	val fids = ref empty_table
    in
	fun map_union(m1,m2) = VarMap.foldli (fn (k,d,m) => VarMap.insert(m,k,d)) m1 m2
	fun map_member(m,v) = (case (VarMap.find(m,v)) of
				   NONE => false
				 | SOME _ => true)
	fun join_free({freeevars=e1,freecvars=c1},{freeevars=e2,freecvars=c2}) = 
	    {freeevars = map_union(e1,e2), freecvars = map_union(c1,c2)}
	val empty_free = {freeevars = VarMap.empty,
			  freecvars = VarMap.empty}

	fun reset_table() = fids := empty_table
	fun get_fids() = VarMap.foldli (fn (fid,_,acc) => VarSet.add(acc,fid)) VarSet.empty (!fids)
	fun add_fid new_fid = fids := (VarMap.insert(!fids,new_fid,
						     ref {escape = false, 
							  callee = FidSet.empty,
							  code_var = fresh_named_var((var2string new_fid)
										     ^ "_code"),
							  unpack_var = fresh_named_var((var2string new_fid)
										       ^ "_unpack"),
							  frees = {freeevars = VarMap.empty,
								   freecvars = VarMap.empty}}))
	fun add_fids new_fids = app add_fid new_fids
	fun add_escape esc_fid = (case (VarMap.find(!fids,esc_fid)) of
				      NONE => error "fid not found for escape"
				    | SOME (r as (ref{escape,code_var,unpack_var,callee,frees})) =>
					  r := {escape = true,
						code_var = code_var,
						unpack_var = unpack_var,
						callee = callee,
						frees = frees})
	fun add_callee (caller,callee_fid) = (case (VarMap.find(!fids,caller)) of
						  NONE => error "fid not found for escape"
						| SOME (r as (ref{escape,unpack_var,code_var,callee,frees})) =>
						      r := {escape = escape,
							    code_var = code_var,
							    unpack_var = unpack_var,
							    callee = FidSet.add(callee,callee_fid),
							    frees = frees})
	fun get_callee caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error "fid not found for escape"
				   | SOME (r as (ref{callee,...})) => callee)
	fun get_frees  caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error "fid not found for escape"
				   | SOME (r as (ref{frees,...})) => frees)
	fun get_codevar caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error "fid not found for escape"
				   | SOME (ref{code_var,...}) => code_var)
	fun get_unpackvar caller = (case (VarMap.find(!fids,caller)) of
					NONE => error "fid not found for escape"
				      | SOME (ref{code_var,...}) => code_var)
	fun add_frees (fid,f) =
	    (case (VarMap.find(!fids,fid)) of
		 NONE => error "fid not found for escape"
	       | SOME (r as (ref{escape,code_var,unpack_var,callee,frees})) =>
		     r := {escape = escape,
			   code_var = code_var,
			   unpack_var = unpack_var,
			   callee = callee,
			   frees = join_free(frees,f)})
		 
    end


    (* ---------------- code to perform the initial free-variable computation ------------- *)
    local
	datatype state = STATE of {curfid : fid,
				   boundevars : con VarMap.map,
				   boundcvars : kind VarMap.map,
				   boundfids : FidSet.set}
	fun add_boundfids(STATE{curfid,boundevars,boundcvars,boundfids},fid_types) = 
	    STATE{curfid = curfid,
		  boundevars = foldl (fn ((v,c),m) => VarMap.insert(m,v,c)) boundevars fid_types,
		  boundcvars = boundcvars,
		  boundfids = VarSet.addList(boundfids,map #1 fid_types)}
	fun add_boundcvars(STATE{curfid,boundevars,boundcvars,boundfids},vk_list) = 
	    STATE{curfid = curfid,
		  boundevars = boundevars,
		  boundcvars = foldl (fn ((v,k),m) => VarMap.insert(m,v,k)) boundcvars vk_list,
		  boundfids = boundfids}
	fun add_boundevar(STATE{curfid,boundevars,boundcvars,boundfids},v,c) = 
	    STATE{curfid = curfid,
		  boundevars = VarMap.insert(boundevars,v,c),
		  boundcvars = boundcvars,
		  boundfids = boundfids}
	fun add_boundcvar(s,v,k) = add_boundcvars(s,[(v,k)])
	    
	type freevars = {freeevars : con VarMap.map,
			 freecvars : kind VarMap.map}
	fun free_cvar (STATE{boundcvars,...},cvar) = 
	    {freeevars = VarMap.empty,
	     freecvars = VarMap.insert(VarMap.empty,cvar,
				       case (VarMap.find(boundcvars,cvar)) of
					   SOME k => k
					 | NONE => error "free_cvar: variable not bound")}
	fun free_evar (STATE{boundevars,...},evar) = 
	    {freeevars = VarMap.insert(VarMap.empty,evar,
				       case (VarMap.find(boundevars,evar)) of
					   SOME c => c
					 | NONE => error "free_evar: variable not bound"),
	     freecvars = VarMap.empty}
	    
    in
	fun initial_state top_fid = STATE{curfid = top_fid,
					  boundcvars = VarMap.empty,
					  boundevars = VarMap.empty,
					  boundfids = VarSet.empty}
	fun e_find_fv (state as STATE({curfid,boundevars,boundcvars,boundfids})) exp : freevars =
	    (case exp of
		 Var_e v => (if VarSet.member(boundfids,v) then add_escape v else ();
				 if map_member(boundevars,v) 
				     then empty_free
				 else free_evar(state,v))
	       | Const_e v => 
		     (case v of
			  ((Prim.int _) | (Prim.uint _) | (Prim.float _)) => empty_free
			| ((Prim.array (c,array)) | (Prim.vector (c,array))) => 
			      let fun folder(e,f) = join_free(f,(e_find_fv state e))
				  val f = Array.foldl folder empty_free array
			      in  join_free(f,(c_find_fv state c))
			      end
			| Prim.refcell (ref e) => (e_find_fv state e)
			| Prim.tag (_,c) => (c_find_fv state c))
	       | Let_e (_,bnds,e) => (* treat as though a sequential let *)
		     let fun folder (bnd,(frees,state)) =
			 (case bnd of
			      Con_b(v,k,c) => let val f = c_find_fv state c
						   in (f, add_boundcvar(state,v,k))
						   end
			    | Exp_b(v,c,e) => let val f = e_find_fv state e
					      in (f, add_boundevar(state,v,c))
					      end
			    | Fixfun_b var_fun_set =>
				  let val local_fids = map #1 (set2list var_fun_set)
				      val _ = add_fids local_fids
				      fun get_fid_type(v,Function(openness,effect,recur,
								  vklist,vclist,vflist,body,c)) = 
					  (v,Arrow_c(Open,(effect,vklist,(map #2 vclist),
							   TilWord32.fromInt(length vflist),c)))
				      val local_fids_types = map get_fid_type (set2list var_fun_set)
				      val boundfids' = VarSet.addList(boundfids,local_fids)
				      fun do_arm(v,Function(_,_,_,vklist,vclist,vflist,body,tipe)) =
				     let 
					 fun vkfolder((v,k),(f,s)) =
					     (join_free(f,k_find_fv s k), add_boundcvar(s,v, k))
					 fun vcfolder((v,c),(f,s)) =
					     (join_free(f,(c_find_fv s c)), add_boundevar(s,v, c))
					 val local_state = STATE{curfid=v,boundfids=boundfids',
								 boundevars = VarMap.empty,
								 boundcvars = VarMap.empty}
					 val fs = (empty_free, local_state)
					 val fs = foldl vkfolder fs vklist
					 val (f,s) = (foldl vcfolder fs 
						      (vclist @ (map (fn v => (v,float64)) vflist)))
					 val f = join_free(f,e_find_fv s body)
					 val f = join_free(f,c_find_fv s tipe)
					 val _ = add_frees(v,f)
				     in  f
				     end
				      val free = (foldl (fn (a,f) => join_free(f,do_arm a))
						  empty_free (set2list var_fun_set))
				  in  (free, add_boundfids(state, local_fids_types))
				  end
			    | Fixclosure_b _ => error "there can't be closures while closure-converting")
			 val (funfree,state') = foldl folder (empty_free,state) bnds
			 val bodyfree = e_find_fv state' e
		     in  join_free(funfree,bodyfree)
		     end
	       | Prim_e (ap,clist,elist) =>
		     let fun cfold(c,f) = join_free(f,c_find_fv state c)
			 fun efold(e,f) = join_free(f,e_find_fv state e)
			 val free = foldl cfold empty_free clist
			 val free = foldl efold free elist
		     in  free
		     end
	       | Switch_e switch => 
		     let
			 fun do_sw {info, arg, arms, default} do_info do_arg do_prearm =
			     let val f1 = do_info info
				 val f2 = do_arg arg
				 fun do_arm(pre,Function(_,_,_,vklist,vclist,vflist,body,tipe)) =
				     let 
					 fun vkfolder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
								      add_boundcvar(s,v,k))
					 fun vcfolder((v,c),(f,s)) =
					     let val f' = c_find_fv s c
					     in (join_free(f,f'), add_boundevar(s,v,c))
					     end
					 val (f,s) = (do_prearm pre, state)
					 val (f,s) = foldl vkfolder (f,s) vklist
					 val (f,s) = foldl vcfolder (f,s) 
					     (vclist @ (map (fn v => (v,float64)) vflist))
					 val f = join_free(f,e_find_fv s body)
					 val f = join_free(f,c_find_fv s tipe)
				     in  f
				     end
				 val f3 = map do_arm arms
				 val f4 = (case default of
					       NONE => empty_free
					     | SOME e => e_find_fv state e)
			     in foldl join_free f4 (f1::f2::f3)
			     end
			 fun nothing _ = empty_free
		     in	 case switch of
			 Intsw_e sw => do_sw sw nothing (e_find_fv state) nothing
		       | Sumsw_e sw => (do_sw sw (fn (tagcount,clist) =>
						  foldl (fn (c,f) => join_free(f,c_find_fv state c)) 
						  empty_free clist)
					(e_find_fv state) nothing)
		       | Exncase_e sw => do_sw sw nothing (e_find_fv state) (e_find_fv state)
		       | Typecase_e sw => do_sw sw nothing (c_find_fv state) nothing
		     end
	       | App_e (e, clist, elist, eflist) =>
		     let val free1 = (case e of
					  Var_e v => if (VarSet.member(boundfids,v))
							 then (add_callee(curfid,v);
							       empty_free)
						     else (e_find_fv state e)
					| _ => (e_find_fv state e))
			 val free2 = map (c_find_fv state) clist
			 val free3 = map (e_find_fv state) elist
			 val free4 = map (e_find_fv state) eflist
			 val joiner = foldl join_free
		     in joiner (joiner (joiner free1 free2) free3) free4
		     end
	       | Call_e _ => error "there can't be direct calls while closure-converting"
	       | Raise_e (e,c) => join_free(e_find_fv state e, c_find_fv state c)
	       | Handle_e (e,Function(_,_,_,[],[(v,c)],_,body,tipe)) =>
		     let val f = e_find_fv state e
		     in  (foldl join_free empty_free
			  [f, (c_find_fv state c),
			   (e_find_fv (add_boundevar(state,v,c)) body),
			   (c_find_fv state tipe)])
		     end
	       | Handle_e _ => error "ill-typed Handle_e")

		 
	and c_find_fv (state as STATE{curfid,boundcvars,boundevars,boundfids}) con : freevars =
	    (case con of
		 Prim_c (pc,clist) => foldl (fn (c,f)=> join_free(f,(c_find_fv state c))) empty_free clist
	       | Mu_c (vcset,v) =>
		     let val vclist = set2list vcset
			 val state' = add_boundcvars(state,map (fn (v,_) => (v,Word_k Runtime)) vclist)
		     in  foldl (fn ((v,c),f) => join_free(f,c_find_fv state' c))
			 empty_free (set2list vcset)
		     end
	       | Arrow_c(_,confun as (_,vklist,clist,numfloat,c)) => 
		     let fun vkfolder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
						      add_boundcvar(s,v,k))
			 fun cfolder (c,(f,s)) = (join_free(f, (c_find_fv state c)),s)
			 val (f,s) = foldl vkfolder (empty_free,state) vklist
			 val (f,s) = foldl cfolder (f,s) clist
		     in  join_free(f, c_find_fv s c)
		     end
	       | Code_c _ => error "can't have code_c during closure-conversion"
	       | Var_c v => if (map_member(boundcvars,v)) then empty_free else free_cvar(state,v)
	       | Typecase_c {arg, arms, default} => 
		     let val f = c_find_fv state arg
			 val f = (case default of
				      NONE => f
				    | SOME c => join_free(f,c_find_fv state c))
			 fun armfolder ((pc,vklist,c,k),f) = 
			     let fun folder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
							    add_boundcvar(s,v,k))
				 val (f,_) = foldl folder (f,state) vklist
			     in join_free(f,c_find_fv state c)
			     end
		     in  foldl armfolder f arms
		     end
	       | Let_c(_,cbnds,c) => 
		     let 
			 fun cb_folder (Con_cb(v,k,c),(f,s)) = 
			     let val f' = c_find_fv state c
			     in (join_free(f, f'), add_boundcvar(s,v,k))
			     end
			   | cb_folder ((Fun_cb(v,_,vklist,c,k)),(f,s)) = 
			     let fun folder((v,k),(f,s)) = (join_free(f,k_find_fv s k),
							    add_boundcvar(s,v,k))
				 val (f,s) = foldl folder (empty_free,state) vklist
				 val f' = c_find_fv s c
			     in  (join_free(f, f'), add_boundcvar(s,v,k))
			     end
			 val (f,s) = foldl cb_folder (empty_free,state) cbnds
			 val f' = c_find_fv s c
		     in  join_free(f, f')
		     end
	       | Crecord_c lclist => let val flist = map (fn (_,c) => c_find_fv state c) lclist
				     in foldl join_free empty_free flist
				     end
	       | Proj_c (c,l) => c_find_fv state c
	       | Closure_c _ => error "can't have closure_c during closure-conversion"
	       | App_c(c,clist) => let val f = c_find_fv state c
				   in foldl (fn (c,f) => join_free(f,c_find_fv state c))
				       f clist
				   end
	       | Annotate_c (_,c) => c_find_fv state c)
		 
	and k_find_fv state kind : freevars =
	    (case kind of
	    ((Type_k _) | (Word_k _)) => empty_free
	  | (Singleton_k (p,k,c)) => (k_find_fv state k; (c_find_fv state c))
	  | (Record_k lv_kind_seq) =>
		 let fun folder (((l,v),k),(f,s)) = (join_free(f,k_find_fv s k), add_boundcvar(s,v,k))
		 in  #1(foldsequence folder (empty_free,state) lv_kind_seq)
		 end
	  | (Arrow_k (_,vklist,k)) =>
		let fun folder ((v,k),s) = (k_find_fv s k; add_boundcvar(s,v,k))
		    val state' = foldl folder state vklist
		in  k_find_fv state' k
		end
	  | (Code_k _) => error "can't have code_k during closure-conversion") 
		 
    end


   (* ------- compute the final free-variable list by closing over the callgraph ------ *)
   local
       fun close_fun (curfid,nextset) = 
	   let val fvs = get_frees curfid
	       val callees = get_callee curfid
	       val callee_fvs = VarSet.foldl (fn (f,fv) => join_free(get_frees f, fv)) empty_free callees
	       fun map_isSubset(m1,m2) = VarMap.foldli (fn (k,_,f) => f andalso map_member(m2,k)) true m1
	       fun subset_free({freeevars=e1,freecvars=c1},{freeevars=e2,freecvars=c2}) = 
		   map_isSubset(e1,e2) andalso map_isSubset(c1,c2)
	   in if (subset_free(callee_fvs,fvs))
		  then nextset
	      else (add_frees(curfid,callee_fvs);
		    VarSet.add(nextset,curfid))
	   end
   in  fun close_funs workset = 
       if (VarSet.isEmpty workset)
	   then ()
       else close_funs (VarSet.foldl close_fun VarSet.empty workset)
   end


   (* ------- rewrite the expression to its closed-form ----------- *)
   fun set_remove bf v = if (VarSet.member(bf,v)) then VarSet.delete(bf,v) else bf
   fun e_rewrite boundfids arg_exp =
       (case arg_exp of
	    Var_e v => arg_exp (* closures retain their name *)
	  | Const_e v => Const_e 
		(case v of
		     ((Prim.int _) | (Prim.uint _) | (Prim.float _)) => v
		   | (Prim.array (c,array)) =>
			 (Array.modify (e_rewrite boundfids) array;
			  Prim.array(c_rewrite c,array))
		   | (Prim.vector (c,array)) =>
			 (Array.modify (e_rewrite boundfids) array;
			  Prim.vector(c_rewrite c,array))
		   | Prim.refcell (r as (ref e)) => (r := e_rewrite boundfids e; v)
		   | Prim.tag (t,c) => Prim.tag(t,c_rewrite c))
	  | Let_e(letsort,bnds,e) => 
		let 
		    fun bnd_rewrite bf (Con_b(v,k,c)) = (set_remove bf v,
							    [Con_b(v,k_rewrite k, c_rewrite c)])
		      | bnd_rewrite bf (Exp_b(v,c,e)) = (set_remove bf v,
							    [Exp_b(v,c_rewrite c, 
								   e_rewrite bf e)])
		      | bnd_rewrite bf (Fixclosure_b _) = error "there can't be closures while closure-converting"
		      | bnd_rewrite bf (Fixfun_b var_fun_set) = 
			let val var_fun_list = set2list var_fun_set
			    val bf = foldl (fn ((v,_),bf) => VarSet.add(bf,v)) bf var_fun_list
			    fun var_fun_doer(v,Function(openness,effect,recur,
							vklist,vclist,vflist,body,tipe)) = 
				let val code_var = get_codevar v
				    val unpack_var = get_unpackvar v
				    val {freeevars,freecvars} = get_frees v
				    val vk_free = (VarMap.listItemsi freecvars)
				    val vc_free = (VarMap.listItemsi freeevars)
				    val vklist' = vklist @ vk_free
				    val vclist' = vclist @ vc_free
				    val tipe' = c_rewrite tipe
				    val code_fun = Function(Closed,effect,recur,
							    vklist',vclist',vflist,
							    e_rewrite bf body, tipe')
				    val (cfv_var,efv_var) = (fresh_named_var "free_cons",
							     fresh_named_var "free_exp")
				    val vklist'' = vklist @ [(cfv_var, kind_tuple(map #2 vk_free))]
				    val vclist'' = vclist @ [(efv_var, con_tuple(map #2 vc_free))]
				    fun cproj (i,_) = Proj_c(Var_c cfv_var, generate_tuple_label(i+1))
				    fun eproj (i,_) = Prim_e(NilPrimOp(select (generate_tuple_label(i+1))),
							     [con_tuple(map #2 vc_free)],
							     [Var_e efv_var])
				    val cprojes = Listops.mapcount cproj vk_free
				    val eprojes = Listops.mapcount eproj vc_free
				    val call_code_body = Call_e(code_var,
								(map (Var_c o #1) vklist) @ cprojes,
								(map (Var_e o #1) vclist) @ eprojes,
								(map Var_e vflist))
				    val unpack_fun = Function(Closed,effect,Nonleaf,
							      vklist'',
							      vclist'',
							      vflist,
							      call_code_body,
							      tipe')
				    val closure = {code = code_var,
						   cenv = con_tuple_inject(map (Var_c o #1) vk_free),
						   venv = exp_tuple(map (Var_e o #1) vc_free)}
				in ([(code_var,code_fun),(unpack_var,unpack_fun)],
				    (v,closure))
				end
			    val fun_close = map var_fun_doer var_fun_list
			    val fun_list = List.concat(map #1 fun_close)
			    val closure_list = map #2 fun_close
			in  (bf,[Fixfun_b (list2set fun_list),
				 Fixclosure_b (list2set closure_list)])
			end
		    val (bf',bnds') = foldl (fn (bnd,(bf,acc_bnds)) => let val (bf,bnds) = bnd_rewrite bf bnd
								       in  (bf,bnds @ acc_bnds)
								       end) (boundfids,[]) bnds
		    val e' = e_rewrite bf' e
		in Let_e(letsort,bnds',e')
		end
	  | Prim_e(p,clist,elist) => Prim_e(p, map c_rewrite clist, 
					    (map (e_rewrite boundfids) elist))
	  | Switch_e switch => 
		let
		    fun do_sw {info, arg, arms, default} do_info do_arg do_prearm =
			let  fun do_arm(prearm,Function(openness,effect,recur,
							vklist,vclist,vflist,body,tipe)) =
				     let 
					 fun vkfolder((v,k),(vklist,bf)) = 
					     ((v,k_rewrite k)::vklist,set_remove bf v)
					 fun vcfolder((v,c),(vclist,bf)) = 
					     ((v,c_rewrite c)::vclist,set_remove bf v)
					 val (vklist',bf') = foldr vkfolder ([],boundfids) vklist
					 val (vclist',bf') = foldr vcfolder ([],bf') vclist
					 val body' = e_rewrite bf' body
					 val tipe' = c_rewrite tipe
				     in  (do_prearm prearm,
					  Function(openness,effect,recur,vklist',vclist',vflist,body',tipe'))
				     end
			in {info = do_info info,
			    arg = do_arg arg,
			    arms = map do_arm arms,
			    default = Util.mapopt (e_rewrite boundfids) default}
			end
		    fun identity x = x
		    val switch' = 
			(case switch of
			     Intsw_e sw => Intsw_e(do_sw sw identity (e_rewrite boundfids) identity)
			   | Sumsw_e sw => Sumsw_e(do_sw sw (fn (tc,clist) => (tc,map c_rewrite clist))
						   (e_rewrite boundfids) identity)
			   | Exncase_e sw => Exncase_e(do_sw sw identity (e_rewrite boundfids) 
						       (e_rewrite boundfids))
			   | Typecase_e sw => Typecase_e(do_sw sw identity c_rewrite identity))
		in  Switch_e switch'
		end
	  | App_e (e, clist, elist, eflist) => 
		let val clist' = map c_rewrite clist
		    val elist' = map (e_rewrite boundfids) elist
		    val eflist' = map (e_rewrite boundfids) eflist
		    fun docall (v,{freeevars, freecvars}) = 
			let val clist'' = map (fn (v,_) => Var_c v) (VarMap.listItemsi freecvars)
			    val elist'' = map (fn (v,_) => Var_e v) (VarMap.listItemsi freeevars)
			in Call_e(v,clist' @ clist'', elist' @ elist'', eflist')
			end
		in case e of
		    Var_e v => if (VarSet.member(boundfids, v)) 
				   then docall(v,get_frees v)
			       else App_e(e_rewrite boundfids e, clist', elist', eflist')
		  | _ => App_e(e_rewrite boundfids e, clist', elist', eflist')
		end
	  | Call_e _ => error "there can't be direct calls while closure-converting"
	  | Raise_e (e,c) => Raise_e(e_rewrite boundfids e, c_rewrite c)
	  | Handle_e (e,Function(openness,effect,recur,[],[(v,c)],_,body,tipe)) =>
		let val bf = if (VarSet.member(boundfids,v)) 
				 then VarSet.delete(boundfids,v) else boundfids
		in Handle_e(e_rewrite boundfids e, 
			    Function(openness,effect,recur,[],[(v,c_rewrite c)],[],
				     e_rewrite bf body,  c_rewrite tipe))
		end
	  | Handle_e _ => error "ill-typed Handle_e")

   and c_rewrite arg_con : con = 
       (case arg_con of
	    Prim_c (primcon,clist) => Prim_c(primcon, map c_rewrite clist)
	  | Mu_c (vc_set,v) =>  let val vc_list = set2list vc_set
				    val vc_list' = map (fn (v,c) => (v, c_rewrite c)) vc_list
				in Mu_c(list2set vc_list', v)
				end
	  | Arrow_c (Open,(effect,vklist,clist,numfloats,c)) => 
		let val vklist' = map (fn(v,k) => (v,k_rewrite k)) vklist
		in Arrow_c(Closed,(effect,vklist',map c_rewrite clist,numfloats,c_rewrite c))
		end
	  | Arrow_c (Closed,confun) => error "cannot encounter Arrow_c(Closed,...) during closure-conversion"
	  | Code_c _ => error "cannot encounter Code_c during closure-conversion"
	  | Var_c v => arg_con
	  | Let_c (letsort,cbnds,c) => 
		let 
		    fun cb_rewrite (Con_cb(v,k,c)) = [Con_cb(v,k_rewrite k, c_rewrite c)]
		      | cb_rewrite (Fun_cb(v,Open,vklist,c,k)) = 
			let val code_var = get_codevar v
			    val unpack_var = get_unpackvar v
			    val {freeevars,freecvars} = get_frees v (* freeevars must be empty *)
			    val vk_free = (VarMap.listItemsi freecvars)
			    val cfv_var = fresh_named_var "free_cons"
			    fun get_cbnd (i,(v,k)) = Con_cb(v,k, Proj_c(Var_c cfv_var,
									generate_tuple_label(i+1)))
			    val cbnds = Listops.mapcount get_cbnd vk_free
			    val vklist' = vklist @ [(cfv_var, kind_tuple(map #2 vk_free))]
			    val code_cb = Fun_cb(code_var,Closed, vklist',
						 Let_c(Sequential,cbnds,c_rewrite c), k)
			    val con_env = con_tuple_inject(map (fn (v,k) => Var_c v) vk_free)
			    val closure_cb = Con_cb(v,Arrow_k(Closed,vklist,k),
						    Closure_c (Var_c code_var, con_env))
			in  [code_cb, closure_cb]
			end			
		      | cb_rewrite (Fun_cb(_,Closed,_,_,_)) = error "found Fun_cb closed"
		    val cbnds' = List.concat(map cb_rewrite cbnds)
		in Let_c(letsort,cbnds',c_rewrite c)
		end
	  | Typecase_c {arg,arms,default} =>
	    let val arg' = c_rewrite arg
		val default' = Util.mapopt c_rewrite default
		fun do_arm(pc,vklist,c,k) = 
		    let val vklist' = map (fn (v,k) => (v, k_rewrite k)) vklist
		    in (pc,vklist',c_rewrite c, k_rewrite k)
		    end
		val arms' = map do_arm arms
	    in  Typecase_c{arg = arg', arms = arms', default = default'}
	    end
	  | Crecord_c lclist => Crecord_c (map (fn (l,c) => (l, c_rewrite c)) lclist)
	  | Proj_c (c,l) => Proj_c(c_rewrite c, l)
	  | Closure_c (c1,c2) => error "should not encounter Closure_c during closure-conversion"
	  | App_c (c,clist) => App_c(c_rewrite c, map c_rewrite clist)
	  | Annotate_c (annot,c) => Annotate_c(annot, c_rewrite c))

   and k_rewrite arg_kind : kind = 
       (case arg_kind of 
	    ((Type_k _) | (Word_k _)) => arg_kind
	  | Singleton_k (p,k,c) => Singleton_k(p, k_rewrite k, c_rewrite c)
	  | Record_k lv_k_seq => let val lv_k_list = sequence2list lv_k_seq
				     fun doer((l,v),k) = ((l,v),k_rewrite k)
				     val lv_k_list' = map doer lv_k_list
				 in Record_k (list2sequence lv_k_list')
				 end
	  | Arrow_k (openness,vklist,k) => let val vklist' = map (fn (v,k) => (v,k_rewrite k)) vklist
					   in Arrow_k(Closed, vklist', k_rewrite k)
					   end
	  | Code_k _ => error "cannot encounter code_k during closure-conversion")

   fun close_exp arg_exp = 
       let val _ = reset_table()
	   val top_fid = fresh_named_var "top_fid"
	   val state = initial_state top_fid
	   val {freeevars,freecvars} = e_find_fv state arg_exp
	   val _ = print "Done with e_find_fv\n"
	   val _ = (print "free is empty: ";
		    print (Bool.toString (VarMap.numItems freeevars = 0
					  andalso VarMap.numItems freecvars = 0));
		    print "\n")
	   val _ = close_funs(get_fids())
	   val _ = print "Done with close_funs\n"
	   val result = e_rewrite VarSet.empty arg_exp
       in  result
       end	   

end
