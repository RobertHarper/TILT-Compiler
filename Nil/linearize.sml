functor Linearize(structure Nil : NIL
		  structure NilUtil : NILUTIL
		  structure Ppnil : PPNIL
		  sharing NilUtil.Nil = Ppnil.Nil = Nil) 
    : LINEARIZE = 
struct

    val error = fn s => Util.error "linearize.sml" s
    structure Nil = Nil
    open Nil Name NilUtil
    val flatten = Listops.flatten
    val list2set = Util.list2set
    val set2list = Util.set2list
    val list2sequence = Util.list2sequence
    val sequence2list = Util.sequence2list
    val foldl_acc = Listops.foldl_acc
    val linearize = false
    val debug = ref false
    val killunused = ref false

    local
	datatype cell = Use | Defer of cellref list
	withtype cellref = (var * cell) ref
	type state = (cellref list) * (var * cellref) VarMap.map
	val seen : VarSet.set ref = ref VarSet.empty
	fun augmentCell (r1,r2) = 
	    (if (r1 = r2)
		 then ()
	     else (case !r1 of
		       (_,Use) => ()
		     | (v,Defer list) => r1 := (v,Defer(r2 :: list))))
	fun skip 0 = ()
	  | skip n = (print " "; skip (n-1))
	fun cellref2bool tab (ref(v,c)) = 
	    ((* skip tab;
	     print "cellref2bool on v = "; Ppnil.pp_var v; *)
	     let val res = (case c of
				Use => ((* print "Use\n"; *) true)
			      | Defer cells => ((* print "Defer\n"; *) Listops.orfold (cellref2bool (tab+5)) cells))
(*		 val _ = (skip tab; print "cellref2bool on v = "; Ppnil.pp_var v; 
			  print " = "; print (Bool.toString res); print "\n") *)
	     in  res
	     end)
	val cellref2bool = fn arg => cellref2bool 0 arg
    in
	type state = state
	val num_renamed = ref 0
	fun reset_state() : state = (seen := VarSet.empty; 
				     num_renamed := 0;
				     ([ref (fresh_named_var "outer", Use)], VarMap.empty))
	fun copy_state ((curcells,s) : state) = ((ref(fresh_named_var "copy",Use)) :: curcells,s)
	fun state_stat str ((_,m) : state) : unit = 
	    let val _ = if (!debug)
			    then (print str; print "----\n";
				  print "state map has ";
				  print (Int.toString (VarMap.numItems m)); print "items\n";
				  VarMap.appi (fn (v,(v',_)) => if (Name.eq_var(v,v'))
								    then ()
								else (Ppnil.pp_var v;
								      print " -> ";
								      Ppnil.pp_var v';
								      print "\n")) m;
				  print "state seen has ";
				  print (Int.toString (VarSet.numItems (!seen))); print "items\n")
			else ()
	    in ()
	    end
	fun is_used ((_,s) : state, v) =
	    (not (!killunused) orelse
	     case (VarMap.find(s,v)) of
		 NONE => error ("is_used failed on " ^ (Name.var2string v))
	       | SOME (_,cellref) => cellref2bool cellref)

	fun find_var ((curcells,s) : state,v : var) : var = 
	    ((* print "find_var called on "; Ppnil.pp_var v; print "\n"; *)
	     case (VarMap.find(s,v)) of
		 NONE => error ("find_var failed on " ^ (Name.var2string v))
	       | SOME (v',r) => (augmentCell(r,hd curcells); v'))

	    
	fun add_var ((curcells,m) : state, v : var) : state * var = 
	    let val is_seen = VarSet.member(!seen,v)
		val _ = if (!debug)
			    then (print ("add_var on " ^ (Name.var2string v));
				  if is_seen then print "  RENAME" else ();
				      print "\n")
			else ()
		val _ = if is_seen
			    then num_renamed := !num_renamed + 1
			else ()
		val _ = seen := (if is_seen then !seen else VarSet.add(!seen,v))
		val v' = if is_seen then derived_var v else v
		val newcurcell = ref (v',Use)
		val m = (case (VarMap.find(m,v)) of
			     NONE => VarMap.insert(m,v,(v',newcurcell))
			   | SOME _ => VarMap.insert(#1(VarMap.remove(m,v)),v,(v',newcurcell)))
	    in  ((newcurcell::curcells,m),v')
	    end

	fun pop_var([],m) = error "pop_var got empty stack"
	  | pop_var(topcell::curcells,m) : state = (topcell := (#1(!topcell), Defer[]);
						    (curcells,m))

    end

   fun lsw lexp lfunction state do_info do_arg do_prearm ({info,arg,arms,default} : ('info,'arg,'t) sw) 
       : bnd list * ('info,'arg,'t) sw = 
       let val (info_bnd,info') = do_info info
	   val (arg_bnd,arg') = do_arg arg
	   val (default_bnd, default') = (case default of
					      NONE => ([],NONE) 
					    | SOME e => let val (bnd,e) = lexp state e
							in  (bnd,SOME e)
							end)
	   fun do_arm(prearm,function) = let val (bnd,prearm') = do_prearm prearm
					     val function' = lfunction state function
					 in  (bnd,(prearm',function'))
					 end
	   val armbnd_arms = map do_arm arms
	   val armbnd = map #1 armbnd_arms
	   val arms' = map #2 armbnd_arms
	   val bnds = info_bnd @ arg_bnd @ default_bnd @ (flatten armbnd)
       in (bnds,{info = info',
		 arg = arg',
		 arms = arms',
		 default = default'})
       end

   fun lbnd state arg_bnd : state * bnd list =
       let 
(*
	   val _ = (print "\nlinearizing bnd:\n";
		    Ppnil.pp_bnd arg_bnd;
		    state_stat "lbnd" state;
		    print "\n")
*)
	   fun add_vars state vx_list = foldl (fn ((v,_),s) => #1(add_var(s,v))) state vx_list
	   fun pop_vars state vx_list = foldl (fn (_,s) => pop_var s) state vx_list
	   fun vf_help wrapper vf_set = 
	       let val vf_list = set2list vf_set
		   val newstate = add_vars state vf_list
		   val newstate = pop_vars newstate vf_list
		   val vf_list = map (fn (v,f) => (find_var(newstate,v),
						   lfunction newstate f)) vf_list
	       in  (newstate, [wrapper (list2set vf_list)])
	       end
	   fun vcl_help state (v,{code,cenv,venv,tipe}) = 
	       let val v = find_var(state,v)
		   val (bnd_cenv,cenv') = lcon2 state cenv
		   val (bnd_tipe,tipe') = lcon2 state tipe
		   val (bnd_venv,venv') = lexp state venv
		   val code' = find_var(state,code)
		   val bnd = bnd_cenv @ bnd_tipe @ bnd_venv
	       in  (bnd,(v,{code=code',cenv=cenv',venv=venv',tipe=tipe'}))
	       end
	   fun mapset f s = list2set(map f (set2list s))

       in  (case arg_bnd of
		Con_b (v,k,c) => let val (state,v) = add_var(state,v)
				     val (cbnds,c) = lcon state c
				     val (cbnds_k,k) = lkind state k
				     val cbnd = Con_cb(v,k,c)
				     val state = pop_var state
				 in  (state, map NilUtil.cbnd2bnd (cbnds @ cbnds_k @ [cbnd]))
				 end
	      | Exp_b (v,c,e) => let val (state,v) = add_var(state,v)
				     val (bnds,e) = lexp state e
				     val (bnds_c,c) = lcon2 state c
				     val bnd = Exp_b(v,c,e)
				     val state = pop_var state
				 in  (state, bnds @ bnds_c @ [bnd])
				 end
	      | Fixopen_b vf_set => vf_help Fixopen_b vf_set
	      | Fixcode_b vf_set => vf_help Fixcode_b vf_set
(* RECURSIVE BINDING *)
	      | Fixclosure_b (flag,vcl_set) => let val state = add_vars state vcl_set
						   val state = pop_vars state vcl_set
						   val vcl_list = set2list vcl_set
						   val bnd_vcl = map (vcl_help state) vcl_list
						   val fixbnd = Fixclosure_b(flag,list2set(map #2 bnd_vcl))
						   val bnds = flatten (map #1 bnd_vcl)
					       in  (state,bnds @ [fixbnd])
					       end)
       end

   and lfunction state (Function(effect,recur,vklist,vclist,vflist,e,c)) : function =
       let 
	   val state = copy_state state
	   val (cbnds_vk,vklist,state) = lvklist state vklist
	   val (cbnds_vc,vclist,state) = lvclist state vclist
	   fun vfolder(v,state) = 
	       let val (state,v) = add_var(state,v)
		   val state = pop_var state
	       in (v,state)
	       end
	   val (vflist,state) = foldl_acc vfolder state vflist
	   val (bnds_e,e) = lexp state e
	   val (bnds_c,c) = lcon2 state c
	   val bnds = (map NilUtil.cbnd2bnd cbnds_vk) @ (map NilUtil.cbnd2bnd cbnds_vc) @ bnds_e @ bnds_c
	   val e = (case bnds of 
		      [] => e
		    | bnds => lete(bnds,e))
       in  Function(effect,recur,vklist,vclist,vflist,e,c)
       end


   and lexp state arg_exp : bnd list * exp = 
       let 
(*
	   val _ = 
	       (print "\nlinearizing exp:\n";
		Ppnil.pp_exp arg_exp;
		state_stat "lexp" state;
		print "\n")
*)
	   val (bnds,e) = lexp' state arg_exp
       in  (case e of
		Var_e _ => (bnds,e) 
	      | Const_e _ => (bnds,e)
	      | _ => if linearize
			 then error "unimplemented"
		     else (bnds,e))
       end

   and lexp' state arg_exp : bnd list * exp = 
	let 
	    val self = lexp state
	in  case arg_exp of
	    Var_e v => ([],Var_e(find_var(state,v)))
	  | Const_e _ => ([],arg_exp)
	  | Let_e (_,bnds,e) => 
		let fun folder (bnd,(s,acc)) = let val (s,bnds) = lbnd s bnd
					       in  (s, (bnd,bnds) :: acc)
					       end
		    val (state,rev_bnd_bnds) = foldl folder (state,[]) bnds
		    val (bnds'',e') = lexp state e
		    fun folder ((orig_bnd,bnds),acc) = 
			 let val res = case orig_bnd of
			     Con_b(v,k,c) => if (is_used(state,v)) (* constructors never have an effect *)
						 then bnds@acc
					     else (if (!debug)
						       then (print "discarding con variable ";
							     Ppnil.pp_var v; print "\n")
						   else ();
						   acc)
			   | Exp_b(v,c,e) => if ((is_used(state,v)) orelse (NilUtil.effect e))
						 then bnds@acc
					     else (if (!debug)
						       then (print "discarding exp variable ";
							     Ppnil.pp_var v; print "\n")
						   else ();
						   acc)
			   | _ => bnds @ acc
			 in res
			 end
		    val final_bnds = foldl folder bnds'' rev_bnd_bnds
		in  ([],lete(final_bnds, e'))
		end
	  | Prim_e (ap,clist,elist) =>
		let val cbnds_con_list = map (lcon state) clist
		    val clist' = map #2 cbnds_con_list
		    val cbnds = flatten (map #1 cbnds_con_list)
		    val bnds_exp_list = map (lexp state) elist
		    val elist' = map #2 bnds_exp_list
		    val bnds = flatten (map #1 bnds_exp_list)
		in  (bnds @ (map NilUtil.cbnd2bnd cbnds), Prim_e(ap,clist',elist'))
		end
	  | App_e (openness,f,clist,elist,flist) =>
		let val cbnds_con_list = map (lcon state) clist
		    val clist' = map #2 cbnds_con_list
		    val cbnds = flatten (map #1 cbnds_con_list)
		    val bnds_exp_list = map (lexp state) elist
		    val elist' = map #2 bnds_exp_list
		    val bnds = flatten (map #1 bnds_exp_list)
		    val fbnds_fexp_list = map (lexp state) flist
		    val flist' = map #2 fbnds_fexp_list
		    val fbnds = flatten (map #1 fbnds_fexp_list)
		    val (fbnd,f') = lexp state f
		in  (fbnd @ bnds @ fbnds @ (map NilUtil.cbnd2bnd cbnds), App_e(openness,f',clist',elist',flist'))
		end
	  | Raise_e (e,c) => 
		let val (bnd,e') = lexp state e
		    val (cbnd,c') = lcon state c
		in  (bnd @ (map NilUtil.cbnd2bnd cbnd), Raise_e(e',c'))
		end
	  | Switch_e switch =>
		let fun lsw'() = lsw lexp lfunction state 
		    fun help pack do_info do_arg do_prearm sw = 
			let val (bnd,sw') = (lsw'()) do_info do_arg do_prearm sw
			in  (bnd,Switch_e(pack sw'))
			end
		    fun nada arg = ([],arg)
		    fun sumhelp (w,cons) = let val cbnds_cons = map (lcon state) cons
					       fun help (cbnds,_) = map NilUtil.cbnd2bnd cbnds
					       val cbnds = flatten(map help cbnds_cons)
					   in  (cbnds, (w,map #2 cbnds_cons))
					   end
		in  (case switch of
			 (Intsw_e sw) => help Intsw_e nada (lexp state) nada sw
		       | (Sumsw_e sw) => help Sumsw_e sumhelp (lexp state) nada sw
		       | (Exncase_e sw) => help Exncase_e nada (lexp state) (lexp state) sw
		       | (Typecase_e sw) => help Typecase_e nada (lcon2 state) nada sw)
		end
	  | Handle_e (e,f) => 
		let val (bnd,e') = lexp state e
		    val f' = lfunction state f
		in  (bnd,Handle_e(e',f'))
		end
	end


   and lcon2 state arg_con : bnd list * con = 
       let val (cbnds,c) = lcon state arg_con
       in  (map NilUtil.cbnd2bnd cbnds, c)
       end

   and lcbnd state arg_cbnd : state * conbnd list = 
       let val _ = state_stat "lcbnd" state
	   fun lconfun wrapper (v,vklist,c,k) = 
	   let val state = copy_state state 
	       val (state',v) = add_var(state,v)
	       val state' = pop_var state'
	       val (cbnds_vk,vklist,state) = lvklist state' vklist
	       val (cbnds_c,c) = lcon state c
	       val (cbnds_k,k) = lkind state k
	       val cbnd = wrapper(v,vklist,c,k)
	       val _ = state_stat "lcbnd: lconfun" state'
	   in  (state', cbnds_vk @ cbnds_c @ cbnds_k @ [cbnd])
	   end
       in (case arg_cbnd of
	       Con_cb (v,k,c) => let val (state,v) = add_var(state,v)
				     val (cbnd_k,k) = lkind state k
				     val (cbnd_c,c) = lcon state c
				     val state = pop_var state
				 in  (state, cbnd_k @ cbnd_c @ [Con_cb(v,k,c)])
				 end
	     | Open_cb arg => lconfun Open_cb arg
	     | Code_cb arg => lconfun Code_cb arg)
       end

   and lcon' state arg_con : con = let val (cbnds,c) = lcon state arg_con
				   in  Let_c(Sequential,cbnds,c)
				   end

   and lcon state arg_con : conbnd list * con = 
       (
(*
	print "\nlinearizing con:\n";
	Ppnil.pp_con arg_con;
	state_stat "lcon" state;
	print "\n";
*)
	case arg_con of
	    Prim_c (pc,cons) => 
		let val cbnd_cons = map (lcon state) cons
		    val cons' = map #2 cbnd_cons
		    val cbnds = flatten (map #1 cbnd_cons)
		in  (cbnds, Prim_c(pc,cons'))
		end
	  | Mu_c (flag,vc_seq,v) => (* cannot just use lvclist here: not sequential bindings *)
		let val vclist = sequence2list vc_seq
		    val state = foldl (fn ((v,_),s) => #1(add_var(s,v))) state vclist
		    val vclist' = map (fn (v,c) => (derived_var v, c)) vclist
		    val (cbnds,vclist',state) = lvclist state vclist'
		    val vclist = Listops.map2 (fn ((v,_),(_,c)) => (find_var(state,v),c)) 
			(vclist,vclist')
		    val v = find_var(state,v)
		in  (cbnds,Mu_c(flag,list2sequence vclist, v))
		end
	  | AllArrow_c (openness,effect,vklist,clist,w32,c) =>
		let 
		    val (cbnds_vk, vklist,state) = lvklist state vklist
		    val cbnd_clist = map (lcon state) clist
		    val cbnd_c = lcon state c
		    val cbnds = cbnds_vk @ (flatten (map #1 cbnd_clist)) @ (#1 cbnd_c)
		    val clist = map #2 cbnd_clist
		    val c = #2 cbnd_c
		in  (cbnds, AllArrow_c (openness,effect,vklist,clist,w32,c))
		end
	  | Var_c v => ([],Var_c(find_var(state,v)))
	  | Let_c (letsort,cbnds,c) =>
		let 
		    fun folder(cbnd,(acc,state)) = 
			let val (state,cbnds') = lcbnd state cbnd
			    val _ = state_stat "let_c folder: after call" state
			in  (cbnds' :: acc, state)
			end
		    val (rev_cbnds,state) = foldl folder ([],state) cbnds
		    val _ = state_stat "let_c after fold" state
(*
		    val _ = (print "rev_cbnds are: ";
			     app (fn cbnds => (Ppnil.pp_list Ppnil.pp_conbnd' cbnds ("","","",true);
					       print "\n")) rev_cbnds;
			     print "\n")
*)
		    val (cbnds',c') = lcon state c
		    fun folder ((orig_cbnd,cbnds),acc) = 
			(case orig_cbnd of
			     Con_cb(v,_,_) => if (is_used (state,v)) (* constructors never have an effect *)
						 then cbnds@acc
					     else acc
			   | _ => cbnds@acc)
		    val final_cbnds = foldl folder cbnds' (Listops.zip cbnds rev_cbnds)
(*
		    val _ = (print "final_cbnds are: ";
			     Ppnil.pp_list Ppnil.pp_conbnd' cbnds ("","","",true);
			     print "\n")
*)
		in  ([], Let_c(letsort, final_cbnds, c'))
		end
	  | Crecord_c lc_list => let val cbnd_lclist = map (fn (l,c) => 
							    let val (cbnd,c) = lcon state c
							    in (cbnd,(l,c))
							    end) lc_list
				 in  (flatten(map #1 cbnd_lclist), 
				      Crecord_c (map #2 cbnd_lclist))
				 end
	  | Proj_c (c,l) => let val (cbnd,c) = lcon state c
			    in  (cbnd, Proj_c(c,l))
			    end
	  | Closure_c (c1,c2) => let val (cbnd1,c1)= lcon state c1
				     val (cbnd2,c2)= lcon state c2
				 in  (cbnd1 @ cbnd2, 
				      Closure_c(c1,c2))
				 end
	  | App_c (c,clist) => let val (cbnd,c) = lcon state c
				   val cbnd_clist = map (lcon state) clist
			       in  (cbnd @ (flatten (map #1 cbnd_clist)),
				    App_c(c,map #2 cbnd_clist))
			       end
          | Typecase_c {arg : con,
			arms : (primcon * (var * kind) list * con) list,
			default : con,
			kind : kind} => 
			       let val (cbnd_arg,arg) = lcon state arg
				   val (cbnd_def,default) = lcon state default
				   val (cbnd_kind,kind) = lkind state kind
				   fun do_arm ((pc,vklist,c),(acc,cbnds,state)) = 
				       let val (cbnds_vk,vklist,state) = lvklist state vklist
					   val (cbnds_c,c) = lcon state c
				       in  ((pc,vklist,c)::acc,cbnds_vk @ cbnds_c @ cbnds, state)
				       end
				   val (arms,cbnds,state) = foldl do_arm ([],[],state) arms
			       in  (cbnd_arg @ cbnd_def @ cbnds @ cbnd_kind,
				    Typecase_c {arg = arg,
						arms = arms,
						default = default,
						kind = kind})
			       end
     | Annotate_c (a,c) => let val (cbnds,c) = lcon state c
			   in  (cbnds, Annotate_c(a,c))
			   end)

   and lvklist state vklist = 
       let fun vkfolder((v,k),(cbnds,acc,state)) = 
	   let val (state,v) = add_var(state,v)
	       val state = pop_var state
	       val (cbnd,k) = lkind state k
	   in  ((rev cbnd)@cbnds, (v,k)::acc, state)
	   end
	   val (rev_cbnds, rev_vklist,state) = foldl vkfolder ([],[],state) vklist
       in  (rev rev_cbnds, rev rev_vklist, state)
       end

   and lvclist state vclist = 
       let fun vcfolder((v,c),(cbnds,acc,state)) = 
	   let val (state,v) = add_var(state,v)
	       val state = pop_var state
	       val (cbnd,c) = lcon state c
	   in  ((rev cbnd)@cbnds, (v,c)::acc, state)
	   end
	   val (rev_cbnds, rev_vclist,state) = foldl vcfolder ([],[],state) vclist
       in  (rev rev_cbnds, rev rev_vclist, state)
       end

   and lkind' state arg_kind : kind = 
       let val (cbnds,k) = lkind state arg_kind
       in  case (cbnds,k) of
	   (_,Type_k _) => k
	 | (_,Word_k _) => k
	 | (_,Singleton_k(p,k,c)) => Singleton_k(p,k,Let_c(Sequential,cbnds,c))
	 | ([],Record_k _) => k
	 | ([],Arrow_k _) => k
	 | (_,Record_k _) => error "lkind' with non-empty cbnds and record_k"
	 | (_,Arrow_k _) => error "lkind' with non-empty cbnds and arrow_k"
       end

   and lkind state arg_kind : conbnd list * kind = 
       (case arg_kind of
	    Type_k _ => ([],arg_kind)
	  | Word_k _ => ([],arg_kind)
	  | Singleton_k (p,k,c) => let val (cbnd_k,k) = lkind state k
				       val (cbnd_c,c) = lcon state c
				   in (cbnd_k @ cbnd_c, Singleton_k(p,k,c))
				   end
	  | Record_k lvk_seq => let val lvk_list = sequence2list lvk_seq
				    val vk_list = map (fn ((l,v),k) => (v,k)) lvk_list
				    val (cbnds,vk_list,state) = lvklist state vk_list
				    val lvk_list = Listops.map2 (fn (((l,_),_),(v,k)) => ((l,v),k)) 
					(lvk_list,vk_list)
				in  (cbnds, Record_k (list2sequence lvk_list))
				end
	  | Arrow_k(openness,vklist,k) => let val (cbnds_vk,vklist,state) = lvklist state vklist
					      val (cbnds_k,k) = lkind state k
					  in  (cbnds_vk @ cbnds_k, Arrow_k(openness,vklist,k))
					  end)


   fun lexport state (ExportValue(l,e,c)) = 
       let val (bnds,e) = lexp state e
	   val (cbnds,c) = lcon state c
       in  ExportValue(l,lete(bnds,e),letc(cbnds,c))
       end
     | lexport state (ExportType(l,c,k)) = 
       let val (cbnds_c,c) = lcon state c
	   val (cbnds_k,k) = lkind state k
	   val _ = (case cbnds_k of
			[] => ()
		      | _ => error "lexport: ExportType has nontrivial kind")
       in  ExportType(l,letc(cbnds_c,c), k)
       end

   fun limport (ImportValue(l,v,c),s) =
       let val (s,v) = add_var(s,v)
       in  (ImportValue(l,v,lcon' s c),s)
       end
     | limport (ImportType(l,v,k),s) =
       let val (s,v) = add_var(s,v)
       in  (ImportType(l,v,lkind' s k),s)
       end

   fun limports (imports,s) = 
       let fun folder(imp,(acc,s)) = let val (imp,s) = limport(imp,s)
				     in  (imp::acc,s)
				     end
	   val (rev_imps,s) = foldl folder ([],s) imports
       in  (rev rev_imps, s)
       end
       
   fun linearize_mod (MODULE{bnds,imports,exports}) = 
       let val state = reset_state()
	   val (imports,state) = limports(imports,state)
	   fun folder (bnd,(acc,state)) = let val (state,bnds) = lbnd state bnd
					  in  (rev bnds @ acc, state)
					  end
	   val (rev_bnds,state) = foldl folder ([],state) bnds
	   val exports = map (lexport state) exports
	   val _ = (print "Number of renamed variables: ";
		    print (Int.toString (!num_renamed));
		    print "\n")
       in  MODULE{bnds = rev rev_bnds,
		  imports = imports,
		  exports = exports}
       end

end
