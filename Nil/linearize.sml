functor Linearize(structure Nil : NIL
		  structure NilUtil : NILUTIL
		  structure Ppnil : PPNIL
		  sharing NilUtil.Nil = Ppnil.Nil = Nil) 
    : LINEARIZE = 
struct

    val error = fn s => Util.error "linearize.sml" s
    structure Nil = Nil
    open Nil Name NilUtil
    val list2set = Util.list2set
    val set2list = Util.set2list
    val list2sequence = Util.list2sequence
    val sequence2list = Util.sequence2list
    val foldl_acc = Listops.foldl_acc
    val linearize = false
    val debug = ref false

    local
	type state =  var VarMap.map
	val seen : VarSet.set ref = ref VarSet.empty
    in
	type state = state
	val num_renamed = ref 0
	val num_var = ref 0
	val num_lexp = ref 0
	val num_lcon = ref 0
	val num_lkind = ref 0

	val num_lcon_prim = ref 0
	val num_lcon_expb = ref 0
	val num_lcon_conb = ref 0
	val num_lcon_concb = ref 0
	val num_lkind_single = ref 0

	val depth_lcon_prim = ref 0
	val depth_lcon_expb = ref 0
	val depth_lcon_conb = ref 0
	val depth_lcon_concb = ref 0
	val depth_lkind_single = ref 0

	fun bumper(num,depth) = if (!depth > 0) then num := !num + 1 else ()
	fun inc n = n := !n + 1
	fun dec n = n := !n - 1
	    
	fun reset_state() : state = (seen := VarSet.empty; 
				     num_renamed := 0;
				     num_var := 0;
				     num_lexp := 0;
				     num_lcon := 0;
				     num_lkind := 0;
				     num_lcon_prim := 0;
				     num_lcon_conb := 0;
				     num_lcon_concb := 0;
				     num_lkind_single := 0;
				     depth_lcon_prim := 0;
				     depth_lcon_expb := 0;
				     depth_lcon_conb := 0;
				     depth_lcon_concb := 0;
				     depth_lkind_single := 0;
				     VarMap.empty)

	fun state_stat str (m : state) : unit = 
	    let val _ = if (!debug)
			    then (print str; print "----\n";
				  print "state map has ";
				  print (Int.toString (VarMap.numItems m)); print "items\n";
				  VarMap.appi (fn (v,v') => if (Name.eq_var(v,v'))
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

	fun find_var (s : state,v : var) : var = 
	     case (VarMap.find(s,v)) of
		 NONE => error ("find_var failed on " ^ (Name.var2string v))
	       | SOME v' => v'

	    
	fun add_var (m : state, v : var) : state * var = 
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
		val m = (case (VarMap.find(m,v)) of
			     NONE => VarMap.insert(m,v,v')
			   | SOME _ => VarMap.insert(#1(VarMap.remove(m,v)),v,v'))
	    in  (m,v')
	    end



    end

   fun lswitch state switch = 
     (case switch of
	  Intsw_e {size,arg,result_type,arms,default} =>
	      let val result_type = lcon state result_type
		  val arg = lexp state arg
		  val arms = map (fn (w,e) => (w,lexp state e)) arms
		  val default = Util.mapopt (lexp state) default
	      in  Intsw_e {size=size,arg=arg,result_type=result_type,arms=arms,default=default}
	      end
	| Sumsw_e {sumtype,arg,result_type,bound,arms,default} =>
	      let val result_type = lcon state result_type
		  val sumtype = lcon state sumtype
		  val arg = lexp state arg
		  val (state,bound) = add_var(state,bound)
		  val arms = map (fn (t,e) => (t,lexp state e)) arms
		  val default = Util.mapopt (lexp state) default
	      in  Sumsw_e {sumtype=sumtype,arg=arg,result_type=result_type,
			   bound=bound,arms=arms,default=default}
	      end
	| Exncase_e {arg,result_type,bound,arms,default} =>
	      let val result_type = lcon state result_type
		  val arg = lexp state arg
		  val (state,bound) = add_var(state,bound)
		  val arms = map (fn (e1,e2) => (lexp state e1,lexp state e2)) arms
		  val default = Util.mapopt (lexp state) default
	      in  Exncase_e {arg=arg,result_type=result_type,
			     bound=bound,arms=arms,default=default}
	      end
	| Typecase_e _ => error "typecase not handled")

   and lbnd state arg_bnd : bnd * state =
       let 
	   fun add_vars state vx_list = foldl (fn ((v,_),s) => #1(add_var(s,v))) state vx_list
	   fun vf_help wrapper vf_set = 
	       let val vf_list = set2list vf_set
		   val newstate = add_vars state vf_list
		   val vf_list = map (fn (v,f) => (find_var(newstate,v),
						   lfunction newstate f)) vf_list
	       in  (wrapper (list2set vf_list), newstate)
	       end
	   fun vcl_help state (v,{code,cenv,venv,tipe}) = 
	       let val v = find_var(state,v)
		   val cenv' = lcon state cenv
		   val tipe' = lcon state tipe
		   val venv' = lexp state venv
		   val code' = find_var(state,code)
	       in  (v,{code=code',cenv=cenv',venv=venv',tipe=tipe'})
	       end
	   fun mapset f s = list2set(map f (set2list s))

       in  (case arg_bnd of
		Con_b (v,c) => let val _ = inc depth_lcon_conb
				   val c = lcon state c
				   val _ = dec depth_lcon_conb
				   val (state,v) = add_var(state,v)
			       in  (Con_b(v,c), state)
			       end
	      | Exp_b (v,c,e) => let val e = lexp state e
				     val _ = inc depth_lcon_expb
				     val c = lcon state c
				     val _ = dec depth_lcon_expb
				     val (state,v) = add_var(state,v)
				 in  (Exp_b(v,c,e), state)
				 end
	      | Fixopen_b vf_set => vf_help Fixopen_b vf_set
	      | Fixcode_b vf_set => vf_help Fixcode_b vf_set
(* RECURSIVE BINDING *)
	      | Fixclosure_b (flag,vcl_set) => 
				 let val state = add_vars state vcl_set
				     val vcl_list = set2list vcl_set
				     val vcl_list = map (vcl_help state) vcl_list
				     val fixbnd = Fixclosure_b(flag,list2set vcl_list)
				 in  (fixbnd, state)
				 end)
       end

   and lfunction state (Function(effect,recur,vklist,vclist,vflist,e,c)) : function =
       let 
	   val (vklist,state) = lvklist state vklist
	   val (vclist,state) = lvclist state vclist
	   fun vfolder(v,state) = 
	       let val (state,v) = add_var(state,v)
	       in (v,state)
	       end
	   val (vflist,state) = foldl_acc vfolder state vflist
	   val e = lexp state e
	   val c = lcon state c
       in  Function(effect,recur,vklist,vclist,vflist,e,c)
       end



   and lexp state arg_exp : exp =
	let val _ = num_lexp := !num_lexp + 1;
	    val self = lexp state
	in  case arg_exp of
	    Var_e v => (num_var := !num_var + 1; Var_e(find_var(state,v)))
	  | Const_e _ => (arg_exp)
	  | Let_e (_,bnds,e) => 
		let fun folder (bnd,s) = lbnd s bnd
		    val (bnds,state) = foldl_acc folder state bnds
		    val e = lexp state e
		in  (lete(bnds, e))
		end
	  | Prim_e (ap,clist,elist) =>
		let val _ = inc depth_lcon_prim
		    val clist' = map (lcon state) clist
		    val _ = dec depth_lcon_prim
		    val elist' = map (lexp state) elist
		in  Prim_e(ap,clist',elist')
		end
	  | App_e (openness,f,clist,elist,flist) =>
		let val f = lexp state f
		    val clist = map (lcon state) clist
		    val elist = map (lexp state) elist
		    val flist = map (lexp state) flist
		in  App_e (openness,f,clist,elist,flist)
		end
	  | Raise_e (e,c) => 
		let val e = lexp state e
		    val c = lcon state c
		in  Raise_e(e,c)
		end
	  | Switch_e switch => Switch_e(lswitch state switch)
	  | Handle_e (e,v,handler,c) => 
		let val e = lexp state e
		    val c = lcon state c
		    val (state,v) = add_var(state,v)
		    val handler = lexp state handler
		in  Handle_e(e,v,handler,c)
		end
	end


   and lcbnd state arg_cbnd : conbnd * state =
       let val _ = state_stat "lcbnd" state
	   fun lconfun wrapper (v,vklist,c,k) = 
	   let val (vklist,state) = lvklist state vklist
	       val c = lcon state c
	       val k = lkind state k
	       val (state,v) = add_var(state,v)
	       val cbnd = wrapper(v,vklist,c,k)
	       val _ = state_stat "lcbnd: lconfun" state
	   in  (cbnd, state)
	   end
       in (case arg_cbnd of
	       Con_cb (v,c) => let   val _ = inc depth_lcon_concb
				     val c = lcon state c
				     val _ = dec depth_lcon_concb
				     val (state,v) = add_var(state,v)
				 in  (Con_cb(v,c), state)
				 end
	     | Open_cb arg => lconfun Open_cb arg
	     | Code_cb arg => lconfun Code_cb arg)
       end



   and lcon state arg_con : con = 
       (inc num_lcon;
	bumper(num_lcon_prim, depth_lcon_prim);
	bumper(num_lcon_expb, depth_lcon_expb);
	bumper(num_lcon_conb, depth_lcon_conb);
	bumper(num_lcon_concb, depth_lcon_concb);


	case arg_con of
	    Var_c v => (num_var := !num_var + 1; Var_c(find_var(state,v)))
	  | Prim_c (pc,cons) => 
		let 
		    val cons = map (lcon state) cons
		    
		in  Prim_c(pc,cons)
		end
	  | Mu_c (flag,vc_seq) => (* cannot just use lvclist here: 
				   not sequential bindings *)
		let val vclist = sequence2list vc_seq
		    val state = foldl (fn ((v,_),s) => #1(add_var(s,v))) state vclist
		    val vclist' = map (fn (v,c) => (derived_var v, c)) vclist
		    val (vclist',state) = lvclist state vclist'
		    val vclist = Listops.map2 (fn ((v,_),(_,c)) => (find_var(state,v),c)) 
			(vclist,vclist')
		in  Mu_c(flag,list2sequence vclist)
		end
	  | AllArrow_c (openness,effect,vklist,clist,w32,c) =>
		let 
		    val (vklist,state) = lvklist state vklist
		    val clist = map (lcon state) clist
		    val c = lcon state c
		in  AllArrow_c (openness,effect,vklist,clist,w32,c)
		end
	  | Let_c (letsort,cbnds,c) =>
		let 
		    fun folder(cbnd,state) = lcbnd state cbnd
		    val (cbnds,state) = foldl_acc folder state cbnds
		    val _ = state_stat "let_c after fold" state
		    val c = lcon state c

		in  Let_c(letsort, cbnds, c)
		end
	  | Crecord_c lc_list => 
		let fun doer(l,c) = (l, lcon state c)
		in  Crecord_c (map doer lc_list)
		end
	  | Proj_c (c,l) => Proj_c(lcon state c, l)

	  | Closure_c (c1,c2) => Closure_c(lcon state c1, lcon state c2)

	  | App_c (c,clist) => let val c = lcon state c
				   val clist = map (lcon state) clist
			       in  App_c(c,clist)
			       end
          | Typecase_c {arg : con,
			arms : (primcon * (var * kind) list * con) list,
			default : con,
			kind : kind} => 
			       let val arg = lcon state arg
				   val default = lcon state default
				   val kind = lkind state kind
				   fun do_arm ((pc,vklist,c),(acc,state)) = 
				       let val (vklist,state) = lvklist state vklist
					   val c = lcon state c
				       in  ((pc,vklist,c)::acc, state)
				       end
				   val (rev_arms,state) = foldl do_arm ([],state) arms
				   val arms = rev rev_arms
			       in  Typecase_c {arg = arg,
					       arms = arms,
					       default = default,
					       kind = kind}
			       end
     | Annotate_c (a,c) => Annotate_c(a,lcon state c))


   and lvklist state vklist = 
       let fun vkfolder((v,k),(acc,state)) = 
	   let val k = lkind state k
	       val (state,v) = add_var(state,v)
	   in  ((v,k)::acc, state)
	   end
	   val (rev_vklist,state) = foldl vkfolder ([],state) vklist
       in  (rev rev_vklist, state)
       end

   and lvclist state vclist = 
       let fun vcfolder((v,c),(acc,state)) = 
	   let val c = lcon state c
	       val (state,v) = add_var(state,v)
	   in  ((v,c)::acc, state)
	   end
	   val (rev_vclist,state) = foldl vcfolder ([],state) vclist
       in  (rev rev_vclist, state)
       end


   and lkind state arg_kind : kind = 
       (inc num_lkind;
	bumper(num_lkind_single, depth_lkind_single);

	case arg_kind of
	    Type_k _ => arg_kind
	  | Word_k _ => arg_kind
	  | Singleton_k (p,k,c) => let val _ = inc depth_lkind_single
				       val k = lkind state k
				       val _ = dec depth_lkind_single
				       val c = lcon state c
				   in  Singleton_k(p,k,c)
				   end
	  | Record_k lvk_seq => let val lvk_list = sequence2list lvk_seq
				    val vk_list = map (fn ((l,v),k) => (v,k)) lvk_list
				    val (vk_list,state) = lvklist state vk_list
				    val lvk_list = Listops.map2 (fn (((l,_),_),(v,k)) => ((l,v),k)) 
					(lvk_list,vk_list)
				in  Record_k (list2sequence lvk_list)
				end
	  | Arrow_k(openness,vklist,k) => let val (vklist,state) = lvklist state vklist
					      val k = lkind state k
					  in  Arrow_k(openness,vklist,k)
					  end)


   fun lexport state (ExportValue(l,e,c)) = 
       let val e = lexp state e
	   val c = lcon state c
       in  ExportValue(l,e,c)
       end
     | lexport state (ExportType(l,c,k)) = 
       let val c = lcon state c
	   val k = lkind state k
       in  ExportType(l,c, k)
       end

   fun limport (ImportValue(l,v,c),s) =
       let val (s,v) = add_var(s,v)
       in  (ImportValue(l,v,lcon s c),s)
       end
     | limport (ImportType(l,v,k),s) =
       let val (s,v) = add_var(s,v)
       in  (ImportType(l,v,lkind s k),s)
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
	   fun folder (bnd,state) = lbnd state bnd
	   val (bnds,state) = foldl_acc folder state bnds
	   val exports = map (lexport state) exports
	   val _ = (print "Number of renamed variables: ";
		    print (Int.toString (!num_renamed)); print "\n";
		    print "Number of variables: ";
		    print (Int.toString (!num_var)); print "\n";
		    print "Number of lexp calls: ";
		    print (Int.toString (!num_lexp)); print "\n";
		    print "Number of lcon calls: ";
		    print (Int.toString (!num_lcon)); print "\n";

		    print "Number of lcon calls within Prim_e: ";
		    print (Int.toString (!num_lcon_prim)); print "\n";
		    print "Number of lcon calls with Exp_b: ";
		    print (Int.toString (!num_lcon_expb)); print "\n";
		    print "Number of lcon calls with Con_b: ";
		    print (Int.toString (!num_lcon_conb)); print "\n";
		    print "Number of lcon calls with Con_cb: ";
		    print (Int.toString (!num_lcon_concb)); print "\n";

		    print "Number of lkind calls: ";
		    print (Int.toString (!num_lkind)); print "\n";
		    print "Number of lkind calls inside singleton kinds: ";
		    print (Int.toString (!num_lkind_single)); print "\n")

       in  MODULE{bnds = bnds,
		  imports = imports,
		  exports = exports}
       end

end
