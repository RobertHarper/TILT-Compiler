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
	val num_lcon_sum = ref 0
	val num_lcon_inj = ref 0
	val num_lcon_case = ref 0
	val num_lcon_expb = ref 0
	val num_lcon_conb = ref 0
	val sum_depth_case = ref 0
	val sum_depth_inj = ref 0
	val sum_depth_sum = ref 0
	val sum_depth_expb = ref 0
	val sum_depth_conb = ref 0
	fun reset_state() : state = (seen := VarSet.empty; 
				     num_renamed := 0;
				     num_var := 0;
				     num_lexp := 0;
				     num_lcon := 0;
				     num_lkind := 0;
				     num_lcon_sum := 0;
				     num_lcon_inj := 0;
				     num_lcon_case := 0;
				     num_lcon_expb := 0;
				     num_lcon_conb := 0;
				     sum_depth_inj := 0;
				     sum_depth_case := 0;
				     sum_depth_sum := 0;
				     sum_depth_expb := 0;
				     sum_depth_conb := 0;
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

   fun lsw lexp lfunction state do_info do_arg do_prearm ({info,arg,arms,default} : ('info,'arg,'t) sw) 
       : ('info,'arg,'t) sw = 
       let val info' = do_info info
	   val arg' = do_arg arg
	   val default' = Util.mapopt (lexp state) default
	   fun do_arm(prearm,function) = let val prearm' = do_prearm prearm
					     val function' = lfunction state function
					 in  (prearm',function')
					 end
	   val arms' = map do_arm arms
       in {info = info',
	   arg = arg',
	   arms = arms',
	   default = default'}
       end

   fun lbnd state arg_bnd : bnd * state =
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
		Con_b (v,k,c) => let val _ = sum_depth_conb := !sum_depth_conb + 1
				     val c = lcon state c
				     val k = lkind state k
				     val _ = sum_depth_conb := !sum_depth_conb + 1
				     val (state,v) = add_var(state,v)
				 in  (Con_b(v,k,c), state)
				 end
	      | Exp_b (v,c,e) => let val e = lexp state e
				     val _ = sum_depth_expb := !sum_depth_expb + 1
				     val c = lcon state c
				     val _ = sum_depth_expb := !sum_depth_expb - 1
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
		let val _ = (case ap of
				 NilPrimOp inject  => sum_depth_inj := !sum_depth_inj + 1
			       | NilPrimOp inject_record  => sum_depth_inj := !sum_depth_inj + 1
			       | _ => ())
		    val clist' = map (lcon state) clist
		    val _ = (case ap of
				 NilPrimOp inject => sum_depth_inj := !sum_depth_inj - 1
			       | NilPrimOp inject_record => sum_depth_inj := !sum_depth_inj - 1
			       | _ => ())
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
	  | Switch_e switch =>
		let fun lsw'() = lsw lexp lfunction state 
		    fun help pack do_info do_arg do_prearm sw = 
			let val sw' = (lsw'()) do_info do_arg do_prearm sw
			in  Switch_e(pack sw')
			end
		    fun nada arg = arg
		    fun sumhelp c = let val _ = sum_depth_case := !sum_depth_case + 1
					       val result = lcon state c
					       val _ = sum_depth_case := !sum_depth_case - 1
					   in  result
					   end
		in  (case switch of
			 (Intsw_e sw) => help Intsw_e nada (lexp state) nada sw
		       | (Sumsw_e sw) => help Sumsw_e sumhelp (lexp state) nada sw
		       | (Exncase_e sw) => help Exncase_e nada (lexp state) (lexp state) sw
		       | (Typecase_e sw) => help Typecase_e nada (lcon state) nada sw)
		end
	  | Handle_e (e,f) => 
		let val e = lexp state e
		    val f = lfunction state f
		in  Handle_e(e,f)
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
	       Con_cb (v,k,c) => let val k = lkind state k
				     val c = lcon state c
				     val (state,v) = add_var(state,v)
				 in  (Con_cb(v,k,c), state)
				 end
	     | Open_cb arg => lconfun Open_cb arg
	     | Code_cb arg => lconfun Code_cb arg)
       end



   and lcon state arg_con : con = 
       (num_lcon := !num_lcon + 1;
	if (!sum_depth_inj > 0)
	    then num_lcon_inj := !num_lcon_inj + 1
	else ();
	if (!sum_depth_expb > 0)
	    then num_lcon_expb := !num_lcon_expb + 1
	else ();
	if (!sum_depth_conb > 0)
	    then num_lcon_conb := !num_lcon_conb + 1
	else ();
	if (!sum_depth_sum > 0)
	    then num_lcon_sum := !num_lcon_sum + 1
	else ();
	if (!sum_depth_case > 0)
	    then num_lcon_case := !num_lcon_case + 1
	else ();
	case arg_con of
	    Var_c v => (num_var := !num_var + 1; Var_c(find_var(state,v)))
	  | Prim_c (pc,cons) => 
		let val _ = case pc of Sum_c _ => sum_depth_sum := !sum_depth_sum + 1 | _ => ()
		    val cons = map (lcon state) cons
		    val _ = case pc of Sum_c _ => sum_depth_sum := !sum_depth_sum - 1 | _ => ()
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
       (num_lkind := !num_lkind + 1;
	case arg_kind of
	    Type_k _ => arg_kind
	  | Word_k _ => arg_kind
	  | Singleton_k (p,k,c) => let val k = lkind state k
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

		    print "Number of lcon calls with Exp_b: ";
		    print (Int.toString (!num_lcon_expb)); print "\n";
		    print "Number of lcon calls with Con_b: ";
		    print (Int.toString (!num_lcon_conb)); print "\n";

		    print "Number of lcon calls with sums: ";
		    print (Int.toString (!num_lcon_sum)); print "\n";
		    print "Number of lcon calls with case: ";
		    print (Int.toString (!num_lcon_case)); print "\n";
		    print "Number of lcon calls with inj: ";
		    print (Int.toString (!num_lcon_inj)); print "\n";
		    print "Number of lkind calls: ";
		    print (Int.toString (!num_lkind)); print "\n")

       in  MODULE{bnds = bnds,
		  imports = imports,
		  exports = exports}
       end

end
