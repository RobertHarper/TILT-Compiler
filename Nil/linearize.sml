(*$import Prelude TopLevel Util Stats Nil Int Prim Array String Name Listops Sequence LINEARIZE NilUtil Ppnil Normalize *)

structure Linearize
    :> LINEARIZE =
struct

    val error = fn s => Util.error "linearize.sml" s
    val show_stats = Stats.ff("LinearizeStats")
    val linearize = Stats.tt("Linearize")
    val debug = ref false

    open Nil Name NilUtil Ppnil Listops
    val list2sequence = Sequence.fromList
    val sequence2list = Sequence.toList



    fun map_unzip f ls = Listops.unzip(map f ls)

    local
	type state = bool * var VarMap.map
	val seen : VarSet.set ref = ref VarSet.empty
    in
	type state = state
	val num_renamed = ref 0
	val num_var = ref 0
	val num_lexp = ref 0
	val num_lcon = ref 0
	val num_lkind = ref 0

	val num_lcon_prim = ref 0
	val num_lcon_import = ref 0
	val num_lcon_conb = ref 0
	val num_lcon_concb = ref 0
	val num_lcon_single = ref 0
	val num_lcon_function = ref 0
	val num_lkind_single = ref 0

	val depth_lcon_prim = ref 0
	val depth_lcon_import = ref 0
	val depth_lcon_conb = ref 0
	val depth_lcon_concb = ref 0
        val depth_lcon_single = ref 0
	val depth_lcon_function = ref 0
	val depth_lkind_single = ref 0

	fun bumper(num,depth) = if (!depth > 0) then num := !num + 1 else ()
	fun inc n = n := !n + 1
	fun dec n = n := !n - 1
	    
	fun reset_state canBeOpen : state = (seen := VarSet.empty; 
					     num_renamed := 0;
					     num_var := 0;
					     num_lexp := 0;
					     num_lcon := 0;
					     num_lkind := 0;
					     num_lcon_prim := 0;
					     num_lcon_import := 0;
					     num_lcon_single := 0;
					     num_lcon_function := 0;
					     num_lcon_conb := 0;
					     num_lcon_concb := 0;
					     num_lkind_single := 0;
					     depth_lcon_prim := 0;
					     depth_lcon_single := 0;
					     depth_lcon_function := 0;
					     depth_lcon_conb := 0;
					     depth_lcon_concb := 0;
					     depth_lkind_single := 0;
					     (canBeOpen,VarMap.empty))

	fun state_stat str ((canBeOpen,m) : state) : unit = 
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

	fun find_var ((canBeOpen,s) : state,v : var) : var = 
	     case (VarMap.find(s,v)) of
		 NONE => if canBeOpen
			     then v
			 else error ("find_var failed on " ^ (Name.var2string v))
	       | SOME v' => v'

	    
	fun add_var ((canBeOpen,m) : state, v : var) : state * var = 
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
	    in  ((canBeOpen,m),v')
	    end



    end


    fun small_con con =
	case con of 
	    Var_c _ => true
	  | Prim_c (primcon, clist) => length clist = 0 
	  | _ => false

    fun small_exp exp =
	case exp of 
	    Var_e _ => true
          | Const_e (Prim.int (Prim.W64,_)) => false
          | Const_e (Prim.uint (Prim.W64,_)) => false
          | Const_e (Prim.int _) => true
          | Const_e (Prim.uint _) => true
	  | _ => false

    fun lvalue lift state value = 
      (case value 
	 of Prim.array (con, arr) => error "Arrays shouldn't ever show up"
	  | Prim.vector (con, arr) => 
	   let
	     val (cbnds, con) = lcon lift state con
	     val cbnds  = (map (fn cb => Con_b(Runtime,cb)) cbnds)
	     fun folder (exp,(es,bnds_list)) = 
	       let val (bnds,exp) = lexp lift state exp
	       in (exp::es,bnds@bnds_list)
	       end
	     val (exps,bnds) = Array.foldr folder ([],[]) arr
	     val arr = Array.fromList exps
	   in
	     (cbnds @ bnds,Prim.vector (con,arr))
	   end
	  | Prim.refcell r => error "Ref cells shouldn't ever show up"
	  | Prim.tag (t,con) => let val (cbnds,con) = lcon lift state con
				    val cbnds  = (map (fn cb => Con_b(Runtime,cb)) cbnds)
				in (cbnds,Prim.tag (t,con)) end
	  | _ => ([],value))

   and lswitch lift state switch = 
     (case switch of
	  Intsw_e {size,arg,arms,default,result_type} =>
	      let val arg = lexp_lift' state arg
		  val result_type = lcon_flat state result_type
		  val arms = map (fn (w,e) => (w,lexp_lift' state e)) arms
		  val default = Util.mapopt (lexp_lift' state) default
	      in  Intsw_e {size=size,arg=arg,arms=arms,default=default,
			   result_type=result_type}
	      end
	| Sumsw_e {sumtype,arg,bound,arms,default,result_type} =>
	      let val sumtype = lcon_flat state sumtype
		  val result_type = lcon_flat state result_type
		  val arg = lexp_lift' state arg
		  val (state,bound) = add_var(state,bound)
		  val arms = map (fn (t,tr,e) => (t,tr,lexp_lift' state e)) arms
		  val default = Util.mapopt (lexp_lift' state) default
	      in  Sumsw_e {sumtype=sumtype,arg=arg,
			   bound=bound,arms=arms,default=default,
			   result_type=result_type}
	      end
	| Exncase_e {arg,bound,arms,default,result_type} =>
	      let 
		  val arg = lexp_lift' state arg
		  val result_type = lcon_flat state result_type
		  val (state,bound) = add_var(state,bound)
		  val arms = map (fn (e1,trace,e2) => (lexp_lift' state e1, trace, lexp_lift' state e2)) arms
		  val default = Util.mapopt (lexp_lift' state) default
	      in  Exncase_e {arg=arg,
			     bound=bound,arms=arms,default=default,
			     result_type=result_type}
	      end
	| Typecase_e {arg,arms,default,result_type} =>
	      let val result_type = lcon_flat state result_type
		  val arg = lcon_flat state arg
		  val default = lexp_lift' state default
		  val arms = map (fn (pc,vklist,e) => 
				  let val (vklist,state) = lvklist state vklist
				  in  (pc, vklist, lexp_lift' state e)
				  end) arms
	      in  Typecase_e {arg=arg, arms=arms, default=default, result_type=result_type}
	      end)

   and lbnd state arg_bnd : bnd list * state =
       let 
	   fun add_vars state vx_list = foldl (fn ((v,_),s) => #1(add_var(s,v))) state vx_list
	   fun vf_help wrapper vf_set = 
	       let val vf_list = sequence2list vf_set
		   val newstate = add_vars state vf_list
		   val vf_list = map (fn (v,f) => (find_var(newstate,v),
						   lfunction newstate f)) vf_list
	       in  ([wrapper (list2sequence vf_list)], newstate)
	       end
	   fun vcl_help state (v,{code,cenv,venv,tipe}) = 
	       let val v = find_var(state,v)
		   val cenv' = lcon_flat state cenv
		   val tipe' = lcon_flat state tipe
		   val venv' = lexp_lift' state venv
		   val code' = find_var(state,code)
	       in  (v,{code=code',cenv=cenv',venv=venv',tipe=tipe'})
	       end
	   fun mapset f s = list2sequence(map f (sequence2list s))

       in  (case arg_bnd of
		Con_b (p,cb) => let val _ = inc depth_lcon_conb
				    val (cbnds,state) = lcbnd true state cb
				    val _ = dec depth_lcon_conb
				in  (map (fn cb => Con_b(p,cb)) cbnds,state)
				end
	      | Exp_b (v,niltrace,e) => let val (bnds,e) = lexp true state e
				   val (state,v) = add_var(state,v)
			       in  (bnds @ [Exp_b(v,niltrace,e)], state)
			       end
	      | Fixopen_b vf_set => vf_help Fixopen_b vf_set
	      | Fixcode_b vf_set => vf_help Fixcode_b vf_set
(* RECURSIVE BINDING *)
	      | Fixclosure_b (flag,vcl_set) => 
				 let val vcl_list = sequence2list vcl_set
				     val state = add_vars state vcl_list
				     val vcl_list = map (vcl_help state) vcl_list
				     val fixbnd = Fixclosure_b(flag,list2sequence vcl_list)
				 in  ([fixbnd], state)
				 end)
       end

   and lfunction state (Function{effect,recursive,isDependent,
				 tFormals,eFormals,fFormals,body,body_type}) : function =
       let 
	   val (tFormals,state) = lvklist state tFormals
	   val _ = inc depth_lcon_function
	   val (eFormals,state) = lvtrclist_flat state eFormals
	   val _ = dec depth_lcon_function
	   fun vfolder(v,state) = 
	       let val (state,v) = add_var(state,v)
	       in (v,state)
	       end
	   val (fFormals,state) = foldl_acc vfolder state fFormals
	   val body = lexp_lift' state body
	   val body_type = lcon_lift' state body_type
       in  Function{effect=effect,recursive=recursive,isDependent=isDependent,
		    tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
		    body=body,body_type=body_type}
       end


   and lexp lift state arg_exp : bnd list * exp =
       let val (bnds,e) = lexp' lift state arg_exp
       in  if (small_exp e orelse not (!linearize) orelse not lift)
	       then (bnds, e)
	   else
	       let 
		   val v = 
		       (case e of
			    Prim_e(NilPrimOp (select l), _, [Var_e v]) =>
				let
				    val vname = Name.var2name v
				    val lname = Name.label2name l
				in
				    if ((String.size vname = 0) orelse
					(String.sub(vname,0) = #"_")) then
				     Name.fresh_named_var (Name.label2name l)
				 else
				     Name.fresh_named_var
				     ((Name.var2name v) ^ "_" ^ 
				      (Name.label2name l))
				end
			  | _ => Name.fresh_named_var "" (* "tmpexp" *))
	       in  (bnds @ [Exp_b(v,TraceUnknown,e)], Var_e v)
	       end
       end
       handle e => (print "exception in lexp call with con =\n";
		    pp_exp arg_exp; print "\n"; raise e)


   and lexp' lift state arg_exp : bnd list * exp =
	let val _ = num_lexp := !num_lexp + 1;
	    val self = lexp lift state
	in  case arg_exp of
	    Var_e v => (num_var := !num_var + 1; 
			([],Var_e(find_var(state,v))))
	  | Const_e value => 
	      let
		val (bnds,value) = lvalue lift state value
	      in
		(bnds,Const_e value)
	      end
	  | Let_e (sort,bnds,e) => 
		let fun folder (bnd,s) = lbnd s bnd
		    val (bnds,state) = foldl_acc folder state bnds
		    val bnds = flatten bnds
		    val (bnds',e) = lexp lift state e
		in  if lift
			then (bnds @ bnds', e)
		    else ([], NilUtil.makeLetE Sequential (bnds@bnds') e)
		end
	  | Prim_e (ap,clist,elist) =>
		let val _ = inc depth_lcon_prim
		    (* val constr = Normalize.prim_uses_carg ap *)
		    val cbnds_clist = map (lcon lift state) clist
		    val (cbnds,clist) = Listops.unzip cbnds_clist
		    val cbnds = flatten cbnds 
		    val _ = dec depth_lcon_prim
		    val (bnds,elist) = map_unzip (lexp lift state) elist
		    val bnds = flatten bnds
		in  ((map (fn cb => Con_b(Runtime,cb)) cbnds) @ bnds,
		     Prim_e(ap,clist,elist))
		end
	  | ExternApp_e (f,elist) =>
		let val (bnds,f) = lexp lift state f
		    val (bnds',elist) = map_unzip (lexp lift state) elist
		in  (bnds @ flatten bnds', ExternApp_e (f,elist))
		end
	  | App_e (openness,f,clist,elist,flist) =>
		let val (bnds,f) = lexp lift state f
		    val (cbnds,clist) = map_unzip (lcon lift state) clist
		    val (bnds',elist) = map_unzip (lexp lift state) elist
		    val (bnds'',flist) = map_unzip (lexp lift state) flist
		in  (bnds @ (flatten((mapmap (fn cb => Con_b(Runtime,cb)) cbnds) @ 
				      bnds' @ bnds'')), 
		     App_e (openness,f,clist,elist,flist))
		end
	  | Raise_e (e,c) => 
		let val (bnds,e) = lexp lift state e
		    val c = lcon_flat state c
		in  (bnds,Raise_e(e,c))
		end
	  | Switch_e switch => ([], Switch_e(lswitch lift state switch))
	  | Handle_e {body,bound,handler,result_type} => 
		let val body = lexp_lift' state body
		    val (state,bound) = add_var(state,bound)
		    val handler = lexp_lift' state handler
		    val result_type = lcon_flat state result_type
		in 
		    ([],Handle_e{body = body, bound = bound,
				 handler = handler, result_type = result_type})
		end
	end


   and lcbnd lift state arg_cbnd : conbnd list * state =
       let val _ = state_stat "lcbnd" state
	   fun lconfun wrapper (v,vklist,c) = 
	   let val (vklist,state) = lvklist state vklist
	       val c = lcon_flat state c
	       val (state,v) = add_var(state,v)
	       val cbnd = wrapper(v,vklist,c)
	       val _ = state_stat "lcbnd: lconfun" state
	   in  ([cbnd], state)
	   end
       in (case arg_cbnd of
	       Con_cb (v,c) => let val _ = inc depth_lcon_concb
				   val (cbnds,c) = lcon lift state c
				   val _ = dec depth_lcon_concb
				   val (state,v) = add_var(state,v)
			       in  (cbnds @ [Con_cb(v,c)], state)
			       end
	     | Open_cb arg => lconfun Open_cb arg
	     | Code_cb arg => lconfun Code_cb arg)
       end

   and lcbnds lift state cbnds : conbnd list * state = 
       let val (cbnds,state) = foldl_acc (fn (cbnd,s) => lcbnd lift s cbnd) state cbnds
       in  (flatten cbnds, state)
       end

   and lcon_lift state arg_con : conbnd list * con = lcon true state arg_con
   and lcon_lift' state arg_con : con = 
       let val (cbnds,c) = lcon true state arg_con
       in  (case cbnds of
		[] => c
	      | _ => Let_c(Sequential,cbnds,c))
       end
   and lcon_flat state arg_con : con  = 
	let val (cbnds,c) = lcon false state arg_con
	    val _ = (case cbnds of
			 [] => ()
		       | _ => (print "lcon_flat got non-empty cbnds...";
			       Ppnil.pp_con arg_con;
			       print "\n";
			       Ppnil.pp_con (Let_c(Sequential,cbnds,c));
			       print "\n";
			       error "lcon_flat got non-empty cbnds"))

	in  c
	end

   and lexp_lift state arg_exp : bnd list * exp = lexp true state arg_exp
   and lexp_lift' state arg_exp : exp = 
       let val (bnds,e) = lexp true state arg_exp
       in  NilUtil.makeLetE Sequential bnds e
       end
   and lexp_flat state arg_exp : exp  = 
	let val (bnds,e) = lexp false state arg_exp
	    val _ = (case bnds of
			 [] => ()
		       | _ => (print "lexp_flat got non-empty bnds...";
			       Ppnil.pp_exp arg_exp;
			       print "\n";
			       Ppnil.pp_exp (Let_e(Sequential,bnds,e));
			       print "\n";
			       error "lexp_flat got non-empty bnds"))
	in  e
	end

   and lcon lift state arg_con : conbnd list * con  = 
       let val (cbnds,c) = lcon' lift state arg_con
       in  if (small_con c orelse not (!linearize) orelse not lift)
	       then (cbnds, c)
	   else let val v = Name.fresh_named_var "type"
		in  (cbnds @ [Con_cb(v,c)], Var_c v)
		end
       end
       handle e => (print "exception in lcon call with con =\n";
		    pp_con arg_con; print "\n"; raise e)

   and lcon' lift state arg_con : conbnd list * con  = 
       let val local_lcon = lcon lift
	   val _ = (inc num_lcon;
		    bumper(num_lcon_prim, depth_lcon_prim);
		    bumper(num_lcon_import, depth_lcon_import);
		    bumper(num_lcon_single, depth_lcon_single);
		    bumper(num_lcon_function, depth_lcon_function);
		    bumper(num_lcon_conb, depth_lcon_conb);
		    bumper(num_lcon_concb, depth_lcon_concb))

       in
	case arg_con of
	    Var_c v => (num_var := !num_var + 1; 
			([],Var_c(find_var(state,v))))
	  (* dependent records *)
	  | Prim_c (Record_c(labs,SOME vars),cons) => 
		let fun folder ((v,c),state) = 
		    let val (cbnds,c) = lcon false state c
			val (state,v) = add_var(state,v)
		    in  ((cbnds,(v,c)),state)
		    end
		    val (cbnds_vc,_) = foldl_acc folder state (Listops.zip vars cons)
		    val (cbnds,vc) = Listops.unzip cbnds_vc
		in  (flatten cbnds,
		     Prim_c(Record_c(labs,SOME (map #1 vc)), map #2 vc))
		end
	  | Prim_c (pc,cons) => 
                let val (cbnds,cons) = map_unzip (local_lcon state) cons
                in  (flatten cbnds, Prim_c(pc,cons))
                end
	  | Mu_c (flag,vc_seq) => (* cannot just use lvclist here: 
				   not sequential bindings *)
		let val state = Sequence.foldl (fn ((v,_),s) => #1(add_var(s,v))) state vc_seq
		    val vc_seq' = Sequence.map (fn (v,c) => (derived_var v, c)) vc_seq
		    val vc_seq' = Sequence.map (fn (v,c) => (v, lcon_flat state c)) vc_seq'
		    val vc_seq'' = Sequence.map2 (fn ((v,_),(_,c)) => (find_var(state,v),c)) 
			         (vc_seq,vc_seq')
		in  ([],Mu_c(flag,vc_seq''))
		end
	  | ExternArrow_c (clist,c) =>
		let 
		    val (cbnds,clist) = Listops.unzip(map (local_lcon state) clist)
		    val (cbnds',c) = local_lcon state c
		in  (flatten cbnds@cbnds',ExternArrow_c (clist,c))
		end
	  | AllArrow_c {openness,effect,isDependent,tFormals,eFormals,fFormals,body_type} =>
	      let 
		  val (tFormals,state) = lvklist state tFormals
		  val (eFormals,state) = lvoptclist state eFormals
		  val body_type = lcon_lift' state body_type
	      in  ([],
		   AllArrow_c {openness=openness,effect=effect,isDependent=isDependent,
			       tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
			       body_type=body_type})
	      end
	  | Let_c (letsort,cbnds,c) =>
		let 
		    val (cbnds,state) = lcbnds lift state cbnds
		    val _ = state_stat "let_c after fold" state
		    val (cbnds',c) = local_lcon state c
		in  if lift 
			then (cbnds @ cbnds', c)
		    else ([],Let_c(Sequential, cbnds @ cbnds', c))
		end
	  | Crecord_c lc_list => 
		let fun doer(l,c) = let val (cbnds,c) = local_lcon state c
				    in  (cbnds, (l,c))
				    end
		    val (cbnds, lc_list) = Listops.unzip (map doer lc_list)
		in  (flatten cbnds, Crecord_c lc_list)
		end

	  | Proj_c (c,l) => let val (cbnds, c) = local_lcon state c
			    in  (cbnds, Proj_c(c,l))
			    end

	  | Typeof_c e => ([],Typeof_c(lexp_flat state e))

	  | Closure_c (c1,c2) => let val (cbnds1,c1) = local_lcon state c1
				     val (cbnds2,c2) = local_lcon state c2
				 in  (cbnds1@cbnds2,Closure_c(c1,c2))
				 end
	  | App_c (c,clist) => let val (cbnds,c) = local_lcon state c
				   val temp = map (local_lcon state) clist
				   val (cbnds',clist) = Listops.unzip temp
			       in  (cbnds@flatten cbnds', App_c(c,clist))
			       end
          | Typecase_c _ => error "typecase not done"

	  | Annotate_c (a,c) => let val (cbnds,c) = local_lcon state c
				in  (cbnds,Annotate_c(a,c))
				end
       end

   and lvk state (v,k) = 
	   let val k = lkind state k
	       val (state,v) = add_var(state,v)
	   in  ((v,k), state)
	   end


   and lvklist state vklist = 
       let fun vkfolder((v,k),(acc,state)) = 
	   let val k = lkind state k
	       val (state,v) = add_var(state,v)
	   in  ((v,k)::acc, state)
	   end
	   val (rev_vklist,state) = foldl vkfolder ([],state) vklist
       in  (rev rev_vklist, state)
       end

   and lvc lift state (v,c) : (conbnd list * (var * con)) * state = 
	   let val (cbnds,c) = lcon lift state c
	       val (state,v) = add_var(state,v)
	   in  ((cbnds,(v,c)), state)
	   end

   and lvclist state vclist = 
       let val (temp,state) = foldl_acc (fn (vc,s) => (lvc true s vc)) state vclist
       in  (flatten(map #1 temp), map #2 temp, state)
       end

   and lvtrclist_flat state vtrclist = 
       let fun vtrcfolder((v,tr,c),state) =
	   let val _ = inc depth_lcon_function
	       val c = lcon_flat state c
               val _ = dec depth_lcon_function
	       val (state,v) = add_var(state,v)
	   in  ((v,tr,c), state)
	   end
       in  foldl_acc vtrcfolder state vtrclist
       end

   and lvoptclist state vclist = 
       let fun vcfolder((vopt,c),state) =
	   let val _ = inc depth_lcon_function
	       val c = lcon_lift' state c
               val _ = dec depth_lcon_function
	       val (state,vopt) = (case vopt of
				       SOME v => let val (s,v) = add_var(state,v)
						 in  (s, SOME v)
						 end
				     | NONE => (state, vopt))
	   in  ((vopt,c), state)
	   end
	   val (vopt_c,state) = foldl_acc vcfolder state vclist
       in  (vopt_c, state)
       end

   and lkind state arg_kind : kind = 
       ((lkind' state arg_kind)
       handle e => (print "exception in lkind call with kind =\n";
		    pp_kind arg_kind; print "\n"; raise e))

   and lkind' state arg_kind : kind = 
       (inc num_lkind;
	bumper(num_lkind_single, depth_lkind_single);

	case arg_kind of
	    Type_k => arg_kind
	  | SingleType_k c => let val _ = inc depth_lcon_single
				  val c = lcon_flat state c
				  val _ = dec depth_lcon_single
			      in  SingleType_k c
			      end
	  | Single_k c => let val _ = inc depth_lcon_single
			      val c = lcon_flat state c
			      val _ = dec depth_lcon_single
			  in  Single_k c
			  end
	  | Record_k lvk_seq => 
		let fun folder (((l,v),k),state) = 
			let val ((v,k),state) = lvk state (v,k)
			in  (((l,v),k),state)
			end
		in  Record_k (#1(Sequence.foldl_acc folder state lvk_seq))
		end
	  | Arrow_k(openness,vklist,k) => let val (vklist,state) = lvklist state vklist
					      val k = lkind state k
					  in  Arrow_k(openness,vklist,k)
					  end)


   fun lexport state (ExportValue(l,v)) = ExportValue(l,find_var(state,v))
     | lexport state (ExportType(l,v)) = ExportType(l,find_var(state,v))


   fun limport (ImportValue(l,v,tr,c),s) =
       let val (s,v) = add_var(s,v)
	   val _ = inc depth_lcon_import
	   val c = lcon_flat s c
	   val _ = dec depth_lcon_import
       in  (ImportValue(l,v,tr,c),s)
	   handle e => (print "exception in limport call\n";
			raise e)
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
       
   fun linearize_exp e = 
       let (* Permit expression to be open *)
	   val state = reset_state true
	   val e = lexp_lift' state e
	   val _ = reset_state false
       in  e
       end

   fun linearize_mod (MODULE{bnds,imports,exports}) = 
       let (* Module must be closed *)
	   val state = reset_state false
	   val (imports,state) = limports(imports,state)
	   fun folder (bnd,state) = lbnd state bnd
	   val (bnds,state) = foldl_acc folder state bnds
	   val bnds = flatten bnds
	   val exports = map (lexport state) exports
	   val _ = if (!show_stats)
		       then (print "  Number of renamed variables: ";
			     print (Int.toString (!num_renamed)); print "\n";
			     print "  Number of variables: ";
			     print (Int.toString (!num_var)); print "\n";
			     print "  Number of lexp calls: ";
			     print (Int.toString (!num_lexp)); print "\n";
			     print "  Number of lcon calls: ";
			     print (Int.toString (!num_lcon)); print "\n";
			     
			     print "    Number of lcon calls from Prim_e: ";
			     print (Int.toString (!num_lcon_prim)); print "\n";
			     print "    Number of lcon calls from Import: ";
			     print (Int.toString (!num_lcon_import)); print "\n";
			     print "    Number of lcon calls from Singletons: ";
			     print (Int.toString (!num_lcon_single)); print "\n";
			     print "    Number of lcon calls from Function formals: ";
			     print (Int.toString (!num_lcon_function)); print "\n";
			     print "    Number of lcon calls from Con_b: ";
			     print (Int.toString (!num_lcon_conb)); print "\n";
			     print "    Number of lcon calls from Con_cb: ";
			     print (Int.toString (!num_lcon_concb)); print "\n";
			     
			     print "  Number of lkind calls: ";
			     print (Int.toString (!num_lkind)); print "\n")
		   else ()
	   val _ = reset_state false

       in  MODULE{bnds = bnds,
		  imports = imports,
		  exports = exports}
       end

end
