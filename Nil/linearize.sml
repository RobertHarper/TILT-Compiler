(*
 Tools for putting Nil code into A-normal form
*)

structure Linearize
    :> LINEARIZE =
struct

    val error = fn s => Util.error "linearize.sml" s
    val show_stats = Stats.ff("LinearizeStats")
    val linearize = Stats.tt("Linearize")
    val linearize_cse = Stats.tt("LinearizeCSE")
    val debug = ref false

    open Nil

    val foldl_acc = Listops.foldl_acc
    val flatten = Listops.flatten
    val mapmap = Listops.mapmap

    val small_con = NilDefs.small_con
    val small_exp = NilDefs.small_exp

    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    structure ConMap = ExpTable.Conmap

    val derived_var = Name.derived_var

    val pp_kind = Ppnil.pp_kind
    val pp_con = Ppnil.pp_con
    val pp_exp = Ppnil.pp_exp

    val list2sequence = Sequence.fromList
    val sequence2list = Sequence.toList

    fun map_unzip f ls = Listops.unzip(map f ls)
    val unzip = Listops.unzip
    val zip = Listops.zip

    (* Code for handling renaming variables with fresh names and keeping track of various translation statistics *)
    local
	type state = bool * var VarMap.map * var ConMap.map
	(* 1: whether or not unbound variables are allowed in input
	   2: mapping of variables in input to different variables in output 
	   3: mapping from constructors to variables binding them *)

	val seen : VarSet.set ref = ref VarSet.empty
        (* variable names that have been encountered in the input code thus far *)
    in
	type state = state

        (* References for keeping translation statistics *)

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

        (*
	 val reset_state : bool -> state
	 reset_state canBeOpen ==> a new state that allows unbound variables iff canBeOpen
	 Effects: Resets seen and statistics references
	*)
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
					     (canBeOpen,VarMap.empty,ConMap.empty))

        (* debug routine to print information on a state *)
	fun state_stat str ((canBeOpen,m,eqn) : state) : unit =
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

	(*
	 val find_var : state * var -> var
	 find_var (state, v) ==> replacement variable for v in state
	 Effects: Error if an unbound variable is requested for a state which does not allow unbound variables
        *)
	fun find_var ((canBeOpen,s,eqn) : state,v : var) : var =
	     case (VarMap.find(s,v)) of
		 NONE => if canBeOpen
			     then v
			 else error ("find_var failed on " ^ (Name.var2string v))
	       | SOME v' => v'

        (*
	 val add_var : state * var -> state * var
	 add_var (state, v) ==> (state modified to replace v with a fresh variable,
	                         the fresh variable chosen)
	 Effects: Can modify statistics on number of renamed variables
        *)
	fun add_var ((canBeOpen,m,eqn) : state, v : var) : state * var =
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
	    in  ((canBeOpen,m,eqn),v')
	    end

      (*
	 val replace_var : state * var * var -> state 
	 replace_var (state, v, v') ==> (state modified to replace v with v'.  used for CSE)
        *)
	fun replace_var ((canBeOpen,m,eqn) : state, v : var, v' : var) : state =
	    let 
	      val _ = seen := VarSet.add(!seen,v)
	      val m = (case (VarMap.find(m,v)) of
			 NONE => VarMap.insert(m,v,v')
		       | SOME _ => VarMap.insert(#1(VarMap.remove(m,v)),v,v'))
	    in  (canBeOpen,m,eqn)
	    end

      (*
	 val add_availC : state * var * con -> state 
	 add_availC (state, v, c) ==> (state modified to indicate that v is bound to c.  used for CSE)
        *)
	fun add_availC ((canBeOpen,m,eqn) : state, v : var, c : con) : state =
	    let 
	      val eqn = ConMap.insert(eqn,c,v)
	    in  (canBeOpen,m,eqn)
	    end

      (*
	 val find_availC : state * con -> var option
	 find_availC (state, c) ==> SOME v if v is available and bound to c.  used for CSE)
        *)
	fun find_availC ((canBeOpen,m,eqn) : state, c : con) : var option = ConMap.find(eqn,c)



    end


    (*
     Most of the translation functions below take a boolean "lift" parameter. This ensures that Let's in the entity to be
     translated and its component parts have their bindings "lifted" to the list of extra bindings generated as part of
     A-normalizing, instead of translating them into new Let's. It also guarantees that, where possible,
     (sub)expressions and (sub)constructors are translated as small expressions/constructors or Let's with small
     expression/constructor bodies.
    *)


    (*
      val lvalue : bool -> state -> value -> bnd list * value
	  lvalue lift state value ==> (bnds, value') such that Let bnds In value' End is an A-normal form equivalent to value,
	      using the variable renamings from state
	  Effects: Error if value is an array or ref
    *)
    fun lvalue lift state value =
      (case value
	 of Prim.array (con, arr) => error "Arrays shouldn't ever show up"
	  | Prim.vector (con, arr) =>
	   let
	     val (state, cbnds, con) = lcon lift state con
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
	  | Prim.tag (t,con) => let val (state,cbnds,con) = lcon lift state con
				    val cbnds  = (map (fn cb => Con_b(Runtime,cb)) cbnds)
				in (cbnds,Prim.tag (t,con)) end
	  | _ => ([],value))


   (*
    val lswitch : bool -> state -> switch -> switch
    lswitch lift state switch ==> switch with its component constructors and terms A-normalized using state
   *)
   and lswitch lift state switch =
     (case switch of
	  Intsw_e {size,arg,arms,default,result_type} =>
	      let val (bnds,arg) = lexp lift state arg
		  val result_type = lcon_flat state result_type
		  val arms = map (fn (w,e) => (w,lexp_lift' state e)) arms
		  val default = Util.mapopt (lexp_lift' state) default
	      in  (bnds,Intsw_e {size=size,arg=arg,arms=arms,default=default,
				 result_type=result_type})
	      end
	| Sumsw_e {sumtype,arg,bound,arms,default,result_type} =>
	      let val sumtype = lcon_flat state sumtype
		  val result_type = lcon_flat state result_type
		  val (bnds,arg) = lexp lift state arg
		  val (state,bound) = add_var(state,bound)
		  val arms = map (fn (t,tr,e) => (t,tr,lexp_lift' state e)) arms
		  val default = Util.mapopt (lexp_lift' state) default
	      in (bnds,Sumsw_e {sumtype=sumtype,arg=arg,
				bound=bound,arms=arms,default=default,
				result_type=result_type})
	      end
	| Exncase_e {arg,bound,arms,default,result_type} =>
	      let
		  val (bnds,arg) = lexp lift state arg
		  val result_type = lcon_flat state result_type
		  val (state,bound) = add_var(state,bound)
		  val arms = map (fn (e1,trace,e2) => (lexp_lift' state e1, trace, lexp_lift' state e2)) arms
		  val default = Util.mapopt (lexp_lift' state) default
	      in  (bnds,Exncase_e {arg=arg,
				   bound=bound,arms=arms,default=default,
				   result_type=result_type})
	      end
	| Typecase_e {arg,arms,default,result_type} =>
	      let val result_type = lcon_flat state result_type
		  val arg = lcon_flat state arg
		  val default = lexp_lift' state default
		  val arms = map (fn (pc,vklist,e) =>
				  let val (vklist,state) = lvklist state vklist
				  in  (pc, vklist, lexp_lift' state e)
				  end) arms
	      in  ([],Typecase_e {arg=arg, arms=arms, default=default, result_type=result_type})
	      end
	| Ifthenelse_e _ => error "Ifthenelse not implemented yet")

   (*
    val lbnd : state -> bnd -> bnd list * state
    lbnd state bnd ==> (bnds, state'), where bnds is a sequence of A-normal form bindings equivalent to bnd.
	Uses state's variable renamings, with state' obtained from state to include new variables encountered
   *)
   and lbnd state arg_bnd : bnd list * state =
       let
	   fun add_vars state vx_list = foldl (fn (((v,_),_),s) => #1(add_var(s,v))) state vx_list

	   (* Open/Code fold function *)
	   fun vf_help wrapper vf_set =
	       let val vf_list = sequence2list vf_set
		   val newstate = add_vars state vf_list
		   val vf_list = map (fn ((v,c),f) =>
				      let
					  val c = lcon_flat state c
					  val f = lfunction newstate f
				      in
					  ((find_var(newstate,v),c),f)
				      end) vf_list
	       in  ([wrapper (list2sequence vf_list)], newstate)
	       end

           (* Closure fold function *)
	   fun vcl_help state ((v,c),{code,cenv,venv}) =
	       let val v = find_var(state,v)
		   val c' = lcon_flat state c
		   val cenv' = lcon_flat state cenv
		   val venv' = lexp_lift' state venv
		   val code' = find_var(state,code)
	       in  ((v,c),{code=code',cenv=cenv',venv=venv'})
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

   (*
    val lfunction : state -> function -> function
    lfunction state func ==> func with its component parts A-normalized, using state
   *)
   and lfunction state (Function{effect,recursive,
				 tFormals,eFormals,fFormals,body}) : function =
       let
	   val (tFormals,state) = lvlist state tFormals
	   val _ = inc depth_lcon_function
	   val (eFormals,state) = lvtrlist_flat state eFormals
	   val _ = dec depth_lcon_function
	   fun vfolder(v,state) =
	       let val (state,v) = add_var(state,v)
	       in (v,state)
	       end
	   val (fFormals,state) = foldl_acc vfolder state fFormals
	   val body = lexp_lift' state body
       in  Function{effect=effect,recursive=recursive,
		    tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
		    body=body}
       end


   (*
    val lexp : bool -> state -> exp -> bnd list * exp
    lexp lift state exp ==> (bnds, exp') such that Let bnds In exp' End is an A-normal equivalent of exp, using state.
   *)
    and lexp lift state arg_exp : bnd list * exp =
      let val (bnds,e) = lexp' lift state arg_exp
       in  if (small_exp e orelse not (!linearize) orelse not lift)
	       then (bnds, e)
	   else
	       let
		   val v =
		       (case e of
			    Prim_e(NilPrimOp (select l),_, _, [Var_e v]) =>
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
	       in
		   (bnds @ [Exp_b(v,TraceUnknown,e)], Var_e v)
	       end
       end
       handle e => (print "exception in lexp call with con =\n";
		    pp_exp arg_exp; print "\n"; raise e)


   (*
    val lexp': bool -> state -> exp -> bnd list * exp
    lexp' lift state exp ==> (bnds, exp') such that Let bnds In exp' End is an A-normal equivalent of exp, using state.
	Does not guarantee that exp' is small.
   *)
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
	  | Prim_e (ap,trs,clist,elist) =>
		let val _ = inc depth_lcon_prim
		    val (state,cbnds,clist) = lcon_list lift state clist
		    val _ = dec depth_lcon_prim
		    val (bnds,elist) = map_unzip (lexp lift state) elist
		    val bnds = flatten bnds
		in  ((map (fn cb => Con_b(Runtime,cb)) cbnds) @ bnds,
		     Prim_e(ap,trs,clist,elist))
		end
	  | ExternApp_e (f,elist) =>
		let val (bnds,f) = lexp lift state f
		    val (bnds',elist) = map_unzip (lexp lift state) elist
		in  (bnds @ flatten bnds', ExternApp_e (f,elist))
		end
	  | App_e (openness,f,clist,elist,flist) =>
		let val (bnds,f) = lexp lift state f
		    val (state,cbnds,clist) = lcon_list lift state clist
		    val (bnds',elist) = map_unzip (lexp lift state) elist
		    val (bnds'',flist) = map_unzip (lexp lift state) flist
		in  (bnds @ (map (fn cb => Con_b(Runtime,cb)) cbnds) @ (flatten (bnds' @ bnds'')),
		     App_e (openness,f,clist,elist,flist))
		end
	  | Raise_e (e,c) =>
		let val (bnds,e) = lexp lift state e
		    val c = lcon_flat state c
		in  (bnds,Raise_e(e,c))
		end
	  | Switch_e switch => let val (bnds,switch) = lswitch lift state switch
			       in (bnds,Switch_e switch)
			       end
	  | Handle_e {body,bound,handler,result_type} =>
		let val body = lexp_lift' state body
		    val (state,bound) = add_var(state,bound)
		    val handler = lexp_lift' state handler
		    val result_type = lcon_flat state result_type
		in
		    ([],Handle_e{body = body, bound = bound,
				 handler = handler, result_type = result_type})
		end
	  | Coerce_e (ccn,cons,exp) =>
		let
		  val (bnds,ccn) = lexp lift state ccn
		  val (state,cbnds,cons) = lcon_list lift state cons
		  val (bnds',exp) = lexp lift state exp
		in (bnds @
		    (map (fn cb => Con_b(Runtime,cb)) cbnds) @
		    bnds',
		    Coerce_e (ccn,cons,exp))
		end
	  | ForgetKnown_e (sumcon,which) =>
	    let
	      val (state,cbnds,sumcon) = lcon lift state sumcon
	      val bnds = map (fn cb => (Con_b(Runtime,cb))) cbnds
	    in
	      (bnds, ForgetKnown_e (sumcon,which))
	    end
	  | Fold_e (vars,from,to) =>
	    let
		fun folder (v,s) = let val (s,v) = add_var (s,v) in s end
		val state = foldl folder state vars
		val from = lcon_lift' state from
		val to = lcon_lift' state to
	    in
		([], Fold_e (vars,from,to))
	    end
	  | Unfold_e (vars,from,to) =>
	    let
		fun folder (v,s) = let val (s,v) = add_var (s,v) in s end
		val state = foldl folder state vars
		val from = lcon_lift' state from
		val to = lcon_lift' state to
	    in
		([],Unfold_e (vars,from,to))
	    end
	end


   (*
    val lcbnd : bool -> state -> conbnd -> conbnd list * state
    lcbnd lift state cbnd ==> (cbnds, state'), such that cbnds are an A-normalized version of cbnd, translated using state
        and updating state' from state to include new variables encountered
   *)
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
				   val (state,cbnds,c) = lcon lift state c
				   val _ = dec depth_lcon_concb
			       in
				 if !linearize_cse then
				   let
				     val c =
				       (case find_availC(state,c) of
					  NONE => c
					| SOME v' => Var_c v')
				     val (cbnds,state) = 
				       (case c of
					  Var_c newv => 
					    let 
					      val state = replace_var(state,v,newv)
					    in (cbnds,state)
					    end
					| _ => 
					    let 
					      val (state,v) = add_var (state,v)
					      val cbnds = cbnds @ [Con_cb(v,c)]
					      val state = add_availC(state, v, c)
					    in (cbnds,state)
					    end)
				   in (cbnds,state)
				   end
				 else 
				   let
				     val (state,v) = add_var (state,v)
				   in
				     (cbnds @ [Con_cb(v,c)], state)
				   end
			       end
	     | Open_cb arg => lconfun Open_cb arg
	     | Code_cb arg => lconfun Code_cb arg)
       end

   (*
    val lcbnds : bool * state * conbnd list -> conbnd list * state
    lcbnds lift state cbnds ==> (cbnds', state') such that cbnds' are an A-normalized version of cbnds, translated using state
	and updating state' from state to include new variables
   *)
   and lcbnds lift state cbnds : conbnd list * state =
       let val (cbnds,state) = foldl_acc (fn (cbnd,s) => lcbnd lift s cbnd) state cbnds
       in  (flatten cbnds, state)
       end

   (*
    val lcon_lift : state -> con -> conbnd list * con
    lcon_lift state con ==> (cbnds, con'), such that Let cbnds In con' End is an A-normal version of con, translated using state.
	The translation is performed in lift mode.
   *)
   and lcon_lift state arg_con : state * conbnd list * con = lcon true state arg_con

   (*
    val lcon_lift' : state -> con -> con
    lcon_lift' state con ==> an A-normal version of con, translated using state and in lift mode
   *)
   and lcon_lift' state arg_con : con =
       let val (_,cbnds,c) = lcon true state arg_con
       in  (case cbnds of
		[] => c
	      | _ => Let_c(Sequential,cbnds,c))
       end

   (*
    val lcon_flat : state -> con -> con
    lcon_flat state con ==> a non-lift translation of con into a single A-normal form constructor, using state
    Effects: Error if extra bindings are generated translating con
   *)
   and lcon_flat state arg_con : con  =
	let val (_,cbnds,c) = lcon false state arg_con
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

   (*
    val lexp_lift : state -> exp -> bnd list * exp
    lexp_lift state exp ==> a lift mode translation of exp, using state
   *)
   and lexp_lift state arg_exp : bnd list * exp = lexp true state arg_exp

   (*
    val lexp_lift' : state -> exp -> exp
    lexp_lift' state exp ==> a lift mode translation of exp, using state, returned as a single Let expression
   *)
   and lexp_lift' state arg_exp : exp =
       let val (bnds,e) = lexp true state arg_exp
       in  NilUtil.makeLetE Sequential bnds e
       end

   (*
    val lexp_flat : state -> exp -> exp
    lexp_flat state exp ==> a non-lift mode translation of exp, using state
    Effects: Error if extra bindings are generated translating exp
   *)
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

   (*
    val lcon : bool -> state -> con -> state * conbnd list * con
    lcon lift state con ==> (state', cbnds, con'), such that Let cbnds In con' End is an A-normal version of con, 
      translated with state, and state' holds bindings for cbnds.
   *)
   and lcon lift state arg_con : state * conbnd list * con  =
       let val (state,cbnds,c) = lcon' lift state arg_con
       in  if (small_con c orelse not (!linearize) orelse not lift)
	       then (state,cbnds, c)
	   else 
	     (case (!linearize_cse, find_availC (state,c))
		of (true,SOME v) => (state,cbnds,Var_c v)
		 | _ => 
		  let 
		    val v = Name.fresh_named_var "type"
		    val state = add_availC (state,v,c)
		  in  (state,cbnds @ [Con_cb(v,c)], Var_c v)
		  end)
       end
     handle e => (print "exception in lcon call with con =\n";
		  pp_con arg_con; print "\n"; raise e)

   (*
    val lcon': bool -> state -> con -> state * conbnd list * con
    lcon' lift state con ==> (cbnds, con'), such that Let cbnds In con' End is an A-normal version of con, translated with state.
	Does not guarantee that con' is small.
    Effects: Error if con is a Typecase_c
   *)
   and lcon' lift state arg_con : state * conbnd list * con  =
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
			(state,[],Var_c(find_var(state,v))))
	  | Prim_c (pc,cons) =>
                let val (state,cbnds,cons) = lcon_list lift state cons
                in  (state,cbnds, Prim_c(pc,cons))
                end
	  | Mu_c (flag,vc_seq) => (* cannot just use lvclist here:
				   not sequential bindings *)
		let val state = Sequence.foldl (fn ((v,_),s) => #1(add_var(s,v))) state vc_seq
		    val vc_seq' = Sequence.map (fn (v,c) => (derived_var v, c)) vc_seq
		    val vc_seq' = Sequence.map (fn (v,c) => (v, lcon_flat state c)) vc_seq'
		    val vc_seq'' = Sequence.map2 (fn ((v,_),(_,c)) => (find_var(state,v),c))
			         (vc_seq,vc_seq')
		in  (state,[],Mu_c(flag,vc_seq''))
		end
	  | ExternArrow_c (clist,c) =>
		let
		    val (state,cbnds,clist) = lcon_list lift state clist
		    val (state,cbnds',c) = local_lcon state c
		in  (state,cbnds@cbnds',ExternArrow_c (clist,c))
		end

	  (* This is a special case that turns out to be pretty important in practice.
	   * -leaf
	   *)
	  | AllArrow_c {openness,effect,tFormals = [],eFormals,fFormals,body_type} =>
	      let
		val (state,cbnds,eFormals) = lcon_list lift state eFormals
		val (state,cbnds',body_type) = local_lcon state body_type
		val cbnds = cbnds @ cbnds'
	      in  (state,
		   cbnds,
		   AllArrow_c {openness=openness,effect=effect,
			       tFormals=[],eFormals=eFormals,fFormals=fFormals,
			       body_type=body_type})
	      end
	  | AllArrow_c {openness,effect,tFormals,eFormals,fFormals,body_type} =>
	      let
		  val (tFormals,inner_state) = lvklist state tFormals
		  val eFormals = lclist_flat inner_state eFormals
		  val body_type = lcon_lift' inner_state body_type
	      in  (state,
		   [],
		   AllArrow_c {openness=openness,effect=effect,
			       tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
			       body_type=body_type})
	      end
	  | Let_c (letsort,cbnds,c) =>
		let
		    val (cbnds,inner_state) = lcbnds lift state cbnds
		    val _ = state_stat "let_c after fold" inner_state
		    val (inner_state,cbnds',c) = local_lcon inner_state c
		in  if lift
			then (inner_state,cbnds @ cbnds', c)
		    else (state,[],Let_c(Sequential, cbnds @ cbnds', c))
		end
	  | Crecord_c lc_list =>
		let 
		  val (llist,clist) = unzip lc_list
		  val (state,cbnds,clist) = lcon_list lift state clist
		  val lc_list = zip llist clist
		in  (state,cbnds, Crecord_c lc_list)
		end

	  | Proj_c (c,l) => let val (state,cbnds, c) = local_lcon state c
			    in  (state,cbnds, Proj_c(c,l))
			    end

	  | Closure_c (c1,c2) => let val (state,cbnds1,c1) = local_lcon state c1
				     val (state,cbnds2,c2) = local_lcon state c2
				 in  (state,cbnds1@cbnds2,Closure_c(c1,c2))
				 end
	  | App_c (c,clist) => let val (state,cbnds,c) = local_lcon state c
				   val (state,cbnds',clist) = lcon_list lift state clist
			       in  (state,cbnds@cbnds', App_c(c,clist))
			       end
	  | Coercion_c {vars = [],from,to} =>
	    let
		val (state,cbnds,from) = local_lcon state from
		val (state,cbnds',to) = local_lcon state to
	    in (state,cbnds@cbnds',Coercion_c{vars = [],from=from,to=to})
	    end
	  | Coercion_c {vars,from,to} =>
	    let
		fun folder (v,(state,vars)) = 
		  let
		    val (state,v) = add_var (state,v)
		  in (state, v::vars)
		  end
		val (inner_state, vars) = foldr folder (state, nil) vars
		val from = lcon_lift' inner_state from
		val to = lcon_lift' inner_state to
	    in (state,[],Coercion_c{vars=vars,from=from,to=to})
	    end
       end

   (*
    fun lvk : state -> var * kind -> (var * kind) * state
    lvk state (v, k) ==> ((v', k'), state'), such that v' is the renaming of v, k' is k A-normalized, and state' is state with
	information on v added
   *)
   and lvk state (v,k) =
	   let val k = lkind state k
	       val (state,v) = add_var(state,v)
	   in  ((v,k), state)
	   end

   (*
    fun lvklist : state -> (var * kind) list -> (var * kind) list * state
    lvklist state vklist ==> (vklist', state'), such that vklist' is the result of mapping lvk over vklist, with state' the
	result of accumulating state changes over the mapping
   *)
   and lvklist state vklist =
       let fun vkfolder((v,k),state) =
	   let val k = lkind state k
	       val (state,v) = add_var(state,v)
	   in  ((v,k), state)
	   end
       in  foldl_acc vkfolder state vklist
       end


   and lvtrlist_flat state vtrlist =
       let fun vtrfolder((v,tr),state) =
	   let val (state,v) = add_var(state,v)
	   in  ((v,tr), state)
	   end
       in  foldl_acc vtrfolder state vtrlist
       end

   and lvlist state vlist =
       let fun vfolder(v,state) =
	   let val (state,v) = add_var(state,v)
	   in  (v, state)
	   end
       in  foldl_acc vfolder state vlist
       end

   and lclist_flat state vclist =
       let 
	 fun cmapper c =
	   let val _ = inc depth_lcon_function
	     val c = lcon_lift' state c
	     val _ = dec depth_lcon_function
	   in  c
	   end
	 val cs = map cmapper vclist
       in  cs
       end

   and lcon_list lift state clist =
       let 
	 fun folder (c,(rcbnds,state)) =
	   let 
	     val (state,cbnds,c) = lcon lift state c
	     val rcbnds = List.revAppend(cbnds,rcbnds)
	   in  (c,(rcbnds,state))
	   end
	 val (cs,(rcbnds,state)) = foldl_acc folder ([],state) clist
       in  (state,rev rcbnds,cs)
       end

   (*
    val lkind : state -> kind -> kind
    lkind state kind ==> A-normalized version of kind, translated using state
   *)
   and lkind state arg_kind : kind =
       ((lkind' state arg_kind)
       handle e => (print "exception in lkind call with kind =\n";
		    pp_kind arg_kind; print "\n"; raise e))

   (*
    val lkind' : state -> kind -> kind
    lkind' state kind ==> A-normalized version of kind, translated using state.
	Constructors contained within kind are translated flatly.
   *)
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


   (*
    val lexport : state -> export_entry -> export_entry
    lexport state export ==> export with its variable renamed according to state
   *)
   fun lexport state (ExportValue(l,v)) = ExportValue(l,find_var(state,v))
     | lexport state (ExportType(l,v)) = ExportType(l,find_var(state,v))

   (*
    val limport : import_entry * state -> import_entry * state
    limport (import, state) ==> (import', state'), where import is import' with its variable renamed according to state
	and constructor (if present) A-normalized (with state' updated from state appropriately)
   *)
   fun limport (imp as ImportValue(l,v,tr,c),s) =
       (case c of
	    ExternArrow_c _ =>
		let val (s,v) = add_var(s,v)
		    val _ = inc depth_lcon_import
		    val c = lcon_flat s c
		    val _ = dec depth_lcon_import
		in  ([ImportValue(l,v,tr,c)],s)
		end
	  | _ =>
		let val (s,v) = add_var(s,v)
		    val _ = inc depth_lcon_import
		    val (state,cbs, c) = lcon_lift s c
		    val _ = dec depth_lcon_import
		    val icbs = map (fn cb => ImportBnd (Runtime, cb)) cbs
		in   (icbs @ [ImportValue(l,v,tr,c)],s)
		end)
     | limport (ImportType(l,v,k),s) =
       let val (s,v) = add_var(s,v)
       in  ([ImportType(l,v,lkind s k)],s)
       end

     (* There are no import bnds yet *)
     | limport (imp as ImportBnd (phase,cb),s) = 
       let
	 val (cbnds,s) = lcbnd true s cb
       in (map (fn cb => (ImportBnd(phase,cb))) cbnds,s)
       end

   (*
    val limports : import_entry list * state -> import_entry_list * state
    limports (imports, state) ==> folding of limport over imports using state
   *)
   fun limports (imports,s) =
       let
	   val (imps, s) = foldl_acc limport s imports
       in
	   (List.concat imps, s)
       end

   (*
    val linearize_exp : exp -> exp
    linearize_exp exp ==> A-normal version of exp
   *)
   fun linearize_exp e =
       let (* Permit expression to be open *)
	   val state = reset_state true
	   val e = lexp_lift' state e
	   val _ = reset_state false
       in  e
       end

   (*
    val linearize_mod : module -> module
    linearize_mod module ==> module with bindings, imports, and exports A-normalized
   *)
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
