(*$import Nil NilUtil Ppnil LibBase SPECIALIZE *)

(* A two-pass optimizer to remove unnecesssarily polymorphic code:
     Essentially, convert
       fun f _ = 5
       fun g() = f () + f ()
    into
       fun f () = 5
       fun g() = f () + f ()
    This is possible when f does not escape.  
    This is desirable since calling a polymorphic function
       requires two calls and can lead to unnecessary constructor code.
*)	

structure Specialize :> SPECIALIZE =
struct

	open Util Nil NilUtil Listops
 	val error = fn s => Util.error "specialize.sml" s

	val debug = ref false
	val diag = ref false
	val do_dead = ref true
	val do_proj = ref true

	(* In the first pass, we locate all term-level functions 
	   that take only constructor arguments.  At the same time,
	   we find calls to these functions and check whether the
         constructor args are the same at each call site. If not, 
	  the function is not specializable and removed as a candidate.
	  Also, the constructor argument must be well-formed at the function
	  definition.  If we assume unique naming, we need to only check that
	  the variables in the arguments are bound.

	  A state consists of constructor (optional) bindings.

	  A global data structure will hold candidate functions,
	    a corresponding state(defining which variables are bounded),
	    and the constructor arguments applied.

       *)

	local
	  datatype state = STATE of {bound : (con option) Name.VarMap.map}
	in
	    exception Unbound
	  type state = state
	  fun new_state() = STATE {bound = Name.VarMap.empty}
	  fun add_vars(STATE{bound},conbinds) = 
		let val bound = foldl (fn ((v,c),m) => Name.VarMap.insert(m,v,c)) bound conbinds
		in  STATE{bound=bound}
		end
	  fun add_var(state,v,copt) = add_vars(state,[(v,copt)])
	  fun lookup (STATE {bound}) v = (case Name.VarMap.find(bound,v) of
				    NONE => raise Unbound
				  | SOME copt => copt)
	  fun show_state (STATE{bound}) = 
	      let fun show(v,copt) = 
		  (Ppnil.pp_var v; print "  -->  ";
		   case copt of
		       NONE => ()
		     | SOME c => Ppnil.pp_con c;
			   print "\n")
	      in  Name.VarMap.appi show bound
	      end
	end


	fun cons_reduce state clist = map (con_reduce state) clist
	and con_reduce (state : state) c =
	    let val self = con_reduce state
	    in  (case c of
		     (Crecord_c lclist) => Crecord_c(map (fn (l,c) => (l,self c)) lclist)
		   | (Prim_c (pc,clist)) => Prim_c(pc,cons_reduce state clist)
		   | (Var_c v) => 
			      (case (lookup state v) of
				   NONE => c
				 | SOME c => self c)
		   | Annotate_c (_, c) => self c
		   | _ => c)
	    end

	fun cons_equal state (clist1,clist2) = Listops.eq_list(con_equal state,clist1,clist2)
	and con_equal (state : state)  (c1,c2) = 
	    let val self = con_equal state
	    in  (case (c1,c2) of
		     (Crecord_c lclist1, Crecord_c lclist2) =>
			 cons_equal state (map #2 lclist1, map #2 lclist2)
		       | (Prim_c (pc1,clist1), Prim_c (pc2,clist2)) => 
			 (NilUtil.primequiv(pc1,pc2)) andalso cons_equal state(clist1,clist2)
		       | (Var_c v1, Var_c v2) => 
			 Name.eq_var(v1,v2) orelse
			 (case (lookup state v1, lookup state v2) of
			      (NONE, NONE) => false
			    | (SOME c1, NONE) => con_equal state (c1,c2)
			    | (NONE, SOME c2) => con_equal state (c1,c2)
			    | (SOME c1, SOME c2) => con_equal state (c1,c2))
		       | (Var_c v1, _) => 
			      (case (lookup state v1) of
				   NONE => false
				 | SOME c1 => con_equal state (c1,c2))
		       | (_, Var_c v2) => 
				   (case (lookup state v2) of
			       NONE => false
			     | SOME c2 => con_equal state (c1,c2))
		       | (Annotate_c (_, c1), _) => con_equal state (c1,c2)
		       | (_, Annotate_c (_, c2)) => con_equal state (c1,c2)
		       | _ => false)
	    end

	fun cons_bound (state : state) clist = Listops.andfold (fn c => con_bound state c) clist
	and con_bound state c = 
	    let val self = con_bound state
(*
		val _ = (print "con_bound called on ";
			 Ppnil.pp_con c; print "  with state\n";
			 show_state state; print "\n")
*)
	    in
		(case c of
		     Annotate_c(_, c) => self c
		   | Var_c v => ((lookup state v; true) handle Unbound => false)
		   | Prim_c (pc, clist) => cons_bound state clist
		   | Crecord_c lclist => cons_bound state (map #2 lclist)
		   | Proj_c(c,_) => self c
		   | _ => false)
	    end

	local
	    val cand = ref (Name.VarMap.empty : (state * var * con list option) Name.VarMap.map)
	in
	    fun reset_candidate() = cand := Name.VarMap.empty
	    fun add_candidate(state,v,inner) = 
		let 
(*
		    val _ = (print "ADDING CANDIDATE: ";
			     Ppnil.pp_var v; print "\n")
*)
		    val c = Name.VarMap.insert(!cand,v,(state,inner,NONE))
		in  cand := c
		end
	    fun remove_candidate str v = 
		let 
(*
		    val _ = (print "REMOVING CANDIDATE("; 
			     print str; print "): ";
			     Ppnil.pp_var v; print "\n")
*)
		    val c = (#1(Name.VarMap.remove(!cand,v))
			     handle LibBase.NotFound => !cand)
		in  cand := c
		end
	    fun update_candidate(v,clist) = 
		let val SOME(state,inner,_) = Name.VarMap.find(!cand,v)
		    val _ = remove_candidate "UPDATE" v
		    val c = Name.VarMap.insert(!cand,v,(state,inner,SOME clist))
		in  cand := c
		end

	    fun is_candidate v = 
		(case (Name.VarMap.find(!cand,v)) of
		     NONE => NONE
		   | SOME (_,inner,SOME clist) => SOME (inner,clist)
		   | _ => NONE)

	    fun use_candidate(s : state, v, clist) = 
		case (Name.VarMap.find(!cand,v)) of
		    NONE => ()
		  | SOME(fs : state, _, NONE) => 
			let val clist = cons_reduce s clist
			in  if (cons_bound fs clist)
				then update_candidate(v,clist)
			    else remove_candidate "USE1" v
			end
		  | SOME(_,_,SOME clist') => if cons_equal s (clist,clist')
					       then ()
					   else remove_candidate "USE2" v


	    fun show_candidate() = 
		let fun show(v,(state,inner,clist_opt)) = 
		    (Ppnil.pp_var v; print "  -->  ";
		     Ppnil.pp_var inner; print "  :  ";
		     case clist_opt of
			 NONE => ()
		       | SOME clist => (app (fn c => (Ppnil.pp_con c; print "\n")) clist);
		     print "\n")
		in  (print "\nCANDIDATES:\n";
		     Name.VarMap.appi show (!cand);
		     print "\n")
		end
		
	end


        (* This first set of functions collects functions that are candidates
	   for specialization.  In the second pass, we eliminate all
          calls to specializable functions and patch the specialized
	   function by wrapping the constructor arguments around the function. *)

        (* ----------------------------- PASS ONE -------------------- *)
	fun scan_exp (state : state) (exp : exp) : unit = 
	   (case exp of
		  Var_e v => remove_candidate "Var_e" v
		| Const_e _ => ()
		| Prim_e(p,clist,elist) => app (scan_exp state) elist
		| Switch_e sw => scan_switch state sw
		| Let_e (letsort,bnds,e) => 
			let val state = scan_bnds(bnds,state)
			in  scan_exp state e
			end
		| App_e(openness,Var_e v,clist,[],[]) => use_candidate(state,v,clist)
		| App_e(openness,f,clist,elist,eflist) => 
			(scan_exp state f; app (scan_exp state) elist; app (scan_exp state) eflist)
		| ExternApp_e(f,elist) => 
			(scan_exp state f; app (scan_exp state) elist)

		| Raise_e(e,c) => scan_exp state e
		| Handle_e(e,bound,handler) => (scan_exp state e; scan_exp state handler))

	and scan_switch (state : state) (switch : switch) : unit = 
	  let fun scan_sw scan_info scan_arg scan_tag {info,arg,arms,default} = 
			(scan_info info;
			 scan_arg arg;
			 map (fn (tag,f) => (scan_tag tag, scan_function state f)) arms;
			 Util.mapopt (scan_exp state) default; ())
	      fun nada _ = ()
	      (* don't need to bind term-level variables *)
	  in  (case switch of
		  Intsw_e {arg,arms,default,...} => 
		      (scan_exp state arg;
		       Util.mapopt (scan_exp state) default;
		       app (fn (t,e) => (scan_exp state e)) arms)
		| Sumsw_e {arg,arms,default,...} =>
		      (scan_exp state arg;
		       Util.mapopt (scan_exp state) default;
		       app (fn (t,e) => (scan_exp state e)) arms)
		| Exncase_e {arg,arms,default,...} =>
		      (scan_exp state arg;
		       Util.mapopt (scan_exp state) default;
		       app (fn (e1,e2) => (scan_exp state e1; scan_exp state e2)) arms)
		| Typecase_e _ => error "typecase_e not done")
	  end

	and scan_function (state : state) (Function(effect,recur,vklist,_,vclist,vlist,e,c)) : unit =
		let val state = add_vars(state,map (fn (v,_) => (v,NONE)) vklist) 
		in scan_exp state e
		end

	and scan_bnds(bnds : bnd list, state : state) : state = foldl scan_bnd state bnds

	and scan_bnd (bnd : bnd, state : state) : state = 
	  	(case bnd of
		     Exp_b(v,_,e) => (scan_exp state e; state)
		   | Con_b(p,cbnd) => 
			 (case cbnd of
			      Con_cb(v,c) => add_var(state,v,SOME c)
			    | Open_cb (v,_,_,_) => add_var(state,v,SOME
							   (Let_c(Sequential,[cbnd],Var_c v)))
			    | Code_cb (v,_,_,_) => add_var(state,v,SOME
							   (Let_c(Sequential,[cbnd],Var_c v))))
		   | Fixopen_b vfset =>
		      let val vflist = (Sequence.toList vfset)
			  val _ = 
			      (case vflist of
				   [(v,Function(_,Leaf,vklist,_,[],[],e,_))] => 
				       (case e of
					   Let_e(_,[Fixopen_b vfset], Var_e inner) =>
					       (case (Sequence.toList vfset) of
						   [(v',_)] => if (Name.eq_var(v',inner))
								   then add_candidate(state,v,inner)
							       else ()
						 | _ => ())
					 | _ => ())
				 | _ => ())
			  fun scan_vf (v,f) = scan_function state f
		      in  (app scan_vf vflist; state)
		      end
		   | Fixcode_b _ => error "sorry: Fixcode not handled"
		   | Fixclosure_b _ => error "sorry: Fixclosure not handled")

	fun scan_import(ImportValue(l,v,c),state) : state = state
	  | scan_import(ImportType(l,v,k),state)  = add_var(state,v,NONE)

	fun scan_export state (ExportValue(l,v)) : unit = ()
	  | scan_export state (ExportType(l,v)) = ()

	fun scan_module(MODULE{imports, exports, bnds}) : unit = 
	  let val state = new_state()
	      val state = foldl scan_import state imports
	      val state = scan_bnds(bnds,state)
	      val _ = app (scan_export state) exports
	      val _ = if (!diag)
			  then show_candidate()
		      else ()
	  in  ()
	  end


        (* ----------------------------- PASS TWO -------------------- *)
	fun do_exp (exp : exp) : exp = 
	   (case exp of
		  Var_e v => exp
		| Const_e _ => exp
		| Prim_e(p,clist,elist) => Prim_e(p, clist, map do_exp elist)
		| Switch_e sw => Switch_e(do_switch sw)
		| Let_e (letsort,bnds,e) => Let_e(letsort,do_bnds bnds, do_exp e)
		| App_e(openness,Var_e v,clist,[],[]) => 
		      (case (is_candidate v) of
			   NONE => App_e(openness,do_exp (Var_e v),clist,[],[])
			 | SOME (inner,_) => Var_e inner)
		| App_e(openness,f,clist,elist,eflist) => 
		      App_e(openness,do_exp f,clist,map do_exp elist,map do_exp eflist) 
		| ExternApp_e(f,elist) =>
		      ExternApp_e(do_exp f,map do_exp elist)
		| Raise_e(e,c) => Raise_e(do_exp e, c)
		| Handle_e(e,v,handler) => Handle_e(do_exp e, v, do_exp handler))

	and do_switch (switch : switch) : switch = 
	   (case switch of
		Intsw_e {size,arg,arms,default} =>
		    let val arg = do_exp arg
			val arms = map (fn (w,e) => (w,do_exp e)) arms
			val default = Util.mapopt do_exp  default
		    in  Intsw_e {size=size,arg=arg,arms=arms,default=default}
		    end
	      | Sumsw_e {sumtype,arg,bound,arms,default} =>
		    let val arg = do_exp arg
			val arms = map (fn (t,e) => (t,do_exp e)) arms
			val default = Util.mapopt do_exp default
		    in  Sumsw_e {sumtype=sumtype,arg=arg,
				 bound=bound,arms=arms,default=default}
	      end
	      | Exncase_e {arg,bound,arms,default} =>
		let val arg = do_exp arg
		    val arms = map (fn (e1,e2) => (do_exp e1, do_exp e2)) arms
		    val default = Util.mapopt do_exp  default
		in  Exncase_e {arg=arg,
			       bound=bound,arms=arms,default=default}
		end
	      | Typecase_e _ => error "typecase not handled")


	and do_function (Function(effect,recur,vklist,dep,vclist,vlist,e,c)) : function =
	    let val e = do_exp e
	    in  Function(effect,recur,vklist,dep,vclist,vlist,e,c)
	    end

	and do_bnds(bnds : bnd list) : bnd list = 
	    let val bnds_list = map do_bnd bnds
	    in  (List.concat bnds_list)
	    end

	and do_bnd (bnd : bnd) : bnd list =
	  	(case bnd of
		     Exp_b(v,traceinfo,e) => [Exp_b(v,traceinfo,do_exp e)]
		   | Con_b(v,c) => [bnd]
		   | Fixopen_b vfset =>
		      let val vflist = (Sequence.toList vfset)
			  fun do_vflist vflist = 
			      [Fixopen_b(Sequence.fromList(map (fn (v,f) => (v,do_function f)) vflist))]
		      in  (case vflist of
			       [(v,Function(_,Leaf,vklist,_,[],[],Let_e(_,[Fixopen_b vflist'],_),_))] =>
				   (case (is_candidate v) of
					NONE => do_vflist vflist
				      | SOME (v,clist) => 
					    let fun mapper ((v,k),c) = Con_b(Runtime,Con_cb(v,c))
						val cbnds = Listops.map2 mapper (vklist,clist)
					    in  cbnds @ do_vflist (Sequence.toList vflist')
					    end)
			     | _ => do_vflist vflist)
		      end
		   | Fixcode_b _ => error "sorry: Fixcode not handled"
		   | Fixclosure_b _ => error "sorry: Fixclosure not handled")

	fun do_import import = import

	fun do_export(ExportValue(l,v)) = ExportValue(l,v)
	  | do_export(ExportType(l,v))  = ExportType(l,v)
	    
	fun do_module(MODULE{imports, exports, bnds}) = 
	  let val imports = map do_import imports
	      val bnds = do_bnds bnds
	      val exports = map do_export exports
	  in  MODULE{imports=imports,exports=exports,bnds=bnds}
	  end

      (* Main optimization routine:
          Scan module for specializable candidates
	   Rewrite module by specializing candidate and rewriting calls to candidates
     *)
      fun optimize module = (reset_candidate();
			     scan_module module; 
			     do_module module)
end
