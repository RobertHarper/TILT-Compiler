(*
 Measuring the size of a module, in terms of numbers of imports, bindings, and exports
*)

structure Measure :> MEASURE =
  struct
    open Nil NilRewrite

    val chatlev = Stats.int("MeasureChatlev",0)
    (* Controls how much extra printed output to produce.
       Greater than 0 gets some output.
       Greater than 1 gets extra ouput, including counts of specific types of constructors. *)

    fun chat i s = if !chatlev > i then print s else ()

    local

      type state = {cstring : con -> string,
		    con:int ref,exp:int ref,kind:int ref,con_vars:int ref,exp_vars:int ref,
		    active:(string*(int ref)) list,counters:(string*(int ref)) list,
		    containers:(string*(int ref)) list}
      (* cstring = function to translate constructors to string name
         !con = number of constructors encountered so far
         !exp = number of expressions encountered
         !kind = number of kinds encountered
         !con_vars = number of constructor variables bound
         !exp_vars = number of expression variables bound
         active = mapping from constructor names to refs to the number of constructor nodes that occur as
	          subconstructors of that variety; only includes constructor names that correspond with constructors that
		  are actually parents of the current subconstructor
	 counters = mapping from constructor names to refs to the number of times that variety of constructor occurs
         containers = mapping from constructor names to refs to the number of constructor nodes that occur as
	          subconstructors of that variety *)

      fun inc x = x := !x + 1

      fun incl clist = List.app (fn (s,x) => inc x) clist

      fun find c clist = List.find (fn (s,x) => s = c) clist

      fun inc1 c clist =
	(case find c clist
	   of SOME(_,x) => inc x
	    | NONE => ())

      fun contains clist c = Option.isSome (find c clist)

      fun conhandler ({cstring,exp,con,kind,active,counters,containers,con_vars,exp_vars} : state,c : con) =
	let
	  val s = cstring c
	in
	  inc con;
	  incl active;
	  inc1 s counters;
	  if contains active s then NOCHANGE
	  else (case find s containers of
		  NONE => NOCHANGE
		| SOME entry =>
		    CHANGE_RECURSE ({cstring = cstring,
				     con = con,exp = exp,kind = kind,con_vars=con_vars,exp_vars=exp_vars,
				     active = entry::active,counters = counters,containers=containers},c))
	end

      fun kindhandler ({kind,active,...} : state,k : kind) =  (inc kind; incl active; NOCHANGE)

      fun exphandler ({exp,active,...} : state,_ : exp) = (inc exp;incl active; NOCHANGE)

      (* con binder *)
      fun con_var_bind (state as {con_vars,...} : state,_) = (inc con_vars;(state,NONE))
      (* exp binder *)
      fun exp_var_bind (state as {exp_vars,...} : state,_) = (inc exp_vars;(state,NONE))

      val all_handlers =
	let
	  val h = set_kindhandler default_handler kindhandler
	  val h = set_conhandler h conhandler
	  val h = set_exphandler h exphandler
	  val h = set_con_binder h con_var_bind
	  val h = set_exp_binder  h exp_var_bind
	in
	  h
	end

      val {rewrite_con,rewrite_exp,rewrite_kind,rewrite_mod,...} = rewriters all_handlers

    in
      type measure = {cstring  : Nil.con -> string,
		      count    : string list,
		      count_in : string list}
      (* cstring = map from constructors to names
         count = names of constructor varieties for which to count the numbers of occurrences
         count_in = names of constructor varieties for which to count the number of constructor nodes in children
	            of their occurrences *)

      type size = {kinds :int,
		   cons  :int,
		   exps  :int,
		   cvars :int,
		   evars :int,
		   total :int}
      (* measure count results *)

      (* polymorphic function to measure starting at different top-level entities *)
      fun item_size rewrite_item {cstring  : con -> string,
				  count    : string list,
				  count_in : string list} item =
	let
	  val exp = ref 0
	  val con = ref 0
	  val kind = ref 0
	  val con_vars = ref 0
	  val exp_vars = ref 0
	  val counters   = map (fn s => (s, ref 0)) count
	  val containers = map (fn s => (s, ref 0)) count_in
	  val state : state = {cstring = cstring,
			       con = con,exp = exp,kind = kind,active=[],
			       con_vars = con_vars,exp_vars = exp_vars,
			       containers = containers,
			       counters   = counters}

	  val _ = rewrite_item state item

	  val printi = print o Int.toString
	  val printl = List.map print
	  fun print_count (s,r) =
	    (printl [s," Nodes            = "];printi (!r);print "\n")
	  fun print_container (s,r) =
	    (printl ["Nodes in ",s,"         = "];printi (!r);print "\n")

	  val total = !exp + !con + !kind

	in if !chatlev > 1 then
	  (print "\n";
	   print "Con vars bound        = ";printi (!con_vars);print "\n";
	   print "Exp vars bound        = ";printi (!exp_vars);print "\n";
	   print "Kind Nodes            = ";printi (!kind);print "\n";
	   print "Con  Nodes            = ";printi (!con);print "\n";
	   print "Exp  Nodes            = ";printi (!exp);print "\n";
	   print "All  Nodes            = ";printi (total);print "\n";

	   List.app print_count counters;
	   List.app print_container containers;

	   print "\n") else ();
	  {kinds= !kind,cons= !con,exps= !exp,cvars= !con_vars,evars= !exp_vars,total=total}
	end
      val con_size  = item_size rewrite_con
      val exp_size  = item_size rewrite_exp
      val kind_size = item_size rewrite_kind

      val mod_size  = item_size rewrite_mod

      (* mod_size meas module ==> counts of components of module, using the constructor string conversion function in meas.
	 count and count_in in meas only matter if the chat level is high enough to generate printed output, in which case
	 the counts of the constructor varieties they give will be printed.
        Effects: Prints count information with chat level above 1. *)

      (* add the counts in two different measure results *)
      fun adds
	  {kinds=ks1,cons=cs1,exps=es1,cvars=cvs1,evars=evs1,total=t1}
	  {kinds=ks2,cons=cs2,exps=es2,cvars=cvs2,evars=evs2,total=t2} =
	  {kinds=ks1+ks2,cons=cs1+cs2,exps=es1+es2,cvars=cvs1+cvs2,evars=evs1+evs2,total=t1+t2}

      (*
        val mod_size' : measure -> Nil.module -> size * size * size
        mod_size meas module ==> (impsize, bndsize, exportsize), where these sizes are the results of measuring the
	    imports, bindings, and exports of module, respectively.
        Effects: Prints total size information with chat level above 0
      *)
      fun mod_size' args (MODULE {bnds,imports,exports,exports_int}) =
	let
	  val _ = chat 0 "\nIMPORTS\n"
	  val impsize = mod_size args (MODULE {bnds=[],imports=imports,exports=[],exports_int=NONE})

	  val _ = chat 0 "\nBNDS\n"
	  val bndsize = mod_size args (MODULE {bnds=bnds,imports=[],exports=[],exports_int=NONE})

	  val _ = chat 0 "\nEXPORTS\n"
	  val exportsize = mod_size args (MODULE {bnds=[],imports=[],exports=exports,exports_int=NONE})

	  val _ = chat 0 "\nEXPORTS_INT\n"
	  val exports_intsize = mod_size args (MODULE {bnds=[],imports=[],exports=[],exports_int=exports_int})

	  val total = adds impsize (adds bndsize (adds exportsize exports_intsize))

	  val _ = chat 0 ("\nBNDS size is "^(Int.toString (#total bndsize))^"\n")
	  val _ = chat 0 ("\nEXPORTS size is "^(Int.toString (#total exportsize))^"\n")
	  val _ = chat 0 ("\nEXPORTS_INT size is "^(Int.toString (#total exports_intsize))^"\n")
	  val _ = chat 0 ("\nIMPORTS size is "^(Int.toString (#total impsize))^"\n")
	  val _ = chat 0 ("\nTOTAL size is "^(Int.toString (#total total))^"\n")
	in (impsize,bndsize,exportsize)
	end

      fun cstring con =
	(case con of
	   Prim_c (Sum_c _,_)    => "Sum_c"
	 | Prim_c (Record_c _,_) => "Record_c"
	 | Prim_c(pc,clist)      => "Prim_c"
	 | AllArrow_c _          => "AllArrow_c"
	 | ExternArrow_c _       => "ExternArrow_c"
	 | Var_c _               => "Var_c"
	 | Let_c _               => "Let_c"
	 | Mu_c _                => "Mu_c"
         | Nurec_c _             => "Nurec_c"
	 | Proj_c _              => "Proj_c"
	 | App_c _               => "App_c"
	 | Crecord_c _           => "Crecord_c"
	 | Closure_c _           => "Closure_c"
	 | Coercion_c _          => "Coercion_c")

    end
  end
