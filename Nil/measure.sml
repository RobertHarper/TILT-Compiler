(*$import Prelude TopLevel List Nil MEASURE NilRewrite Stats Option Int *)

structure Measure :> MEASURE =
  struct
    open Nil NilRewrite
      
    local 
      
      type state = {cstring : con -> string,
		    con:int ref,exp:int ref,kind:int ref,con_vars:int ref,exp_vars:int ref,
		    active:(string*(int ref)) list,counters:(string*(int ref)) list,
		    containers:(string*(int ref)) list}
	

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

      fun con_var_xxx (state as {con_vars,...} : state,_,_) = (inc con_vars;(state,NONE))
      fun exp_var_xxx (state as {exp_vars,...} : state,_,_) = (inc exp_vars;(state,NONE))

      val all_handlers =  
	let
	  val h = set_kindhandler default_handler kindhandler
	  val h = set_conhandler h conhandler
	  val h = set_exphandler h exphandler
	  val h = set_con_binder h con_var_xxx
	  val h = set_con_definer h con_var_xxx
	  val h = set_exp_binder  h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	  val h = set_sum_binder  h exp_var_xxx
	  val h = set_exn_binder  h exp_var_xxx
	in
	  h
	end

      val {rewrite_con,rewrite_exp,rewrite_kind,rewrite_mod,...} = rewriters all_handlers

    in
      
      fun item_size rewrite_item {cstring : con -> string,
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

	in (print "\n";
	    print "Con vars bound        = ";printi (!con_vars);print "\n";
	    print "Exp vars bound        = ";printi (!exp_vars);print "\n";
	    print "Kind Nodes            = ";printi (!kind);print "\n";
	    print "Con  Nodes            = ";printi (!con);print "\n";
	    print "Exp  Nodes            = ";printi (!exp);print "\n";
	    print "All  Nodes            = ";printi (total);print "\n";

	    List.app print_count counters;
	    List.app print_container containers;

	    print "\n";
	    total)
	end
      val con_size  = item_size rewrite_con
      val exp_size  = item_size rewrite_exp
      val kind_size = item_size rewrite_kind
      val mod_size  = item_size rewrite_mod

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
	 | Proj_c _              => "Proj_c"
	 | Typeof_c _            => "Typeof_c"
	 | App_c _               => "App_c"
	 | Crecord_c _           => "Crecord_c"
	 | Closure_c _           => "Closure_c"
	 | Typecase_c _          => "Typecase_c"
	 | Annotate_c (_,c)      => "Annotate_c"
	 | Coercion_c _          => "Coercion_c")

    end
  end