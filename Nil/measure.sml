(*$import Prelude TopLevel List Nil MEASURE NilRewrite Stats Option Int *)

structure Measure :> MEASURE =
  struct
    open Nil NilRewrite
      
(*    local
      structure VarMap = Name.VarMap
	
      datatype state = STATE of {cbound : bool ref VarMap.map,
				 bound : unit -> int,
				 occur : unit -> int,
				 rewrite_kind : state -> Nil.kind -> Nil.kind}
	
      fun conhandler (state : state as STATE {cbound,bound,occur,...},con : con) =
	(case con
	 of Var_c var => 
	   (case VarMap.find (cbound,var)
	      of SOME seen =>
		( if !seen then ()
		  else (seen := true;ignore (occur()));
		  NOCHANGE
		)
	       | NONE => NOCHANGE)
	  | _ => NOCHANGE)

      fun bind_var (STATE {cbound,bound,occur,rewrite_kind},var,flag) = 
	STATE {cbound = VarMap.insert(cbound,var,flag),
	       bound = bound,
	       occur = occur,
	       rewrite_kind = rewrite_kind}

      fun kindhandler (state : state as STATE {cbound,bound,occur,rewrite_kind},kind : kind) =
	(case kind
	   of Record_k lvk_seq =>
	     let 
	       fun loop ([],state) = []
		 | loop (((l,v),k)::rest,state) = 
		 let
		   val _ = bound()
		   val flag = ref false
		   val k = rewrite_kind state k
		   val state = bind_var(state,v,flag)
		   val rest = loop (rest,state)
		 in
		   if !flag then ((l,v),k)::rest else ((l,Name.derived_var v),k)::rest
		 end
	       val lvk_list = loop (Sequence.toList lvk_seq,state)
	       val kind = Record_k (Sequence.fromList lvk_list)
	     in
	       CHANGE_NORECURSE (state,kind)
	     end
	| Arrow_k (openness, vk_list, body) =>
	     let 
	       fun loop ([],state) = ([],rewrite_kind state body)
		 | loop ((v,k)::rest,state) = 
		 let
		   val _ = bound()
		   val flag = ref false
		   val k = rewrite_kind state k
		   val state = bind_var(state,v,flag)
		   val (rest,body) = loop (rest,state)
		 in
		   if !flag then ((v,k)::rest,body) else ((Name.derived_var v,k)::rest,body)
		 end
	       val (vk_list,body) = loop (vk_list,state)
	     in
	       CHANGE_NORECURSE (state,Arrow_k(openness,vk_list,body))
	     end
	| _ => NOCHANGE)

      val all_handlers =  
	let
	  val h = set_kindhandler default_handler kindhandler
	  val h = set_conhandler h conhandler
	in
	  h
	end

      val {rewrite_mod  = measureMod',
	   rewrite_kind = rewrite_kind,
	   ...} = rewriters all_handlers

    in
      fun measureMod module = 
	let
	  val state = STATE {cbound = VarMap.empty,
			     bound = Stats.counter "KindConVarsBound",
			     occur = Stats.counter "KindConVarsOccuring",
			     rewrite_kind = rewrite_kind}
	  val module = measureMod' state module
	in
	  module
	end
    end
*)  
    local 
      
      type state = {con:int ref,exp:int ref,kind:int ref,con_vars:int ref,exp_vars:int ref,
		    active:(string*(int ref)) list,counters:(string*(int ref)) list,
		    containers:(string*(int ref)) list}
	
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
	 | Annotate_c (_,c)      => "Annotate_c")

      fun inc x = x := !x + 1

      fun incl clist = List.app (fn (s,x) => inc x) clist

      fun find c clist = let val s = cstring c in List.find (fn (s',x) => s' = s) clist end

      fun inc1 c clist = 
	(case find c clist
	   of SOME(_,x) => inc x
	    | NONE => ())

      fun contains clist c = Option.isSome (find c clist)

      fun conhandler ({exp,con,kind,active,counters,containers,con_vars,exp_vars} : state,c : con) = 
	(inc con;
	 incl active;
	 inc1 c counters;
	 if contains active c then NOCHANGE
	 else (case find c containers of
		 NONE => NOCHANGE
	       | SOME entry => CHANGE_RECURSE ({con = con,exp = exp,kind = kind,con_vars=con_vars,exp_vars=exp_vars,
						active = entry::active,counters = counters,containers=containers},c))
	 )

      fun kindhandler ({kind,active,counters,...} : state,k : kind) =  (inc kind; incl active; NOCHANGE)

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
	  val h = set_exp_binder h exp_var_xxx
	  val h = set_exp_definer h exp_var_xxx
	in
	  h
	end

      val {rewrite_con,rewrite_exp,rewrite_kind,rewrite_mod,...} = rewriters all_handlers
      val printi = print o Int.toString
    in
      
      fun item_size rewrite_item item = 
	let val exp = ref 0
	  val con = ref 0
	  val kind = ref 0
	  val con_vars = ref 0
	  val exp_vars = ref 0
	  val in_mu = ref 0
	  val mu_count = ref 0
	  val in_prim = ref 0
	  val prim_count = ref 0
	  val in_sum = ref 0
	  val sum_count = ref 0
	  val in_record = ref 0
	  val record_count = ref 0
	  val state : state = {con = con,exp = exp,kind = kind,active=[],
			       con_vars = con_vars,exp_vars = exp_vars,
			       containers = [("Mu_c",in_mu),("Prim_c",in_prim),
					     ("Sum_c",in_sum),("Record_c",in_record)],
			       counters   = [("Mu_c",mu_count),("Prim_c",prim_count),
					     ("Sum_c",sum_count),("Record_c",record_count)]}
	    
	  val _ = rewrite_item state item
	in (print "\n";
	    print "Kind Nodes            = ";printi (!kind);print "\n";
	    print "Con  Nodes            = ";printi (!con);print "\n";
	    print "Exp  Nodes            = ";printi (!exp);print "\n";
	    print "Nodes in Mu_c         = ";printi (!in_mu);print "\n";
	    print "Nodes in Sum_c        = ";printi (!in_sum);print "\n";
	    print "Nodes in Record_c     = ";printi (!in_record);print "\n";
	    print "Nodes in Other Prim_c = ";printi (!in_prim);print "\n";
	    print "Mu_c Nodes            = ";printi (!mu_count);print "\n";
	    print "Sum_c Nodes           = ";printi (!sum_count);print "\n";
	    print "Record_c Nodes        = ";printi (!record_count);print "\n";
	    print "Other Prim_c Nodes    = ";printi (!prim_count);print "\n";
	    print "Con vars bound        = ";printi (!con_vars);print "\n";
	    print "Exp vars bound        = ";printi (!exp_vars);print "\n";
	    print "\n";
	    !exp + !con + !kind)
	end
      val con_size  = item_size rewrite_con
      val exp_size  = item_size rewrite_exp
      val kind_size = item_size rewrite_kind
      val mod_size  = item_size rewrite_mod
      val measureMod = fn nilmod => (mod_size nilmod;nilmod)
    end
  end