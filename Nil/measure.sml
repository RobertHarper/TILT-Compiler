(*$import MEASURE NilRewrite Stats *)

structure Measure :> MEASURE =
  struct
    open Nil NilRewrite
      
    local
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
		   if !flag then ((l,v),k)::rest else ((l,Name.derived_var' v),k)::rest
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
		   if !flag then ((v,k)::rest,body) else ((Name.derived_var' v,k)::rest,body)
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
  
  end