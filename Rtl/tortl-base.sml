structure TortlBase
    :> TORTL_BASE
   =
struct

   (* Module-level declarations *)

    open Nil Rtl
    structure TW32 = TilWord32
    structure TW64 = TilWord64
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

  (* If you permanently change this flag, you must update the default values
   * in Top/target.sml.  If the flag differs from its default, a different
   * executable name will be generated.
   *)
    val mirrorGlobal = Stats.ff "MirrorGlobal"           (* replicate pointer globals *)
    val mirrorGlobalTag = Rtltags.mirrorGlobalTag

    type label = Rtl.label
    fun error s = Util.error "tortl-base.sml" s

    val do_gcmerge = ref true
    val do_constant_records = ref true
    val do_forced_constant_records = ref true
    val do_single_crecord = ref true

    val diag = ref false
    val debug = Stats.ff("TortlBaseDebug")
    val debug_simp = Stats.ff("tortl_base_debug_simp")
    val debug_bound = ref false
    val maxAllocRequest = 2048   (* Measured in words *)
    val maxMutateRequest = 2048  (* Measured in writes *)

    val get_con = NilContext.find_con

    (*val _ = debug_bound := true*)

    fun debugdo t = if (!debug) then (t(); ()) else ()

   (* ------------------ RTL statistics ------------------------------ *)
       fun makeStat() = let val r = ref 0
			in  (r, fn() => r := (!r) + 1)
			end
       val (numSelect, incSelect) = makeStat()
       val (numRecord, incRecord) = makeStat()
       val (numApp, incApp) = makeStat()
       val (numFun, incFun) = makeStat()
       val (numClosure, incClosure) = makeStat()
       val (numCase, incCase) = makeStat()
       val (numSumInject, incSumInject) = makeStat()
       val (numSumProject, incSumProject) = makeStat()
       val (numSumDynInject, incSumDynInject) = makeStat()
       val (numSumDynProject, incSumDynProject) = makeStat()
       val (numVararg, incVararg) = makeStat()
       val (numOnearg, incOnearg) = makeStat()
       val (numPrim, incPrim) = makeStat()
       val (numGC, incGC) = makeStat()
       val (numMutate, incMutate) = makeStat()
       val (numGlobal, incGlobal) = makeStat()

       val stats = [("Record projections", numSelect),
		    ("Record creations", numRecord),
		    ("Applications", numApp),
		    ("Functions", numFun),
		    ("Closures", numClosure),
		    ("Case statements", numCase),
		    ("Static sum injections", numSumInject),
		    ("Static sum projections", numSumProject),
		    ("Dynamic sum injections", numSumDynInject),
		    ("Dynamic sum projections", numSumDynProject),
		    ("Calls to makeVararg", numVararg),
		    ("Calls to makeOnearg", numOnearg),
		    ("Primitives", numPrim),
		    ("GC checks", numGC),
		    ("Mutate checks", numMutate),
		    ("Globals", numGlobal)]

       fun clear_stats() = app (fn (_,r) => r := 0) stats
       fun show_stats() = (print "\nRTL statistics:\n";
			   app (fn (str,r) =>
				let val tab = Util.spaces (30 - size str)
				in  print "  ";
				    print str;
				    print ": ";
				    print tab;
				    print (Int.toString (!r));
				    print "\n"
				end) stats;
			   print "\n\n")

   (* ------------------ Overall Data Structures ------------------------------ *)

   (* A NIL variable is represented at the RTL level as one of:
     (1) a local RTL register
     (2) a global variable
     (3) a label (either local or extern)
     (4) a code label
     In addition, in some cases, we know that the value of the variable is:
     (1) a constant integer
     (2) a constant real
     (3) a constant record which is always laid out at the given label
           Note that it is the responsibility of the caller of the constructor
	     to lay out the record.
           Components are identified by the variable names.
  	   Perform a lookup to determine if the subcomponents are constants or not.
   *)


    datatype location =
	REGISTER of bool * reg   (* flag indicates whether value is constant *)
      | GLOBAL   of label * rep  (* value resides at this label: includes unboxed real *)

    fun locToString (REGISTER _) = "REGISTER"
      | locToString (GLOBAL _) = "GLOBAL"

    datatype value =
	VOID of rep             (* an undefined values *)
      | INT of TilWord32.word   (* an integer *)
      | TAG of TilWord32.word   (* a traceable small pointer value *)
      | REAL of label           (* an unboxed real at given label *)
      | RECORD of label * value list (* a record whose components are at the given label *)
      | LABEL of label          (* the value of this label: e.g. boxed real *)
      | CODE of label           (* code that residing at this label *)

    fun valToString (VOID _) = "VOID"
      | valToString (INT _) = "INT"
      | valToString (TAG _) = "TAG"
      | valToString (REAL _) = "REAL"
      | valToString (RECORD _) = "RECORD"
      | valToString (LABEL _) = "LABEL"
      | valToString (CODE _) = "CODE"

   datatype term = LOCATION of location
                 | VALUE of value


    fun val2string v = 
      (case v 
	 of (VOID _) => "VOID"
	 | (INT i) => "INT("^(TilWord32.toDecimalString i)^")"
	 | (TAG i) => "TAG("^(TilWord32.toDecimalString i)^")"
	 | (REAL l) => "REAL("^(Pprtl.label2s l)^")"
	 | (RECORD (l,vs)) => "RECORD("^(Pprtl.label2s l)^"={"^(Listops.concatWith "," (map val2string vs))^"})"
	 | (LABEL l) => "LABEL("^(Pprtl.label2s l)^")"
	 | (CODE l) => "CODE("^(Pprtl.label2s l)^")")

   fun location2string t = 
     (case t
	of REGISTER (b,reg) => (if b then "REGc " else "REG ")^(Pprtl.reg2s reg)
	 | GLOBAL (l,r) => "GLOBAL " ^ (Pprtl.label2s l) ^ ":" ^ (Pprtl.rep2s r))

   fun term2string t = 
     (case t 
	of LOCATION loc => "LOC="^(location2string loc)
	 | VALUE v => "VAL="^(val2string v))

   type convar_rep  = term option

   type varmap = term VarMap.map
   type convarmap = convar_rep VarMap.map

   datatype gcstate = ALLOC_IMM of instr ref list   (* all ways of reaching current point went through these fixed-size allocation checks *)
	            | MUTATE_IMM of instr ref list  (* all ways of reaching current point went through these mutation checks *)
	            | GC_UNKNOWN                    (* no information known about how this point was reached *)

  (* ----- Global data structures ------------------------------
   dw: number of words data occupies
   dl: list of data for module
   pl: list of procedures for the entire module
   mutable : addresses of global objects that may contain pointers to heap objects
   gvarmap: how a top-level NIL code variable is represented at the RTL level
   gconvarmap: how a top-level NIL code type variable is represented at the RTL level
   varmap: how a NIL variable is represented at the RTL level
   convarmap: how a NIL typevariable is represented at the RTL level
   ------------------------------------------------------------- *)

   exception NotFound
   val exports = ref (Name.VarMap.empty : (Rtl.label list) Name.VarMap.map)
   val globals = ref VarSet.empty
   val dw = ref 0
   val dl : Rtl.data list ref = ref nil
   val pl : Rtl.proc list ref = ref nil
   type suminfo     = TilWord32.word * TilWord32.word option * con list
   type state = {is_top : bool,
		 env : NilContext.context * suminfo option ref VarMap.map,
		 varmap : varmap,
		 convarmap : convarmap,
		 gcstate : gcstate}
   fun make_state() : state = {is_top = true,
			       env = (NilContext.empty(), VarMap.empty),
			       varmap = VarMap.empty,
			       convarmap = VarMap.empty,
			       gcstate = GC_UNKNOWN}
   val global_state : state ref = ref (make_state())
   val unitname = ref ""

   fun stat_state ({convarmap,...} : state) =
       (VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) convarmap; print "\n\n")
   fun show_state ({env,...} : state) =
       (print "Showing environment part of state:\n";
	NilContext.print_context (#1 env);
	print "\n\n")

   local
       val partialRecordLabels : label list ref = ref nil
       val partialRecords = ref 0
       val totalRecords = ref 0
       val partialFields = ref 0
       val totalFields = ref 0
   in  fun add_static_record (l, nonHeapFields, heapFields) =
         (if (heapFields > 0)
	      then (partialRecordLabels := l :: !partialRecordLabels;
		    partialRecords := 1 + !partialRecords)
	  else
	      totalRecords := 1 + !totalRecords;
	  partialFields := heapFields + (!partialFields);
	  totalFields := nonHeapFields + (!totalFields))
       fun get_static_records() = {partialRecords = !partialRecords,
				   totalRecords = !totalRecords,
				   partialFields = !partialFields,
				   totalFields = !totalFields,
				   partialRecordLabels = !partialRecordLabels}
       fun reset_records() = (partialRecordLabels := nil;
			      partialRecords := 0;
			      totalRecords := 0;
			      partialFields := 0;
			      totalFields := 0)
   end
   fun add_proc p = pl := p :: !pl

  (* Stats.subtimer(str,f) for real timing *)
  fun subtimer(str,f) = f
  fun type_of ({env,...}:state) e =
       subtimer("RTL_typeof",Normalize.type_of)(#1 env,e)

  fun std_kind_of ({env,...}:state) c =
      subtimer("RTL_kind_of",NilContext.kind_of) (#1 env,c)

  fun find_con ({env = (nilenv,_),...}:state) v = NilContext.find_con (nilenv, v)

  (* ---- Looking up and adding new variables --------------- *)

  fun top_rep (v : var, vt : term) =
      (VarSet.member(!globals,v))
      andalso (case vt of
		   VALUE _ => true
		 | LOCATION (GLOBAL _) => true
		 | _ => false)

  fun state_var_insert' ({is_top,varmap,
		       env,convarmap,gcstate} : state) (v,(t,c)) : state =
      let val _ = if (!debug_bound)
		      then (print "varmap adding to v = ";
			    Ppnil.pp_var v; print "\n")
		  else ()
	  val _ = case (VarMap.find(varmap,v)) of
		  NONE => ()
		| SOME _ => error ("varmap already contains "
					    ^ (Name.var2string v))
	  val env = (NilContext.insert_con(#1 env,v,c), #2 env)
	  val varmap = VarMap.insert(varmap,v,t)
      in  {is_top=is_top,env=env,varmap=varmap,convarmap=convarmap,gcstate=gcstate}
      end

  fun state_var_insert_eq' ({is_top,varmap,
		       env,convarmap,gcstate} : state) (v,(t,e)) : state =
      let val _ = if (!debug_bound)
		      then (print "varmap adding to v = ";
			    Ppnil.pp_var v; print "\n")
		  else ()
	  val _ = case (VarMap.find(varmap,v)) of
		  NONE => ()
		| SOME _ => error ("varmap already contains "
					    ^ (Name.var2string v))
	  val env = (NilContext.insert_exp(#1 env,v,e), #2 env)
	  val varmap = VarMap.insert(varmap,v,t)
      in  {is_top=is_top,env=env,varmap=varmap,convarmap=convarmap,gcstate=gcstate}
      end

  fun state_var_insert state (arg as (v,(t,_))) =
      (if (top_rep(v,t))
	   then global_state := state_var_insert' (!global_state) arg
       else ();
	state_var_insert' state arg)

  fun state_var_insert_eq state (arg as (v,(t,_))) =
      (if (top_rep(v,t))
	   then global_state := state_var_insert_eq' (!global_state) arg
       else ();
	state_var_insert_eq' state arg)

  fun env_insert' ({is_top,env,varmap,convarmap,gcstate} : state) (v,l,k) : state =
      let val _ = if (!debug_bound)
		      then (print "env adding v = ";
			    Ppnil.pp_var v; print "\n")
		  else ()
	  val (env1, env2) = env
	  val env1 = NilContext.insert_kind(env1,v,k)
	  val env1 =
	      case k of
		  Single_k c => NilContext.insert_con(env1, v, c)
		| _ => env1
	  val env2 = VarMap.insert(env2, v, ref NONE)
	  val newenv = (env1, env2)
	  val newstate = {is_top=is_top,env=newenv,
			  varmap=varmap,convarmap=convarmap,gcstate=gcstate}
      in  newstate
      end

  fun state_convar_insert' ({is_top,convarmap,varmap,env,gcstate}:state)
                           (v,l,(vr,k)) : state =
      let
	val _ = if (!debug_bound)
		  then (print "convar adding to v = "; Ppnil.pp_var v; print "\n")
		else ()
	val _ = (case (VarMap.find(convarmap,v)) of
		   NONE => ()
		 | SOME _ => error ("convarmap already contains "
				    ^ (Name.var2string v)))
	val convarmap = VarMap.insert(convarmap,v,vr)
      in
	{is_top=is_top,env=env,
	 varmap=varmap,convarmap=convarmap,gcstate=gcstate}
      end

  fun state_convar_insert state (arg as (v,l,(vr,k))) =
      let
	val state = state_convar_insert' state arg
	val state = env_insert' state (v,l,k)
	val _ = if (#is_top state)
		    then global_state := state_convar_insert' (!global_state) arg
		  else ()
      in  state
      end

  (* adding term-level variables and functions *)
  fun add_term_direct (s,v,con,term,NONE) = state_var_insert s (v,(term,con))
    | add_term_direct (s,v,con,term,SOME e) = state_var_insert_eq s (v,(term,e))

  (* adding constructor-level variables and functions *)
  fun add_conterm_direct (s,l,v,kind,termOpt) = state_convar_insert s (v,l,(termOpt, kind))

   fun getconvarrep' ({convarmap,...} : state) v = VarMap.find (convarmap,v)
   fun getconvarrep state v : convar_rep =
       (case getconvarrep' state v of
	    NONE => error ("getconvarrep: variable "^(Name.var2string v)^" not found")
	  | SOME result => result)

  fun getrep ({varmap=lm,...} : state) v =
      (case VarMap.find(lm,v) of
	   NONE => error ("getrep: variable "^(Name.var2string v)^" not found")
	 | SOME rep => rep)

    (* given a type returns true and a type in head-normal form
       or false and a type not in head-normal form
       in either case, the returned type is possibly simpler than the argument type *)

    fun simplify_type ({env,...} : state) con : bool * con =
	let val nilenv = #1 env
	    val result = subtimer("RTL_reduce_hnf",Normalize.reduce_hnf)(nilenv,con)
	    fun reduce_more (_, Prim_c(GCTag_c, [Var_c v])) = reduce_more (true, Prim_c(GCTag_c, [get_con (nilenv, v)]))
	      | reduce_more x = x

	    val result = reduce_more result

	    val _ = if (!debug_simp)
		    then (print (Real.toString (Stats.fetch_timer_last "RTL_reduce_hnf"));
			  print "s  simplify on\n";  Ppnil.pp_con con;
			  print "\nreduced to\n"; Ppnil.pp_con (#2 result);
			  print "\n")
		    else ()
	in  result
	end

  fun reduce_to_sum str ({env,...}:state) c =
      let fun slow() = subtimer("RTL_reduceToSum",Normalize.reduceToSumtype) (#1 env,c)
      in  (case c of
	       Var_c v =>
		   let val SOME cache = VarMap.find(#2 env,v)
		   in  (case (!cache) of
			    SOME suminfo => suminfo
			  | NONE => let val suminfo = slow()
					val _ = cache := SOME suminfo
				    in  suminfo
				    end)
		   end
	     | _ => slow())
      end

  fun reduce_to_arrow ({env,...}:state) c =
      Normalize.strip_arrow_norm (#1 env) c

  fun reduce_to_nondep_record ({env,...} : state) con =
      (case #2 (subtimer("RTL_reduce_hnf",Normalize.reduce_hnf) (#1 env,con)) of
	 Prim_c (Record_c labs, cons) => (labs, cons)
	 (*(case vlopt of
	    NONE => (labs,cons)
	  | SOME vs =>
	    let
	      fun loop ([],_,acc) = rev acc
		| loop ((v,c)::rest,rev_vcl,acc) =
		  let val c' = Normalize.removeDependence rev_vcl c (* should we use rev rev_vcl instead? *)
		  in loop (rest,(v,c')::rev_vcl,c'::acc)
		  end
	    in (labs,loop (Listops.zip vs cons,[],[]))
	    end)*)
       | _ => error "reduce_to_record didn't get a record type")


  val maxRtlRecord = Rtltags.maxRecordLength - 1      (* We reserve one slot from Rtl-generated record so that sums can be handled *)

  (* project_indices : int -> int list *)
  fun project_indices index =
      let val n = maxRtlRecord - 1	(* pds: this second "- 1" seems odd *)
	  fun loop (index, acc) =
	      if index >= 0 andalso index < n
		  then rev (index :: acc)
	      else
		  loop (index - n, n :: acc)
      in  loop (index, nil)
      end

  (* Takes a constructor and returns the RTL representation.
     The head-normal form must be statically known. That is, this constructor
     must not involve any computation to determine the RTL representation. *)

  fun cpath2indices (state : state) k labs =
      let
	  fun loop acc _ [] = rev acc
	    | loop acc (k as Record_k fields_seq) (label::rest) =
	      let
		  fun extract acc [] = (print "could not find label "; Ppnil.pp_label label;
					print " in the fields of "; Ppnil.pp_kind k; print "\n";
					error "bad Proj_c")
		    | extract acc (((l,_),fc)::rest) =
		      if (Name.eq_label(label,l)) then (fc,acc) else extract (acc+1) rest
		  val fields_list = (Sequence.toList fields_seq)
		  val (con,index) = extract 0 fields_list
		  val acc = if (!do_single_crecord andalso length fields_list = 1)
				then acc
			    else (index::acc)
	      in  loop acc con rest
	      end
	    | loop acc (Single_k c) labs =
	      let val k = subtimer("RTLkind_of0", NilContext.kind_of) (#1 (#env state),c)
	      in  loop acc k labs
	      end
	    | loop acc (SingleType_k c) labs =
	      loop acc Type_k labs
	    | loop acc _ labs = error "expect record kind"
	  val indices = loop [] k labs
	  val indices = List.concat (map project_indices indices)
	  val _ = debugdo (fn () =>
			   (print "cpath2indices ";
			    Ppnil.pp_kind k;
			    print " = [ ";
			    app (fn i => print (Int.toString i ^ " ")) indices;
			    print "]\n"))
      in  indices
      end



   fun location2rep(REGISTER(_,I (SREGI HEAPALLOC))) = NOTRACE_INT
     | location2rep(REGISTER(_,I (SREGI HEAPLIMIT))) = NOTRACE_INT
     | location2rep(REGISTER(_,I (SREGI EXNSTACK))) = TRACE
     | location2rep(REGISTER(_,I (SREGI EXNARG))) = TRACE
     | location2rep(REGISTER(_,I (SREGI STACK))) = NOTRACE_INT
     | location2rep(REGISTER(_,I (SREGI THREADPTR))) = NOTRACE_INT
     | location2rep(REGISTER(_,I (SREGI HANDLER))) = NOTRACE_CODE
     | location2rep(REGISTER(_,I (REGI (_,rep)))) = rep
     | location2rep(REGISTER(_,F (REGF (_, NOTRACE_REAL)))) = NOTRACE_REAL
     | location2rep(REGISTER(_,F _)) = error "float reg has weird rep. not NOTRACE_REAL"
     | location2rep(GLOBAL(_,rep)) = rep
   fun value2rep(VOID rep) = rep
     | value2rep(INT _) = NOTRACE_INT
     | value2rep(TAG _) = TRACE
     | value2rep(REAL _) = NOTRACE_REAL
     | value2rep(RECORD _) = TRACE
     | value2rep(LABEL _) = TRACE
     | value2rep(CODE _) = NOTRACE_CODE
   fun term2rep(LOCATION loc) = location2rep loc
     | term2rep(VALUE value) = value2rep value

   (* Conservatively guarantees that value of this rep is not a pointer into the heap *)
   fun repIsNonheap rep =
       (case rep of
	    NOTRACE_INT => true
	  | NOTRACE_CODE => true
	  | NOTRACE_REAL => true
	  | NOTRACE_LABEL => true  (* global labels are not in heap *)
	  | _ => false)

   fun niltrace2rep (state : state) niltrace : rep =
       let fun pathcase (v,labs) =
	 (* XXX: Why getconvarrep', not getconvarrep?   joev *)
	   (case getconvarrep' state v of
	      NONE => error ("getconvarrep' of bad variable in niltrace2rep: " ^ Name.var2string v)
	      | SOME topt =>
		(case topt of
		   (SOME (VALUE(RECORD (l,_)))) => (COMPUTE(Projlabel_p (l,labs)))
		 | (SOME (VALUE(LABEL l))) => (COMPUTE(Projlabel_p (l,labs)))
		 | (SOME (VALUE(VOID _))) => error "constructor is void"
		 | (SOME (VALUE(REAL _))) => error "constructor represented as a float"
		 | (SOME (VALUE(CODE _))) => error "constructor function cannot be a type"
		 | (SOME (LOCATION(REGISTER (_,I r)))) => (COMPUTE(Projvar_p (r,labs)))
		 | (SOME (LOCATION(REGISTER (_,F _)))) => error "constructor in float reg"
		 | (SOME (LOCATION(GLOBAL (l,_)))) => (COMPUTE(Projglobal_p(l,labs)))
		 | (SOME (VALUE _)) => (print "niltrace2rep convar = ";
					print (Name.var2string v); print "\n";
					error "inappropriate information for this convar!!")
		 | NONE => (print "niltrace2rep convar = ";
			    print (Name.var2string v); print "\n";
			    error "this is a compiletime convar!!")))

       in
	   (case niltrace of
		Nil.TraceUnknown => error "TraceUnknown in niltrace2rep"
	      | (Nil.TraceCompute v) => pathcase(v,[])
	      | (Nil.TraceKnown t) =>
		    (case t of
			 TraceInfo.Trace => TRACE
		       | TraceInfo.Unset => UNSET
		       | TraceInfo.Notrace_Int => NOTRACE_INT
		       | TraceInfo.Notrace_Code => NOTRACE_CODE
		       | TraceInfo.Notrace_Real => NOTRACE_REAL
		       | TraceInfo.Label => NOTRACE_LABEL
		       | TraceInfo.Locative => LOCATIVE
		       | TraceInfo.Compute (v,labs) =>
			     let val indices =
				 (case labs of
				      [] => []
				    | _ => let val k = std_kind_of state (Var_c v)
					   in cpath2indices state k labs
					   end)
			     in  pathcase(v,indices)
			     end))
       end


  (* ----- Data structures for the current function being compiled
   il: list of instructions for the current function being compiled
   localregs : computing SAVE sets for RTL interpreter
   top: label at top of current function (past prelude code)
   currentfun: the current function being compiled
   argregs : the argument registers
   ---> If top and currentfun and NONE, then we are at top-level
        and the register lists will also be empty.   <--- *)



   local
       val istop : bool ref = ref false
       val top : label ref = ref (LOCAL_CODE "dummy_top")
       val currentfun : (var * label) ref = ref (Name.fresh_named_var "dummy_fun",
					       LOCAL_CODE "dummy_fun")
       val resultreg : reg option ref = ref NONE
       val il : (Rtl.instr ref) list ref = ref nil
       val localregs : reg list ref = ref nil
       val returnreg : regi option ref = ref NONE
       val argregs : reg list ref = ref nil
       val curgc : instr ref option ref = ref NONE
       val gcstack : instr ref option list ref = ref []
       fun add_instr' i = il := (ref i) :: !il;
   in
       fun istoplevel() = !istop
       fun getTop() = !top
       fun getCurrentFun() = !currentfun
       fun getResult thunk =
	   (case !resultreg of
		SOME r => r
	      | NONE => let val r = thunk()
			    val _ = localregs := (r :: (!localregs))
			    val _ = resultreg := SOME r
			in  r
			end)
       fun getLocals() = !localregs
       fun getArgs() = !argregs

       fun dataLength (COMMENT _) = 0
	 | dataLength (DLABEL _) = 0
	 | dataLength (STRING string) = ((size string) + 3) div 4
	 | dataLength (INT32 _) = 1
	 | dataLength (FLOAT _) = 2
	 | dataLength (DATA _) = 1

       fun add_data d =
	   (dl := d :: !dl;
	    dw := (dataLength d) + !dw)

       fun oddlong_align() = if ((!dw) mod 2 = 1)
				 then ()
			     else (add_data(COMMENT "alignment");
				   add_data (INT32 (Rtltags.skip 1)))


       fun join_states ([] : state list) : state = error "join_states got []"
	 | join_states ({is_top,env,varmap,convarmap,gcstate}::restStates) =
	   let (* we must not have duplicates or else we could get exponential blowup *)
		fun instrref_eq(a : instr ref, b) = a = b
		fun instrreflist_join(a : instr ref list, b : instr ref list) = Listops.list_sum_eq(instrref_eq, a, b)
		fun join (ALLOC_IMM a, ALLOC_IMM b) = ALLOC_IMM(instrreflist_join(a,b))
		  | join (MUTATE_IMM a, MUTATE_IMM b) = MUTATE_IMM(instrreflist_join(a,b))
		  | join _ = GC_UNKNOWN
		val gcstate = foldl join gcstate (map #gcstate restStates)
	   in   {is_top=is_top,env=env,varmap=varmap,convarmap=convarmap,gcstate=gcstate}
	   end

       fun shadow_state	({is_top,env,varmap,convarmap,gcstate=_},{gcstate,...}:state) =
	   {is_top=is_top,env=env,varmap=varmap,convarmap=convarmap,gcstate=gcstate}

       fun add_instr (NEEDALLOC _) = error "Use needallocc to add a NEEDALLOC"
	 | add_instr (NEEDMUTATE _) = error "Use needmutate to add a NEEDMUTATE"
	 | add_instr i = add_instr' i

       fun flushgc(state as {is_top,gcstate,env,convarmap,varmap}:state, instr) : state =
	   let val r = ref instr
	       val _ = il := (r :: (!il))
	       val gcstate = (case instr of
				  NEEDALLOC (IMM _) => ALLOC_IMM [r]
				| NEEDALLOC INF => GC_UNKNOWN
				| NEEDMUTATE _ => MUTATE_IMM [r]
				| _ => error "flushgc called with weird instr")
	   in  {is_top=is_top,env=env,convarmap=convarmap,varmap=varmap,
		gcstate=gcstate}
	   end

       fun needalloc(state as {gcstate,...}, operand) : state =
	 if (not (!do_gcmerge))
	     then (incGC(); add_instr'(NEEDALLOC operand); state)
	 else
	     (case (gcstate, operand) of
		  (ALLOC_IMM instrRef, IMM m) =>
		      let fun loop commit [] = true
			    | loop commit ((ir as ref (NEEDALLOC (IMM n)))::rest) =
			       (if commit
				    then (if (n = 0) then incGC() else ();
					      ir := NEEDALLOC (IMM (m+n)))
				else ();
				    if (m+n < maxAllocRequest) then loop commit rest else false)
			    | loop _ _ = error "bad ALLOC_IMM"
		      in  if (loop false instrRef)
			      then (loop true instrRef; state)
			  else flushgc(state, NEEDALLOC operand)
		      end
		| _ => ((case operand of
			     IMM 0 => ()
			   | _ => incGC());
			flushgc(state, NEEDALLOC operand)))

       fun needmutate (state as {gcstate,...}, n) : state =
	 if (not (!do_gcmerge))
	     then (incGC(); add_instr'(NEEDMUTATE n); state)
	 else (case gcstate of
		   MUTATE_IMM instrRef =>
		       let fun loop commit [] = true
			     | loop commit ((ir as (ref (NEEDMUTATE m)))::rest) =
			          (if commit
				       then ir := NEEDMUTATE(m + n)
				   else ();
				   if (m+n < maxMutateRequest)
				       then loop commit rest
				   else false)
			     | loop _ _ = error "bad MUTATE_IMM"
		       in  if (loop false instrRef)
			       then (loop true instrRef; state)
			   else flushgc(state,NEEDMUTATE n)
		       end
		 | _ => flushgc(state,NEEDMUTATE n))

       fun new_gcstate ({is_top,env,varmap,convarmap,gcstate} : state) : state =
	   let val s = {is_top=is_top,env=env,varmap=varmap,convarmap=convarmap,gcstate=GC_UNKNOWN}
	   in  needalloc(s,IMM 0)
	   end


       fun alloc_named_regi v traceflag =
	   let val r = REGI (v,traceflag)
	   in  localregs := (I r) :: (!localregs);
	       r
	   end
       fun alloc_regi (traceflag) = alloc_named_regi (Name.fresh_var()) traceflag


       fun alloc_named_regf v =
	   let val r = REGF (v,NOTRACE_REAL)
	   in  localregs := (F r) :: (!localregs);
	       r
	   end
       fun alloc_regf () = alloc_named_regf (Name.fresh_var())

       fun alloc_reg_trace state trace =
	   let val rep = niltrace2rep state trace
	   in  case rep of
	       NOTRACE_REAL => (rep,F(alloc_regf()))
	     | _ => (rep,I(alloc_regi rep))
	   end



       fun promote_maps ({env,...} : state) : state =
	   let val {varmap,convarmap,gcstate,...} = !global_state
	       fun isNotReg_exp(LOCATION (REGISTER _)) = false
		 | isNotReg_exp _ = true
	       fun isNotReg_con(SOME (LOCATION (REGISTER _))) = false
		 | isNotReg_con _ = true
	       val varmap = VarMap.filter isNotReg_exp varmap
	       val convarmap = VarMap.filter isNotReg_con convarmap
	   in  {is_top = false, varmap = varmap,
		convarmap = convarmap, env = env, gcstate = gcstate}
	   end

       fun set_args (args,return) =
	   (argregs := args;
	    returnreg := SOME return;
	    localregs := (I return) :: args)

       fun reset_state (is_top,names) =
	   (resultreg := NONE;
	    istop := is_top;
	    currentfun := names;
	    top := fresh_code_label "funtop";
	    il := nil;
	    add_instr(ILABEL (!top)))


       fun add_global v = globals := Name.VarSet.add(!globals, v)
       fun is_global v = Name.VarSet.member(!globals, v)

       fun set_global_state (un,exportlist,gl) =
	   let fun exp_adder((v,l),m) = (case VarMap.find(m,v) of
					     NONE => VarMap.insert(m,v,[l])
					   | SOME ls => VarMap.insert(m,v,l::ls))
	       fun gl_adder(v,s) = VarSet.add(s,v)
	   in  (unitname := un;
		global_state :=  make_state();
		exports := (foldl exp_adder VarMap.empty exportlist);
		globals := gl;
		dw := 0;
		dl := nil;
		pl := nil;
		reset_records();
		reset_state(false,(Name.fresh_named_var "code", fresh_code_label "code"));
		clear_stats())
	   end
       fun unset_global_state() = (if (!diag) then show_stats() else ();
				   set_global_state("",[],VarSet.empty));


       fun get_unitname() = !unitname
       fun get_code() = map ! (rev (!il))
       fun get_proc() =
	let val (_,name) = !currentfun
	    val code =  map ! (rev (!il))
	    val SOME return = !returnreg
	    val results = (case !resultreg of
			       SOME r => [r]
			     | NONE => [])
	in  PROC{name    = name,
		 return  = return,
		 args    = !argregs,
		 results = results,
		 code    = Array.fromList code,
		 save    = nil,
		 vars    = NONE}
	end

   end






(* ---------  Helper Functions ------------------------------------------- *)
    val w2i = TW32.toInt
    val i2w = TW32.fromInt;



  fun in_imm_range_vl (VALUE(INT w)) = ((if in_imm_range w then SOME (w2i w) else NONE) handle _ => NONE)
   | in_imm_range_vl (VALUE(TAG w)) = ((if in_imm_range w then SOME (w2i w) else NONE) handle _ => NONE)
    | in_imm_range_vl _ = NONE
  fun in_ea_range scale (VALUE(INT i)) =
      ((if in_ea_disp_range(w2i i * scale)
	    then SOME (w2i i * scale)
	else NONE)
	    handle _ => NONE)
    | in_ea_range scale (VALUE(TAG i)) =
      ((if in_ea_disp_range(w2i i * scale)
	    then SOME (w2i i * scale)
	else NONE)
	    handle _ => NONE)
    | in_ea_range _ _ = NONE



  val heapptr  = SREGI HEAPALLOC
  val stackptr = SREGI STACK
  val exnptr   = SREGI EXNSTACK
  val exnarg   = SREGI EXNARG


  (* ------------------- End of Helper Functions -------------------------- *)



  (* --------- Tag Operation Helper Functions -------------------- *)

  (* for reals, the len reg measures quads *)

  (* all lengths measured in bytes *)
  local
    open Rtltags
      fun mk_arraytag (granularity,offset,tagDesc) (byteLen,dest) =
	  let (*
	      val _ = if (byteLen mod granularity <> 0)
			  then error "mk_arraytag - byte length is incorrect multiple"
		      else ()
              *)
	  in  add_instr(SLL(byteLen,IMM offset,dest));
	      add_instr(ORB(dest,IMM (w2i tagDesc),dest))
	  end
  in
      val mk_word_arraytag = mk_arraytag (1, word_array_len_offset, wordarray)
      val mk_quad_arraytag = mk_arraytag (8, quad_array_len_offset, quadarray)
      val mk_ptr_arraytag = mk_arraytag  (4, ptr_array_len_offset, ptrarray)
      val mk_mirror_ptr_arraytag = mk_arraytag (8, mirror_ptr_array_len_offset, mirrorptrarray)
      val mk_recordtag = recordtag
  end

  (* storing a tag at the allocation point. *)
  fun store_tag_zero tag =
    let val tmp = alloc_regi(NOTRACE_INT)
      in add_instr(LI(tag,tmp));
	 add_instr(STORE32I(REA(heapptr,0),tmp)) (* store tag *)
    end

  (* ----- functions for loading NIL values into RTL registers ----------- *)


  (* ---------------------------------------------------------------------------
   the following are functions that load integer registers or float registers
   or load an sv; for int/float regs, one can optionally specify a dest register *)


  (* if (reg != 0) then thenClause else elseClause *)
  fun ifthenelse (reg, thenInstr, elseInstr) =
      let val elsel = fresh_code_label "else_case"
	  val afterl = fresh_code_label "after_ite"
      in  (add_instr(BCNDSI(EQ,reg,IMM 0,elsel,false));
	   app add_instr thenInstr;
	   add_instr(BR afterl);
	   add_instr(ILABEL elsel);
	   app add_instr elseInstr;
	   add_instr(ILABEL afterl))
      end

  fun loadPtrGlobal (label, global) =
      let val addr = alloc_regi NOTRACE_LABEL
	  val offset = alloc_regi NOTRACE_INT
	  val instr = LADDR(LEA(label,0),addr)
      in  if (!mirrorGlobal)
	      then instr::[(MIRROR_GLOBAL_OFFSET offset),
			   (LOAD32I(RREA(addr,offset),global))]
	  else instr::[(LOAD32I(REA(addr,0),global))]
      end

  (* record_project : regi * int list * regi -> unit *)
  fun record_project' (src, nil, dest) = raise Match
    | record_project' (src, index :: nil, dest) = add_instr(LOAD32I(REA(src,4*index), dest))
    | record_project' (src, index :: indices, dest) =
      let val tmp = alloc_regi TRACE
	  val _ = add_instr(LOAD32I(REA(src,4*index), tmp))
      in  record_project'(tmp,indices,dest)
      end

  (* record_project : regi * int * regi -> unit *)
  fun record_project (src, index, dest) = record_project' (src, project_indices index, dest)

  fun repPathIsPointer repPath =
      let fun loop (r,[]) = let
				val result = alloc_regi NOTRACE_INT
				val _ = add_instr(CMPUI(GT,r,IMM 3,result))
			    in  result
			    end
	    | loop (r,index::rest) = let val s = alloc_regi TRACE
	                                 (* record_project (r,index,s) is overkill
					    because index ultimately comes from project_indices *)
					 val _ = if (index < maxRtlRecord)
						     then ()
						 else error ("rep_path index too big: " ^ Int.toString index)
					 val _ = add_instr(LOAD32I(REA(r,4*index),s))
				     in  loop(s,rest)
				     end
      in  (case repPath of
	       Notneeded_p => error "repPathIsPointer called on Notneeded_p"
	     | Projvar_p (r,ind) => loop(r,ind)
	     | Projlabel_p (lab,ind) => let val addr = alloc_regi NOTRACE_LABEL
					    val _ = add_instr(LADDR(LEA(lab,0),addr))
					in  loop(addr,ind)
					end
	     | Projglobal_p (lab,ind) => let val global = alloc_regi TRACE
					     val instrs = loadPtrGlobal (lab,global)
					     val _ = app add_instr instrs
					 in  loop(global,ind)
					 end)
      end

    (* --- load an RTL location into an integer register --- *)
    fun load_ireg_loc (loc : location, destOpt : regi option) =
	(case loc of
	     GLOBAL(l,NOTRACE_REAL) => error "load_ireg called with (GLOBAL real)"
	   | GLOBAL(label,rep) => let val dest = (case destOpt of
						      NONE => alloc_regi rep
						    | SOME d => d)
				  in  (case rep of
					   TRACE => let val instrs = loadPtrGlobal(label,dest)
						    in  app add_instr instrs
						    end
					 | COMPUTE rtl_rep =>
					       let val isPtr = repPathIsPointer rtl_rep
					       in  ifthenelse(isPtr,
							      loadPtrGlobal(label,dest),
							      [LOAD32I(LEA(label,0),dest)])
					       end
					 | _ => add_instr(LOAD32I(LEA(label,0),dest)));
				      dest
				  end
	   | REGISTER (_,I ir) => (case destOpt of
					NONE => ir
				      | SOME d => (add_instr(MV(ir,d)); d))
	   | REGISTER (_,F _) => error "load_ireg called with (REGISTER (_, F _))")


    fun load_ireg_val (value : value, destOpt : regi option) : regi =
      let
	  fun help(rep,mk_instr) = (case destOpt of
				    NONE => let val r = alloc_regi rep
						val _ = add_instr(mk_instr r)
					    in  r
					    end
				  | SOME d => (add_instr(mk_instr d); d))
      in case value of
	  (* zero is a safe bit-pattern for values of any rep *)
	  VOID rep => help(rep, fn r => LI(0w0,r))
	| INT i => help (NOTRACE_INT, fn r => LI(i,r))
	| TAG i => help (TRACE, fn r => LI(i,r))
	| REAL _ => error "load_ireg: REAL"
	| RECORD(label,_) => help (TRACE, fn r => LADDR(LEA(label,0),r))
	| LABEL label => help (TRACE, fn r => LADDR(LEA(label,0),r))
	| CODE label => help (NOTRACE_CODE, fn r => LADDR(LEA(label,0),r))
      end


    fun mk_named_float_data (r : string, label : label) =
	(oddlong_align();
	 add_data(INT32 (Rtltags.mk_quad_array_tag (i2w 8)));
	 add_data(DLABEL label);
	 add_data(FLOAT r))

    fun mk_float_data (r : string) : label =
	let val label = fresh_data_label "floatdata"
	    val _ = mk_named_float_data (r,label)
	in label
	end

    fun load_freg_loc (rep : location, destOpt : regf option) : regf =
      (case rep of
	  (REGISTER (_,F r)) => (case destOpt of
				      NONE => r
				    | SOME dest => (add_instr(FMV(r,dest)); dest))
	| (GLOBAL (l,NOTRACE_REAL)) => let val dest = (case destOpt of
							   NONE => alloc_regf()
							 | SOME d => d)
				       in  add_instr(LOAD64F(LEA(l,0),dest));
					   dest
				       end
	| (REGISTER (_,I r)) => error "load_freg_loc called on REGISTER (_, I _)"
	| (GLOBAL _) => error "load_freg_loc: got GLOBAL(_, non-NOTRACE_REAL)")


    fun load_freg_val (rep : value, destOpt : regf option) : regf =
	(case rep of
	     VOID rep => (print "WARNING: load_freg on VOID\n"; alloc_regf())
	   | REAL label =>
		 let val r = (case destOpt of
				  NONE => alloc_regf()
				| SOME d => d)
		 in  add_instr(LOAD64F(LEA(label,0),r));
		     r
		 end
	   | INT _ => error "load_freg_val: got INT"
	   | TAG _ => error "load_freg_val: got TAG"
	   | RECORD _ => error "load_freg_val: got RECORD"
	   | LABEL _ => error "load_freg_val: got LABEL"
	   | CODE _ => error "load_freg_val: got CODE")

    fun load_reg_loc (rep : location, destopt : reg option) : reg =
	let val rep_is_float = (case rep of
				    REGISTER (_, F _) => true
				  | GLOBAL (_, NOTRACE_REAL) => true
				  | _ => false)
	in  if (rep_is_float)
	    then
		 (case destopt of
		      NONE => F(load_freg_loc(rep, NONE))
		    | SOME (I ir) => error "load_reg on a FLOAT rep and a SOME(I _)"
		    | SOME (F fr) => F(load_freg_loc(rep, SOME fr)))
	    else (case destopt of
		      NONE => I(load_ireg_loc(rep, NONE))
		    | SOME (F fr) => error "load_freg on a FLOAT rep and a SOME(F _)"
		    | SOME (I ir) => I(load_ireg_loc(rep, SOME ir)))
	end

    fun load_reg_val (value : value, destopt : reg option) : reg =
	(case value of
	     REAL _ =>
		 (case destopt of
		      NONE => F(load_freg_val(value, NONE))
		    | SOME (I ir) => error "load_reg_val on a FLOAT rep and a SOME(I _)"
		    | SOME (F fr) => F(load_freg_val(value, SOME fr)))
	   | _ =>
		 (case destopt of
		      NONE => I(load_ireg_val(value, NONE))
		    | SOME (F fr) => error "load_freg_val on a FLOAT rep and a SOME(F _)"
		    | SOME (I ir) => I(load_ireg_val(value, SOME ir))))

    fun load_reg_term (LOCATION loc, destOpt) = load_reg_loc(loc,destOpt)
      | load_reg_term (VALUE value, destOpt) = load_reg_val(value,destOpt)

    fun load_ireg_term(LOCATION vl, poss_dest) = load_ireg_loc(vl,poss_dest)
      | load_ireg_term(VALUE vv, poss_dest) = load_ireg_val(vv,poss_dest)
    fun load_freg_term(LOCATION vl, poss_dest) = load_freg_loc(vl,poss_dest)
      | load_freg_term(VALUE vv, poss_dest) = load_freg_val(vv,poss_dest)
    fun load_reg_term(LOCATION vl, poss_dest) = load_reg_loc(vl,poss_dest)
      | load_reg_term(VALUE vv, poss_dest) = load_reg_val(vv,poss_dest)
    fun load_ireg_sv(vl as (VALUE(INT i))) =
	if (in_imm_range i) then IMM(TW32.toInt i) else REG(load_ireg_term(vl,NONE))
      | load_ireg_sv(vl as (VALUE(TAG i))) =
	    if (in_imm_range i) then IMM(TW32.toInt i) else REG(load_ireg_term(vl,NONE))
      | load_ireg_sv vl = REG(load_ireg_term(vl,NONE))


  (* --------- end of load(sv/int/float) ------------------------------------------ *)



  (* --------- Common Code Sequence Helper Functions ------------------------------ *)

  (* by possibly adding 4 to heapptr, make it odd-quadword aligned;
   if addition took place, store a skip tag first *)
  fun align_odd_word () =
    let val tmp0 = alloc_regi(NOTRACE_INT)
      val tmp1 = alloc_regi(TRACE)
    in  add_instr(LI(Rtltags.skip 1,tmp0));
	add_instr(STORE32I(REA(heapptr,0),tmp0));  (* store a skiptag *)
	add_instr(ANDB(heapptr,IMM 4,tmp0));
	add_instr(ADD(heapptr,IMM 4,tmp1));
	add_instr(CMV(EQ,tmp0,REG tmp1,heapptr))
    end
  (* by possibly adding 4 to heapptr, make it even-quadword aligned;
   if addition took place, store a skip tag first *)
  fun align_even_word () =
    let val tmp0 = alloc_regi(NOTRACE_INT)
      val tmp1 = alloc_regi(NOTRACE_INT)
    in add_instr(LI(Rtltags.skip 1,tmp0));
      add_instr(STORE32I(REA(heapptr,0),tmp0)); (* store a skiptag *)
      add_instr(ANDB(heapptr,IMM 4,tmp0));
      add_instr(ADD(heapptr,IMM 4,tmp1));
      add_instr(CMV(NE,tmp0,REG tmp1,heapptr))
    end


  fun add (reg,i : int,dest) =
    if in_imm_range (i2w i) then
      add_instr(ADD(reg,IMM i,dest))
    else if in_ea_disp_range i then
	add_instr(LADDR(REA(reg,i),dest))
         else let val size = alloc_regi(NOTRACE_INT)
              in add_instr(LI (i2w i,size));
		  add_instr(ADD(reg,REG size,dest))
	      end



  (* Real globals (NOTRACE_REAL) require alignment.
     Pointer globals may require mirroring.
     Unknown globals (int or pointer)'s tags cannot be statically determined.
     Uninitialized pointers or unknowns have a skip tag (two or three words) until it is initialized.
  *)

    (* sometime the tags must be computed at run-time *)
    fun add_dynamic (r,dynamic) =
	let fun do_one {bitpos,path} =
	    let val isPointer = repPathIsPointer path
		val tmp = alloc_regi NOTRACE_INT
	    in  add_instr(SLL(isPointer,IMM bitpos,tmp));
		add_instr(ORB(tmp,REG r,r))
	    end
	in  app do_one dynamic
	end

    local
	(* Some values used below *)
	val {static=recIntTag,dynamic=[]} = mk_recordtag [NOTRACE_INT]
	val {static=recPtrTag,dynamic=[]} = mk_recordtag [TRACE]
	val skip1 = Rtltags.skip 1
	val skip2 = Rtltags.skip 2
	val skip3 = Rtltags.skip 3
	val quadArrayTag = Rtltags.mk_quad_array_tag 0w8
	val uninit = INT32 Rtltags.uninitVal

	fun initPtrGlobal (state,label, global) =
	    let val addr = alloc_regi NOTRACE_LABEL
		val offset = alloc_regi NOTRACE_INT
		val state = needmutate(state,1)
		val instr1 = LADDR(LEA(label,0),addr)
		val (instr2,ea) =  if (!mirrorGlobal)
				       then ([MIRROR_GLOBAL_OFFSET offset], RREA(addr,offset))
				   else ([], REA(addr,0))
		val instr3 = [STOREMUTATE(ea, PTR_MUTATE),
			      STORE32I(ea, global)]
	    in  (state, instr1 :: (instr2 @ instr3))
	    end

    in

      fun allocate_global(state,label,labels,rtl_rep,lv) =
	let
	  val _ = incGlobal()
	  val _ = add_data(COMMENT "Global")

	  (* Do the static part.  Tag is always known statically since otherwise a skip tag is used *)
	  fun staticPortion (tag, data) =
	      (add_data (INT32 tag);
	       app (fn l => add_data(DLABEL l)) (label::labels);
	       app add_data data);

	  val state =
	      (case lv of
		   LOCATION (REGISTER (_, F fr)) => (oddlong_align();
						     staticPortion(quadArrayTag, [FLOAT "0.0"]);
						     add_instr (STORE64F(LEA(label,0), fr));
						     state)
		 (* Is this case possible? XXXX *)
		 | LOCATION (GLOBAL (l, NOTRACE_REAL)) => let val fr = alloc_regf()
							  in  oddlong_align();
							      staticPortion(quadArrayTag, [FLOAT "0.0"]);
							      add_instr (LOAD64F(LEA(l,0), fr));
							      add_instr (STORE64F(LEA(label,0), fr));
							      state
							  end
		 | LOCATION loc =>
		       let val tag = alloc_regi NOTRACE_INT
			   val (ir,rep) = (case loc of
					       REGISTER (_, I (ir as (REGI (_, rep)))) => (ir, rep)
					     | GLOBAL (l, rep) => (load_ireg_loc(loc,NONE), rep))
		       in  (case rep of
				LOCATIVE => error "global locative"
			      | UNSET => error "global unset"
			      | NOTRACE_LABEL => error "global label"
			      | NOTRACE_REAL => error "global real"
			      | TRACE => let val _ = add_static_record (label, 0, 1)
					     val _ = if (!mirrorGlobal)
							 then (staticPortion(skip3, [uninit, uninit]);
							       add_instr(LI(mirrorGlobalTag, tag)))
						     else (staticPortion(skip2, [uninit]);
							   add_instr(LI(recPtrTag, tag)))
					     val _ = add_instr (STORE32I(LEA(label,~4), tag))
					     val (state,instrs) = initPtrGlobal(state,label, ir)
					     val _ = app add_instr instrs
					 in  state
					 end
			      | COMPUTE rep =>
					     let val _ = add_static_record (label, 0, 1)  (* Might be *)
						 val isPtr = repPathIsPointer rep
					     in  if (!mirrorGlobal)
						     then
							 let val _ = staticPortion(skip3, [uninit, uninit])
							     val (state,instrs) = initPtrGlobal(state,label,ir)
							 in  ifthenelse(isPtr,
									[LI(mirrorGlobalTag, tag),
									 STORE32I(LEA(label,~4), tag)] @
									instrs,
									[LI(recIntTag, tag),
									 STORE32I(LEA(label,~4), tag),
									 LI(skip1, tag),
									 STORE32I(LEA(label,4), tag),
									 STORE32I(LEA(label,0), ir)]);
							     state
							 end
						 else
						     let val _ = staticPortion(skip2, [uninit])
							 val (state,instrs) = initPtrGlobal(state,label,ir)
							 val _ = ifthenelse(isPtr,
									    [LI(recPtrTag, tag),
									     STORE32I(LEA(label,~4), tag)] @
									    instrs,
									    [LI(recIntTag, tag),
									     STORE32I(LEA(label,~4), tag),
									     STORE32I(LEA(label,0), ir)])
						     in  state
						     end
					     end
			      | _ => (staticPortion(skip2, [uninit]);
				      add_instr (STORE32I(LEA(label,0), ir));
				      state))
		       end
		 | VALUE (REAL l) => let val fr = alloc_regf()
				     in  oddlong_align();
					 staticPortion(quadArrayTag, [FLOAT "0.0"]);
					 add_instr (LOAD64F(LEA(l,0), fr));
					 add_instr (STORE64F(LEA(label,0), fr));
					 state
				     end
		 | VALUE (VOID _) => (print "Warning: alloc_global got a VOID\n";
				      staticPortion(recIntTag, [INT32 0w0]);
				      state)
		 | VALUE (INT w32) => (staticPortion(recIntTag, [INT32 w32]); state)
		 | VALUE (CODE l) => (staticPortion(recIntTag, [DATA l]); state)
		 (* Although the following values are not from the heap,
		    they still have a trace-able type and so must be mirrored
		    but does not need to go through a write-barrier *)
		 | VALUE v =>
		     let val datum = (case v of
					  TAG w32 => INT32 w32
					| RECORD (l, _) => DATA l
					| LABEL l => DATA l
					| _ => error "impossible control flow")
		     in  if (!mirrorGlobal)
			     then staticPortion(mirrorGlobalTag, [datum, datum])
			 else (staticPortion(recPtrTag, [datum]));
			 state
		     end)
	in  state
	end
    end (* local *)

  fun help_global (add_obj, (state,v, obj, term, obj2)) : state =
    let
	val _ = Stats.counter_inc(Stats.counter("RTLglobal"))
	val (exported,label,labels) = (case (Name.VarMap.find(!exports,v)) of
					   SOME (lab::rest) => (true,lab,rest)
					 | SOME [] => error "no labels in export entry"
					 | NONE => (false,LOCAL_DATA (Name.var2string v),[]))
	val rep = term2rep term

	val isReg =
	    (case term of
		 LOCATION (REGISTER _) => true
	       | _ => false)

	val state = if (exported orelse isReg)
			then (allocate_global(state,label,labels,rep,term))
		    else state

	val term =
	    (case term of
		 LOCATION (REGISTER _) => LOCATION(GLOBAL(label,rep))
	       | _ => term)

    in  add_obj (state,v,obj,term,obj2)
    end

  val add_term =
      fn arg as (s,v,con,term,e) =>
      if (Name.VarSet.member(!globals,v))
	  then help_global(add_term_direct, arg)
      else add_term_direct(s,v,con,term,e)
  fun add_reg (s,v,con,reg) = add_term (s,v,con,LOCATION(REGISTER(false,reg)), NONE)
  fun add_code(s,v,con,l)   = add_term (s,v,con,VALUE(CODE l),NONE)

  fun add_conglobal (state : state,
		     l : Nil.label option,
		     v : var,
		     kind : kind,
		     termOpt : term option) : state =
    let
	val _ = Stats.counter_inc(Stats.counter("RTLconglobal"))
	val (exported,label,labels) = (case (Name.VarMap.find(!exports,v)) of
					   SOME (lab::rest) => (true,lab,rest)
					 | SOME [] => error "no labels in export entry"
					 | NONE => (false,LOCAL_DATA (Name.var2string v),[]))
	val is_reg =
	    (case termOpt of
		 SOME(LOCATION(REGISTER _)) => true
	       | _ => false)

	val state = (case (termOpt, exported orelse is_reg) of
			 (SOME term, true) => (allocate_global(state,label,labels,TRACE,term))
		       | _ => state)

	val termOpt =
	    (case termOpt of
		 SOME(LOCATION (REGISTER _)) => SOME(LOCATION(GLOBAL(label,TRACE)))
	       | _ => termOpt)

	val state' = add_conterm_direct (state, l, v, kind, termOpt)

    in state'
    end

  val add_conterm =
      fn arg as (_,_,v,_,_) =>
      if (Name.VarSet.member(!globals,v))
	  then add_conglobal arg
      else add_conterm_direct arg

  fun unboxFloat regi : regf = let val fr = alloc_regf()
				   val _ = add_instr(LOAD64F(REA(regi,0),fr))
			       in  fr
			       end

  fun boxFloat (state,regf) : regi * state =
      let val dest = alloc_regi TRACE
	  val state = needalloc(state,IMM 4)
	  val _ = (align_odd_word();
		   store_tag_zero(Rtltags.mk_quad_array_tag (i2w 8));
		   add_instr(STORE64F(REA(heapptr,4),regf));
		   add(heapptr,4,dest);
		   add(heapptr,12,heapptr))
      in  (dest,state)
      end

  fun boxFloat_vl (state,lv) : term * state =
      (case lv of
	  LOCATION(REGISTER (_,F fr)) => let val (ir,state) = boxFloat(state,fr)
				       in  (LOCATION(REGISTER (false,I ir)), state)
				       end
	| LOCATION(REGISTER (_,I _)) => error "can't box an int reg"
	| LOCATION(GLOBAL (l,_)) => (VALUE(LABEL l), state)
	| VALUE(REAL l) => (VALUE(LABEL l), state)
	| VALUE _ => error "can't box a non-REAL value")

 (* code for allocating an fp array at run-time given a list of LOCATIONs: return an ireg *)

 fun fparray (state, val_locs : term list) : regi * state =
    let
      val res = alloc_regi TRACE
      val len = length val_locs
      fun scan (nil,_) = ()
	| scan (h::t,offset) =
	  let val src = load_freg_term (h, NONE)
	  in  add_instr(STORE64F(REA(res,offset),src));
	      scan(t,offset+8)
	  end
      val state = needalloc(state,IMM((if len = 0 then 1 else 2*len)+2))
      val _ = (align_odd_word();
	       store_tag_zero(Rtltags.mk_quad_array_tag(i2w (8 * len)));
	       add_instr(ADD(heapptr,IMM 4,res)))

    in
       scan (val_locs,0);
       if (len = 0)
	   then add_instr(ADD(res,IMM 4,heapptr))
       else
	   add_instr(ADD(res,IMM (8*len),heapptr));
       (res, state)
    end



  local
    fun shuffle (eqreg : ('a * 'a) -> bool,
		 alloc : 'a -> 'a,
		 mover: ('a * 'a) -> instr)
	        (src : 'a list,
		 dest : 'a list) =
      let fun isdest r = Listops.member_eq(eqreg,r, dest)

	local
	  (* given two lists of equal length, remove corresponding equal elements *)
	  fun sieve2 (h::t,h'::t') =
	      let val (nt,nt') = sieve2 (t,t')
	      in  if eqreg(h,h')
		      then (nt,nt')
		  else (h::nt,h'::nt')
	      end
	    | sieve2(nil,nil) = (nil,nil)
	    | sieve2 _ = error "sieve2"

	in  (* remove assignments of register to self *)
	  val (src,dest) = sieve2 (src,dest)
	end

	(* compute which registers are both a source and a dest *)
	val needtmp = map (fn r => (r,isdest r)) src

	(* create temp regs for these, and copy sources into them.*)
	val tmps =
	  map (fn (r,b) =>
	       if b then
		 let val r' = alloc(r)
		 in  add_instr(mover(r,r'));
		   (r,SOME r')
		 end
	       else (r,NONE)) needtmp

	fun merge (nil,nil) = ()
	  | merge ((h',tmp)::t',h::t) =
	  (case tmp
	     of NONE =>
	       if eqreg(h',h) then ()
			   else add_instr(mover(h',h))
		     | SOME r =>
			     add_instr(mover(r,h));
			     merge(t',t))
	  | merge _ = error "shuffle_regs/merge"
      in merge (tmps,dest)
      end
    fun iclone (REGI(_,rep)) = alloc_regi(rep)
      | iclone (SREGI sreg) =  error "cannot shuffle special registers"
    fun fclone (_ : regf) = alloc_regf()
    fun clone (I ir) = I(iclone ir)
      | clone (F fr) = F(fclone fr)
  in
    fun mv (I ir1, I ir2) = MV(ir1,ir2)
      | mv (F fr1, F fr2) = FMV(fr1,fr2)
      | mv _ = error "cannot move between int and float regs"

    (* shuffle_regs (src_regs,dst_regs)               *)
    (* Emits instructions to move the value of each src_reg into the corresponding dst_reg. *)
    (* i.e., final value of dst_regs[i] == initial value of src_regs[i]                     *)
    (* At the moment, this is only used in Tortl when translating recursive tailcalls.      *)
    (*       joev, 8/2002                                                                   *)
    val shuffle_regs  = shuffle (eqreg, clone, mv)
  end

  fun print_kinds ({env = (context, _), ...} : state) = NilContext.print_kinds context
  fun print_convars ({convarmap, ...} : state) = Name.VarMap.appi (fn (v, _) => (Ppnil.pp_var v; print "; ")) convarmap
  fun print_globals () = Name.VarSet.app (fn v => (Ppnil.pp_var v; print "; ")) (!globals)

end
