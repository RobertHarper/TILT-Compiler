(*$import NILSTATIC Nil Ppnil NilContext NilError NilSubst Stats Normalize NilUtil TraceOps Measure Trace *)
structure NilStatic :> NILSTATIC where type context = NilContext.context = 
struct	
  
  structure Annotation = Annotation

  open Nil 
  open Prim

  val fnc_trace = Trace.newtrace "Fnc:Subtype"
  val bnd_trace = Trace.newtrace "Top Level Bnd"
  val conv_trace = Trace.newtrace "Top Level ConValid"
  val expv_trace = Trace.newtrace "Top Level ExpValid"
  val fv_trace = Trace.newtrace "Top Level FncValid"
  val expb_trace = Trace.newtrace "Top Level Expb"
  val fbnd_trace = Trace.newtrace "Top Level Fbnd"
  val norm_trace = Trace.newtrace "Top Level CHNF"
  val measure_trace = Trace.newtrace "Top Level Measure"

  val show_top = Stats.ff "nilstatic_show_top"

  val top_bnds = ref false
  val top_fnc = ref false
  val top_expb = ref false
  val top_fbnd = ref false
  val pp_kind = Ppnil.pp_kind
  val pp_con = Ppnil.pp_con
  val pp_exp = Ppnil.pp_exp
  val pp_kind' = Ppnil.pp_kind'
  val pp_con' = Ppnil.pp_con'
  val pp_exp' = Ppnil.pp_exp'
  val pp_var = Ppnil.pp_var
  val pp_label = Ppnil.pp_label
  val pp_label' = Ppnil.pp_label'
  val pp_list = Ppnil.pp_list

  val trace = Stats.ff "nilstatic_trace"
  val stack_trace = Stats.ff "nilstatic_show_stack"
  val local_debug = Stats.ff "nilstatic_debug"
  val assertions  = Stats.ff "nilstatic_assertions"

  val profile = Stats.ff "nil_profile"
  val local_profile = Stats.ff "nilstatic_profile"
  val internal_profile = Stats.ff "nilstatic_internal_profile"
  val exp_profile      = Stats.ff "nilstatic_exp_profile"
  val bnd_profile      = Stats.ff "nilstatic_bnd_profile"
  val equiv_total_profile = Stats.ff "nilstatic_equiv_total_profile"

  val debug = Stats.ff "nil_debug"

  val show_calls = Stats.ff "nil_show_calls"
  val show_context = Stats.ff "nil_show_context"
  val short_circuit = Stats.tt "nilstatic_shortcircuit"
  val warn_depth = ref 500
  val type_equiv_count = Stats.counter "Type Equiv Calls"
  val alpha_equiv_success = Stats.counter "Alpha Equiv Checks Succeeded"
  val kind_standardize_calls = Stats.counter "Kind Standardize Calls"
  val early_stop    = Stats.counter "Equiv:early stops"
  val partial_check = Stats.counter "Equiv:partial equiv checks"
  val stop_early    = Stats.tt      "Equiv:stop_early"

  val locate = NilError.locate "NilStatic"
  val assert = NilError.assert

  val equiv_depth = ref 0

  fun error s s' = Util.error s s'

  fun error' s = error "nilstatic.sml" s

  local
      datatype entry = 
	EXP of exp * NilContext.context 
      | CON of con * NilContext.context 
      | EQCON of con * con * NilContext.context 
      | KIND of kind * NilContext.context
      | SUBKIND of kind * kind * NilContext.context
      | BND of bnd * NilContext.context
      | MODULE of module * NilContext.context
      val stack = ref ([] : entry list)
      val maxdepth = 10000
      val depth = ref 0
      fun push e = (depth := !depth + 1;
		    stack := (e :: (!stack));
		    if (!debug andalso (!depth mod 20 = 0))
			then (print "****nilstatic.sml: stack depth = ";
			      print (Int.toString (!depth));
			      print "\n")
		    else ();
		    if (!depth) > maxdepth
			then (print "depth = ";
			      print (Int.toString (!depth));
			      print "\nmaxdepth =";
			      print (Int.toString (maxdepth));
			      print "\n";
	                      error (locate "push") "stack depth exceeded")
		    else ())
  in
    fun clear_stack() = (depth := 0; stack := [])
    fun push_exp (e,context) = push (EXP(e,context))
    fun push_con(c,context) = push(CON(c,context))
    fun push_eqcon(c1,c2,context) = push(EQCON(c1,c2,context))
    fun push_kind(k,context) = push(KIND(k,context))
    fun push_subkind(k1,k2,context) = push(SUBKIND(k1,k2,context))
    fun push_bnd(b,context) = push(BND(b,context))
    fun push_mod(m,context) = push(MODULE(m,context))
    fun pop() = (depth := !depth - 1;
		 stack := (tl (!stack)))
    fun show_stack() = let val st = !stack
			   val _ = clear_stack()
			   fun show (EXP(e,context)) = 
				     (print "exp_valid called with expression =\n";
				      pp_exp e;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (CON(c,context)) =
				     (print "con_valid called with constructor =\n";
				      pp_con c;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (EQCON(c1,c2,context)) =
				     (print "con_equiv called with constructor =\n";
				      pp_con c1; print "\nand\n";
				      pp_con c2;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (KIND(k,context)) =
				     (print "kind_valid called with kind =\n";
				      pp_kind k;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (SUBKIND(k1,k2,context)) =
				     (print "sub_kind called with kind1 =\n";
				      pp_kind k1;
				      print "\n                 and kind2 =\n";
				      pp_kind k2;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (BND(b,context)) =
				     (print "bnd_valid called with bound =\n";
				      Ppnil.pp_bnd b;
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
			     | show (MODULE(m,context)) =
				     (print "module_valid called with module =\n";
				      Ppnil.pp_module 
                                        {module = m,
                                         header = "",
                                         name = "",
                                         pass = ""};
				      print "\nand context"; NilContext.print_context context;
				      print "\n\n")
		       in  app show (rev st)
		       end
    fun wrap str f arg = 
      let val debug' = !debug;
      in 
	(debug := !local_debug;(f arg) before (debug := debug'))
	handle e => (debug := debug';print "Error while calling "; print str; print "\n"; if !stack_trace then show_stack() else (); raise e)
      end
  end

  (* Local rebindings from imported structures *)

  val timer = Stats.subtimer'
  val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer' args args2 else #2 args args2
  val flagtimer = fn flag => fn args => fn args2 => if !profile orelse !local_profile orelse !flag 
						      then Stats.subtimer' args args2 else #2 args args2
  (*From Normalize*)

  val expandMuType = subtimer("Tchk:expandMuType",Normalize.expandMuType)
  val projectRecordType = subtimer("Tchk:projectRecordType",Normalize.projectRecordType)
  val projectSumType = subtimer("Tchk:projectSumType",Normalize.projectSumType)
  val type_of = subtimer("Tchk:type_of",Normalize.type_of)
  fun con_head_normalize args = #2( Normalize.reduce_hnf args)
  val is_hnf = Normalize.is_hnf

(*  val con_head_normalize = NilHNF.reduce_hnf*)

  val time_con_head_normalize = fn s => subtimer("Tchk:CHNF:"^s,con_head_normalize)
  val con_head_normalize = time_con_head_normalize "All"


  (*From NilContext*)
  type context = NilContext.context
  val empty = NilContext.empty
  val insert_con = subtimer("Tchk:insert_con",NilContext.insert_con)
  val insert_con_list = subtimer("Tchk:insert_con_list",NilContext.insert_con_list)
  val find_con = subtimer("Tchk:find_con",NilContext.find_con)
  val find_std_con = subtimer ("Tchk:find_std_con",NilContext.find_std_con)
  val insert_kind = subtimer ("Tchk:insert_kind",NilContext.insert_kind)
  val insert_kind_equation = subtimer ("Tchk:insert_kind_equation",NilContext.insert_kind_equation)
  val insert_equation = subtimer ("Tchk:insert_equation",NilContext.insert_equation)
  val insert_kind_list = subtimer("Tchk:insert_kind_list",NilContext.insert_kind_list)
  val find_max_kind = subtimer ("Tchk:find_max_kind",NilContext.find_max_kind)           
  val kind_standardize = subtimer ("Tchk:kind_standardize",NilContext.kind_standardize)

  val exp_error_context   = NilContext.exp_error_context
  val con_error_context   = NilContext.con_error_context
  val kind_error_context  = NilContext.kind_error_context
  val exps_error_context  = NilContext.exps_error_context
  val cons_error_context  = NilContext.cons_error_context
  val kinds_error_context = NilContext.kinds_error_context
  val print_context = NilContext.print_context

  structure Subst = NilSubst
  type con_subst = Subst.con_subst
  type exp_subst = Subst.exp_subst
  val substConInExp  = fn s => subtimer("Tchk:substConInExp", Subst.substConInExp  s)
  val substConInCon  = fn s => subtimer("Tchk:substConInCon", Subst.substConInCon  s)
  val substConInKind = fn s => subtimer("Tchk:substConInKind",Subst.substConInKind s)
  val substConInBnd  = fn s => subtimer("Tchk:substConInBnd", Subst.substConInBnd  s)
  val substConInCBnd = fn s => subtimer("Tchk:substConInCBnd",Subst.substConInCBnd s)
  val substExpInExp  = fn s => subtimer("Tchk:substExpInExp", Subst.substExpInExp  s)
  val substExpInCon  = fn s => subtimer("Tchk:substExpInCon", Subst.substExpInCon  s)
  val substExpConInCon  = fn s => subtimer("Tchk:substExpConInCon", Subst.substExpConInCon  s)
  val substExpConInKind = fn s => subtimer("Tchk:substExpConInKind",Subst.substExpConInKind s)
  val varConConSubst  = fn v => fn c => subtimer("Tchk:varConConSubst", Subst.varConConSubst  v c)
  val varConKindSubst = fn v => fn c => subtimer("Tchk:varConKindSubst",Subst.varConKindSubst v c)
  val varExpConSubst  = fn v => fn e => subtimer("Tchk:varConExpSubst", Subst.varExpConSubst  v e)
  val empty_subst = Subst.C.empty
  val con_subst_compose = Subst.C.compose

  (*From NilUtil*)
  val generate_tuple_label = NilUtil.generate_tuple_label
  val function_type = NilUtil.function_type
  val convert_sum_to_special = NilUtil.convert_sum_to_special
  val exp_tuple = NilUtil.exp_tuple
  val con_tuple = NilUtil.con_tuple
  val same_openness = NilUtil.same_openness
  val same_effect = NilUtil.same_effect
  val primequiv = NilUtil.primequiv
  val sub_phase = NilUtil.sub_phase
  val project_from_kind_nondep = NilUtil.project_from_kind_nondep
  val alpha_equiv_con = subtimer ("Tchk:alpha_equiv",NilUtil.alpha_equiv_con)
  val alpha_equiv_con = fn args => ((alpha_equiv_con args) andalso (alpha_equiv_success();true))
    (*
  val alpha_equiv_kind = NilUtil.alpha_equiv_kind
  val alpha_sub_kind = NilUtil.alpha_sub_kind
  val alpha_normalize_con = NilUtil.alpha_normalize_con
  val alpha_normalize_kind = NilUtil.alpha_normalize_kind
*)
  val is_var_e = NilUtil.is_var_e

  val map_annotate = NilUtil.map_annotate
  val singletonize = subtimer("Tchk:singletonize",NilUtil.singletonize)
  val get_arrow_return = NilUtil.get_arrow_return

  val strip_var = NilUtil.strip_var
  val strip_exntag = NilUtil.strip_exntag
  val strip_recursive = NilUtil.strip_recursive
  val strip_boxfloat = NilUtil.strip_boxfloat
  val strip_float = NilUtil.strip_float
  val strip_int = NilUtil.strip_int
  val strip_sum = NilUtil.strip_sum
  val strip_arrow = NilUtil.strip_arrow
  val strip_externarrow = NilUtil.strip_externarrow
  val strip_record = NilUtil.strip_record
  val strip_crecord = NilUtil.strip_crecord
  val strip_proj = NilUtil.strip_proj
  val strip_prim = NilUtil.strip_prim
  val strip_app = NilUtil.strip_app
  val is_exn_con = NilUtil.is_exn_con
  val is_float_c = NilUtil.is_float_c 
  val strip_annotate = NilUtil.strip_annotate

  (*From Name*)
  val eq_var = Name.eq_var
  val eq_var2 = Name.eq_var2
  val eq_label = Name.eq_label
  val var2string = Name.var2string
  val label2string = Name.label2string
  val fresh_named_var  = Name.fresh_named_var
  fun fresh_var () = fresh_named_var "nilstatic"
  val derived_var  = Name.derived_var

  (*From Listops*)
  (*I've timed these - they aren't important*)
  val count1     = Listops.count1
  val eq_len     = Listops.eq_len
  val eq_len3    = Listops.eq_len3
  val map_second = Listops.map_second
  val foldl_acc  = Listops.foldl_acc
  val foldl2     = Listops.foldl2
  val foldl3     = Listops.foldl3
  val map        = Listops.map
  val map2       = Listops.map2
  val map3       = Listops.map3
  val map0count  = Listops.map0count
  val app2       = Listops.app2
  val app3       = Listops.app3
  val zip        = Listops.zip
  val zip3       = Listops.zip3
  val unzip      = Listops.unzip
  val unzip3     = Listops.unzip3
  val all        = Listops.all
  val all2       = Listops.all2
  val all3       = Listops.all3
  val split      = Listops.split
  val opt_cons   = Listops.opt_cons
  val member_eq  = Listops.member_eq

  (* XXX CS: detects conflicts between namespaces  that ought not occur! *)
  val labels_distinct = Listops.no_dups Name.compare_label
  val no_dups = Sequence.no_dups

  (*From PrimUtil*)
  val same_intsize = NilPrimUtil.same_intsize
  val same_floatsize = NilPrimUtil.same_floatsize

  (*From Util *)
  val eq_opt = Util.eq_opt
  val map_opt = Util.mapopt
  val split_opt = Util.split_opt
  val printl = Util.printl
  val lprintl = Util.lprintl
  val lprint = Util.lprint
  val curry2 = Util.curry2
  val curry3 = Util.curry3

  (*From NilError*)
  val c_all = NilError.c_all
  val c_all1 = NilError.c_all1
  val c_all2 = NilError.c_all2
  val c_all3 = NilError.c_all3

  val perr_e = NilError.perr_e
  val perr_c = NilError.perr_c
  val perr_k = NilError.perr_k
  val perr_e_c = NilError.perr_e_c
  val perr_c_c = NilError.perr_c_c
  val perr_c_k = NilError.perr_c_k
  val perr_k_k = NilError.perr_k_k
  val perr_c_k_k = NilError.perr_c_k_k
  val perr_e_c_c = NilError.perr_e_c_c

  val b_perr_k = NilError.b_perr_k

  val o_perr = NilError.o_perr
    
  val o_perr_e = NilError.o_perr_e
  val o_perr_c = NilError.o_perr_c
  val o_perr_k = NilError.o_perr_k
  val o_perr_e_c = NilError.o_perr_e_c
  val o_perr_c_c = NilError.o_perr_c_c
  val o_perr_k_k = NilError.o_perr_k_k
  val o_perr_c_k_k = NilError.o_perr_c_k_k
  val o_perr_e_c_c = NilError.o_perr_e_c_c
  (* Local helpers *)


  fun foldl_all2 ffun init (l1,l2) = 
    let 
      fun loop ([],[],state) = (true,state)
	| loop (a::arest,b::brest,state) =
	let
	  val (flag,state) = ffun (a,b,state)
	in
	  if flag then
	    loop (arest,brest,state)
	  else
	    (false,state)
	end
	| loop _ = error (locate "foldl_all2") "Passed lists of unequal length"
    in
      loop (l1,l2,init)
    end
    


  fun print_all strings = map print strings

  fun e_error (D,exp,explanation) = 
    (
     print_all ["\n","TYPE ERROR: Problem with expression\n",
		explanation,"\n"];
     Ppnil.pp_exp exp;
     lprintl "WITH MINIMAL CONTEXT AS";
     print_context (exp_error_context (D,exp));
     raise (Util.BUG explanation)
       )

  fun c_error (D,con,explanation) = 
      (
       print_all ["\n","TYPE ERROR: Problem with constructor\n",
		 explanation,"\n"];
       Ppnil.pp_con con;
       lprintl "WITH MINIMAL CONTEXT AS";
       print_context (con_error_context (D,con));
       raise (Util.BUG explanation)
       )

  fun k_error (D,kind,explanation) = 
    (
     print_all ["\n","TYPE ERROR: Problem with kind\n",
		explanation,"\n"];
     Ppnil.pp_kind kind;
     lprintl "WITH MINIMAL CONTEXT AS";
     print_context (kind_error_context (D,kind));
     raise (Util.BUG explanation)
       )

  val find_con = find_con


  fun strip_sum (D,con) = 
    (case NilUtil.strip_sum (con_head_normalize(D,con)) of
       SOME quad => quad
     | _ => c_error(D,con,"Expected a sum type"))
       
  fun strip_int (D,con) = 
    (case NilUtil.strip_int (con_head_normalize(D,con))
       of SOME intsize => intsize
	| NONE => c_error(D,con,"Expected the integer type"))
       
  fun strip_exntag (D,con) = 
    (case NilUtil.strip_exntag (con_head_normalize(D,con))
       of SOME arg => arg
	| NONE => c_error(D,con,"Expected the exn type"))

  fun strip_record (D,con) = 
    (case NilUtil.strip_record (con_head_normalize(D,con))
       of SOME arg => arg
	| NONE => c_error(D,con,"Expected the record type"))

  fun strip_float (D,con) = 
    (case NilUtil.strip_float (con_head_normalize(D,con))
       of SOME arg => arg
	| NONE => c_error(D,con,"Expected the float type"))

  fun strip_boxfloat (D,con) = 
    (case NilUtil.strip_boxfloat (con_head_normalize(D,con))
       of SOME arg => arg
	| NONE => c_error(D,con,"Expected the boxfloat type"))

  fun is_record (D,con) = Option.isSome (NilUtil.strip_record (con_head_normalize(D,con)))

  (*PRE: kind is standard
   *)
  fun is_type kind = 
    (case kind
       of Type_k => true
        | SingleType_k _ => true
	| _ => false)

  fun is_type' D kind = 
    (case kind_standardize(D,kind) 
       of Type_k => true
	| SingleType_k _ => true
	| _ => false)

  fun sub_effect (sk,Total,Total) = true
    | sub_effect (sk,Partial,Partial) = true
    | sub_effect (sk,Total,Partial) = sk
    | sub_effect (sk,_,_) = false

  fun assertWellFormed context = 
    let
      val debug' = !debug
      val _ = debug := false
      val res = 
	((NilContext.is_well_formed (kind_valid,con_valid,sub_kind) context, fn () => ())
	 handle any => (debug := debug';raise any))
      val _ = debug := debug'
    in
      res
    end
  and kind_valid (D,kind) = 
    let 
      val _ = push_kind(kind,D)
      val _ = if (!show_calls)
		then (print "kind_valid called with kind =\n";
		      pp_kind kind;
		      if !show_context then (print "\nand context"; NilContext.print_context D) else ();
		      print "\n\n")
	      else ()

      val _ = 
	if !debug then
	  assert (locate "kind_valid - PRE")
	  [
	   assertWellFormed D
	   (* assertRenamedKind (D,kind) *)
	   ]
	else ()

      val _ = kind_valid'(D,kind)
      val _ = if (!show_calls)
		then (printl "kind_valid returned")
	      else ()

      val _ = pop()
    in  ()
    end

  and kind_valid' (D : context, kind : kind) : unit = 
    (case kind of
       Type_k => ()
     | SingleType_k con => con_analyze(D,con,Type_k)
     | Single_k con => ignore(con_valid(D,con))
     | Record_k elts => 
	 let
	   fun folder (((l,v),k),D) = 
	     (kind_valid (D,k);
	      insert_kind (D,v,k))
	   val _ = Sequence.foldl folder D elts
	   fun compare (((l1,_),_),((l2,_),_)) = Name.compare_label (l1,l2)
	   val _ = 
	     (no_dups compare elts) orelse
	     (k_error (D,kind,"Labels in record kind not distinct"))
	 in ()
	 end
     | Arrow_k (openness, formals, return) => 
	 let 
	   fun folder ((v,k),D) = 
	     (kind_valid (D,k);
	      insert_kind (D,v,k))
	   val D = foldl folder D formals
	 in  
	   kind_valid (D,return)
	 end)

  and con_analyze(D,con,kind) =
    let
      val kind' = con_valid(D,con)
    in
      if sub_kind(D,kind',kind) then ()
      else c_error(D,con,"Con cannot be given kind")
    end
  and con_valid (D : context, constructor : con) : kind = 
    let 
      val _ = push_con(constructor,D)
      val _ = if (!show_calls)
		then (print "con_valid called with constructor =\n";
		      pp_con constructor; 
		      if !show_context then (print "\nand context"; NilContext.print_context D) else ();
		      print "\n\n")
	      else ()

      val _ = 
	if !debug then
	  assert (locate "con_valid PRE")
	  [
	   assertWellFormed D
	   (* assertRenamedCon (D,constructor) *)
	   ]
	else ()
      val k = subtimer("Tchk:con_valid",con_valid') (D,constructor)
      val _ = if (!show_calls)
		then (printl "con_valid returned")
	      else ()
      val _ = 
	if !debug then
	  assert (locate "con_valid POST")
	  [
	   (* assertRenamedKind (D,k) *)
	   ]
	else ()
      val _ = pop()
    in  k
    end


  and pcon_valid (D,pcon,args) = 
    let      
      val kinds = map (curry2 con_valid D) args
      val _ = 
	(case pcon of
	   (Record_c (labels,NONE)) => 
	     let
	       val _ = 
		 (if labels_distinct labels then
		    (c_all is_type b_perr_k kinds) orelse
		    (error (locate "pcon_valid") "Record contains field of non-word kind")
		  else
		    (error (locate "pcon_valid") "Record contains duplicate field labels"))
	     in () 
	     end
	 | (Sum_c {known,totalcount,tagcount}) => 
	     let
	       val kinds = 
		 (case kinds
		    of [rkind] =>
		      (case rkind
			 of Record_k entries => Sequence.maptolist (fn (_,k) => k) entries
			  | other => [other])
		     | _ => error (locate "pcon_valid") "Wrong number of args to Sum_c")

	       val _ = 
		 (
		  ((length kinds) = (Word32.toInt (totalcount - tagcount))) orelse
		  (error (locate "pcon_valid") "Sum_c counts disagree with args");
		  
		  (List.all is_type kinds) orelse
		  (error (locate "pcon_valid") "Sum contains non-word component" );

		  (case known 
		     of SOME i => 
		       (Word32.<=(Word32.fromInt 0,i) andalso 
			Word32.<(i,totalcount)) orelse
		       (perr_c (Prim_c (pcon,args));
			error (locate "pcon_valid") "Illegal index to sum constructor")
		      | NONE => true)
		     )

	     in ()
	     end
	 | (Vararg_c _) => 
	     let
	       val _ = 
		 (c_all is_type b_perr_k kinds) orelse
		 (error (locate "pcon_valid") "Vararg has non-word component" )
	     in ()
	     end
	 | _ => ())
    in ()
    end
  
  and con_valid' (D : context, constructor : con) : kind = 
    let

      val kind = 
	(case constructor of
	   (Prim_c (Record_c (labels,SOME vars), args)) =>
	     let
	       fun folder (v,c,(kinds,D)) = ((con_valid (D,c))::kinds,insert_con (D,v,c))
	       val (kinds,D) = foldl2 folder ([],D) (vars,args)
	       val _ = 
		 (if labels_distinct labels then
		    (c_all is_type b_perr_k kinds) orelse
		    (c_error(D,constructor,"DepRecord contains field of non-word kind"))
		  else
		    (c_error(D,constructor,"DepRecord contains duplicate field labels")))
	     in SingleType_k(constructor)
	     end
	 | (Prim_c (pcon,args)) => ((pcon_valid (D,pcon,args);SingleType_k(constructor))
				    handle e => c_error(D,constructor,"Prim con invalid"))
	 | (Mu_c (is_recur,defs)) =>
	     let
	       val D =
		 if is_recur then
		   let fun folder((v,_),D) = insert_kind (D,v,Type_k)
		   in  Sequence.foldl folder D defs
		   end
		 else D
		   
	       val kinds = map (fn (_,c) => con_valid(D,c)) (Sequence.toList defs)
		 
	       val _ = 
		 (c_all is_type b_perr_k kinds) orelse
		 (c_error(D,constructor,"Invalid kind for recursive constructor" ))
		 
	       val len = Sequence.length defs
	     in  
	       if len = 1
		 then SingleType_k(constructor)
	       else 
		 let 
		   fun mapper (i,(v,c)) = 
		     let val label = generate_tuple_label(i+1)
		     in((label,derived_var v),SingleType_k(Proj_c(constructor,label)))
		     end
		   val entries = Sequence.mapcount mapper defs
		 in  Record_k(entries)
		 end
	     end
	    | (AllArrow_c {openness,effect,isDependent,tFormals,
			   eFormals,fFormals,body_type}) =>
	     let
	       fun folder ((v,k),D) = 
		 (kind_valid (D,k);
		  insert_kind (D,v,k))
	       val D = foldl folder D tFormals
	       fun folder ((vopt,c),D) = 
		   (con_valid (D,c),
		    case vopt of 
			NONE => D
		      | SOME v => insert_con (D,v,c))
	       val (eFormal_kinds,D) = foldl_acc folder D eFormals
	       val body_kind = con_valid (D,body_type)
	       val _ = 
		 ((c_all is_type b_perr_k eFormal_kinds) andalso (is_type body_kind)) orelse
		 (c_error(D,constructor,"Invalid arrow constructor"))
	     in
	       SingleType_k(constructor)
	     end
	    | ExternArrow_c (args,body) => 
	     let
	       val kinds = map (curry2 con_valid D) args
	       val kind = con_valid (D,body)
	       val _ = 
		 ((c_all is_type b_perr_k kinds) andalso (is_type kind)) orelse
		 (c_error(D,constructor,"Invalid extern arrow constructor"))
	     in
	       SingleType_k(constructor)
	     end
	    | (v as (Var_c var)) => 
	      let
		val kind = (find_max_kind (D,var)
			    handle Unbound =>
			      (print_all ["UNBOUND VARIABLE = ",var2string var,
					  " CONTEXT IS \n"];
			       NilContext.print_context D;
			       error  (locate "con_valid") ("variable "^(var2string var)^" not in context")))
	      in
		kind
	      end
	    | (Let_c (Parallel,cbnds,con)) => error' "Parallel bindings not supported yet"
	    | (Let_c (Sequential,cbnds,con)) =>
	      let  
		val D = foldl cbnd_valid D cbnds
		val kind = con_valid(D,con)
	      in kind   (*Since kind is principal, don't need to substitute?*)
	      end
	    | (Typeof_c exp) => (SingleType_k(exp_valid (D,exp)))
	    | (Closure_c (code,env)) => 
	     let
	       val env_kind = con_valid (D,env)
	       val code_kind =  con_valid (D,code)
	       val (vklist,body_kind) = 
		 case code_kind of
		   Arrow_k (Code ,vklist,body_kind) => (vklist,body_kind)
		 | Arrow_k (ExternCode,vklist,body_kind) =>  (vklist,body_kind)
		 | _ => (c_error(D,constructor,"Invalid closure: code component does not have code kind"))
	       val (first,(v,klast)) = split vklist
	       val _ = 
		 (sub_kind (D,env_kind,klast)) orelse
		 (print "Invalid kind for closure environment:";
		  print " env_kind < klast failed\n";
		  print "env_kind is "; pp_kind env_kind; print "\n";
		  print "klast is "; pp_kind klast; print "\n";
		  print "code_kind is "; pp_kind code_kind; print "\n";
		  (c_error(D,constructor,"Invalid kind for closure environment" )))
	       val body_kind = varConKindSubst v env body_kind
	     in
	       Arrow_k(Closure,first,body_kind)
	     end
	    | (Crecord_c entries) => 
	     let
	       val _ = if (!trace)
			 then print "{con_valid processing Crecord_c\n"
		       else ()
	       val (labels,cons) = unzip entries
	       val kinds = map (curry2 con_valid D) cons
	       val _ =  
		 (labels_distinct labels) orelse
		 (Ppnil.pp_list Ppnil.pp_label' labels 
		  ("labels are: ",",",";",true);
                  Listops.no_dups (fn (x,y) => let val _ = (print "comparing: "; pp_label x; print " and "; pp_label y; print " : ")
                                                   val result = Name.compare_label (x,y)
                                                   val _ = if result = EQUAL then print "TRUE\n" else print "FALSE\n"
                                               in result end) labels;
		  (c_error(D,constructor,"Labels in record of constructors not distinct" )))
	       val _ = if (!trace)
			   then print "con_valid done processing Crecord_c\n}"
		       else () 
	       val k_entries = 
		 map2 (fn (l,k) => ((l,fresh_named_var "con_valid_record"),k)) (labels,kinds)
	     in Record_k (Sequence.fromList k_entries)
	     end
	    | (Proj_c (rvals,label)) => 
	     let
	       (*This will never be dependent*)
	       val record_kind = con_valid (D,rvals)
		 
	       val entry_kinds = 
		 (case record_kind of
		    Record_k kinds => kinds
		  | other => 
		      (c_error(D,constructor,"Projection from constructor of non-record kind")))
		val labs = map (#1 o #1) (Sequence.toList entry_kinds)

		val _ = if (member_eq(eq_label,label,labs))
			  then () 
			else (print "attempted to project label ";
			      pp_label label;
			      print " from \n";
			      pp_con rvals;
			      print "\n which has labels";
			      pp_list pp_label' labs ("",", ","",false);
			      c_error(D,constructor,"Ill-formed projection"))
	     in
	       project_from_kind_nondep (record_kind,label)
	     end
	    | (App_c (cfun_orig,actuals)) => 
	     let

	       val cfun_kind = con_valid (D,cfun_orig)

	       val (formals,body_kind) = 
		 case cfun_kind of
		   (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		 | _ => 
		     (
		      print "Invalid kind for constructor application\n";
		      pp_kind cfun_kind; print "\n";
		      (c_error(D,constructor,"Invalid kind for constructor application" ))
		      )

	       val actual_kinds = map (curry2 con_valid D) actuals
	       val (formal_vars,formal_kinds) = unzip formals
	       val _ = 
		 (c_all2 (fn (k1,k2) => sub_kind (D,k1,k2))
		  (o_perr_k_k "Constructor function applied to wrong number of arguments") 
		  (actual_kinds,formal_kinds))
		 orelse
		 (c_error(D,constructor,"Constructor function app failed: argument not subkind of expected kind"))

	       fun folder (v,c,subst) = NilSubst.C.sim_add subst (v,c)
	       val subst = Listops.foldl2 folder (NilSubst.C.empty()) (formal_vars,actuals)
	     in  
	       substConInKind subst body_kind
	     end
	    | (Typecase_c {arg,arms,default,kind=given_kind}) => 
	     let
	       val _ = kind_valid (D,given_kind)
	       val origD = D
	       fun doarm (pcon,args,body) = 
		 let
		   fun folder ((v,k),D) =
		     let
		       val _ = kind_valid (origD,k)
		     in
		       (Var_c v,insert_kind(D,v,k))
		     end
		   val (argcons,D) = foldl_acc folder D args
		   val pkind = pcon_valid (D,pcon,argcons)
		   val body_kind = con_valid(D,body)
		   val _ = 
		     (sub_kind (D,body_kind,given_kind)) orelse
		     (perr_k_k (given_kind,body_kind);
		      (c_error(D,constructor,"Illegal kind in typecase")))
		 in ()
		 end
	       val _ = app doarm arms
	       val arg_kind = con_valid (D,arg)
	       val def_kind = con_valid (D,default)
	       val _ = 
		 ((sub_kind (D,def_kind,given_kind)) andalso (is_type arg_kind)) orelse
		 (c_error(D,constructor,"Error in type case"))
	     in
	       given_kind
	     end
	    | (Annotate_c (annot,con)) => con_valid (D,con)
	     )
    in
      kind
    end
  and cbnd_valid (cbnd,D) =
    let

      fun con_valid_letfun' (D : context, var, formals,body) : unit = 
	let
	  fun folder ((v,k),D) = (kind_valid (D,k);insert_kind(D,v,k))
	  val D = foldl folder D formals
	  val _ = con_valid (D,body)
	in ()
	end
      
      fun bnd_checker maker (var,formals,body) =
	let
	    val _ = if (!trace)
			then (print "{Processing Open_cb/Code_cb with var = ";
			      pp_var var; print "\n")
		    else ()
	  val _ = con_valid_letfun'(D,var,formals,body)
	  val var' = derived_var var
	  val bnd = maker (var',formals,body)
	  val con = Let_c (Sequential,[bnd],Var_c var')
	  val D = insert_equation(D,var,con)
	    val _ = if (!trace)
			then (print "Done processing Open_cb/Code_cb with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  D
	end
      
    in
      (case cbnd
	 of Open_cb args => bnd_checker Open_cb args
	  | Code_cb args => bnd_checker Code_cb args
	  | Con_cb(var,con) =>
	   let
	     val _ = if (!trace)
			 then (print "{Processing Con_cb with var = ";
			       pp_var var; print "\n")
		     else ()
	     val kind = con_valid (D,con)
	     val D = insert_kind_equation(D,var,con,kind)
	     val _ = if (!trace)
			 then (print "Done processing Con_cb with var = ";
			       pp_var var; print "}\n")
		     else ()
	   in D
	   end)
    end
  (*PRE: kind1 and kind2 not necessarily normalized *)
  and sub_kind arg = subeq_kind false arg
  and kind_equiv arg = subeq_kind true arg

  and subeq_phase true (p1,p2) = p1 = p2
    | subeq_phase false (p1,p2) = sub_phase(p1,p2)

  and subeq_kind is_eq (D,kind1,kind2) = 
    let
      val _ = push_subkind(kind1,kind2,D)

      fun sub_one ((var1,kind1),(var2,kind2),(D,subst1,subst2)) = 
	let
	  val var' = derived_var var1
	  val kind1 = substConInKind subst1 kind1
	  val kind2 = substConInKind subst2 kind2
	  val D' = insert_kind (D,var',kind1)
	  val subst1 = Subst.C.sim_add subst1 (var1,Var_c var')
	  val subst2 = Subst.C.sim_add subst2 (var2,Var_c var')
	in
	  (subeq_kind is_eq (D,kind1,kind2),(D',subst1,subst2))
	end

      fun sub_all D (vks1,vks2) = 
	foldl_all2 sub_one (D,empty_subst(),empty_subst()) (vks1,vks2)

      val res = 
      (case (kind1,kind2) 
	 of (Type_k, Type_k) => true
	  | (Type_k,_) => false
	  | (Single_k _,_) => subeq_kind is_eq (D, kind_standardize(D,kind1), kind2)
	  | (_,Single_k _) => subeq_kind is_eq (D, kind1, kind_standardize(D,kind2))
	  | (SingleType_k _ ,Type_k) => true
	  | (SingleType_k (c1),SingleType_k (c2)) => 
	     subtimer("SubKind:st",type_equiv)(D,c1,c2,false)
	  | (SingleType_k (c1), _) => false
(*
	  | (SingleType_k (c1),Single_k (c2)) => 
	   subeq_kind is_eq (D,push_singleton (D,c1),push_singleton (D,c2))
	  | (Single_k _ ,Type_k) => is_type kind1
	  | (Single_k (c1),Single_k (c2)) => 
	   subeq_kind is_eq (D,push_singleton (D,c1),push_singleton (D,c2))
	  | (Single_k (c1),k2) => (* k2 must be a higher kind *)
	   subeq_kind is_eq (D,push_singleton (D,c1),k2)
	  | (k1,Single_k (c2)) => (* k1 must be a higher kind *)
	   subeq_kind is_eq (D,k1,push_singleton (D,c2))
*)
	  | (Arrow_k (openness1, formals1, return1), Arrow_k (openness2, formals2, return2)) => 
	   (if eq_len (formals1,formals2) then
	      let
		val (formals_ok,(D,subst2,subst1)) = sub_all D (formals2,formals1)
		(* Notice reversal of order!!!*)
		val return1 = Subst.substConInKind subst1 return1
		val return2 = Subst.substConInKind subst2 return2
	      in
		formals_ok andalso 
		same_openness (openness1,openness2) andalso 
		  subeq_kind is_eq (D,return1,return2)
	      end
	    else
	      (lprintl "formals of different lengths!!";
	       false))
	  | (Record_k elts1,Record_k elts2) => 
	   let
	     val elts1 = Sequence.toList elts1
	     val elts2 = Sequence.toList elts2
	     val split_lbls = unzip o (map (fn ((l,v),k) => (l,(v,k))))
	     val (labels1,vks1) = split_lbls elts1
	     val (labels2,vks2) = split_lbls elts2
	   in
	     eq_len (elts1,elts2) andalso 
	     (#1 (sub_all D (vks1,vks2))) andalso
	     all2 eq_label (labels1,labels2)
	   end
	  | (Arrow_k _, _) => false
	  | (Record_k _, _) => false)
	 orelse
	    (lprintl "sub_kind failed!";
	     printl "Kind:";
	     pp_kind kind1;
	     lprintl "Not equivalent to :";
	     pp_kind kind2;
	     printl "";
             false)
      val _ = pop()
    in res
    end

  and subtype   (D,c1,c2)    = type_equiv (D,c1,c2,true)
  and type_equiv (D,c1,c2,sk) = flagtimer equiv_total_profile ("Tchk:type_equiv",con_equiv) (D,c1,c2,Type_k,sk)

  (* Given a well-formed context D and well-formed constructors c1 and c2 of kind k,
     return whether they are equivalent at well-formed kind k. *)
  (* note: wrapper function at end of file redefines con_equiv! *)
(*  and con_equiv (args as (D,c1,c2,k,sk)) : bool = 
    (!short_circuit andalso (alpha_equiv_con(c1,c2))) orelse
    ( if !show_calls then 
	(
         print "\nCon equiv called with arg1 = \n";
	 Ppnil.pp_con c1;
	 print "\n arg2 = \n";
	 Ppnil.pp_con c2;
	 print "\nkind =\n";
	 Ppnil.pp_kind k
	 ) else ();
      let val res = con_equiv' (D,c1,(Subst.E.empty(),Subst.C.empty()),c2,(Subst.E.empty(),Subst.C.empty()),k,sk)
      in (
	  if !show_calls then print "\nCon Equiv Done\n" else ();
	  res
	  ) 
      end)

    
  and con_equiv' (D,c1,subst1 as (esubst1,csubst1),c2,subst2 as (esubst2,csubst2),k,sk) : bool = 
    let

      val con_head_normalize = 
	fn args => 
	( (if !show_calls then
	     (print "\nEntering con_head_normalize, arg is:\n";
	      Ppnil.pp_con (#2 args)) else ());
	  let val res = time_con_head_normalize "Equiv" args
	  in 
	    ( if !show_calls then (print "\nReturning with\n";Ppnil.pp_con res;print "\n") else ();
	      res ) 
	  end)

      val _ = push_eqcon(c1,c2,D)
        val res = 
        case k of
	  Type_k => 
	    let
	      fun to_hnf (D,con,subst) = 
		let 
		  fun loop (subst,c) = 
		    let 
		      val (progress,subst,c) = Normalize.con_reduce(D,subst) c  
		    in  case progress of
		      Normalize.PROGRESS => loop (subst,c) 
		    | Normalize.HNF => (subst,c)
		    | Normalize.IRREDUCIBLE =>             (*May be renamings here.  Perhaps should separate?*)
			(case NilContext.find_kind_equation(D,c) of
			   SOME c => loop (subst,c)
			 | NONE => (subst,c))
		    end
		in  loop (subst,con)
		end
	      val (csubst1,c1) = to_hnf(D,c1,csubst1)
	      val (csubst2,c2) = to_hnf(D,c2,csubst2)
	    in (case con_structural_equiv(D,c1,(esubst1,csubst1),c2,(esubst2,csubst2),sk) of
		  NONE => (Ppnil.pp_con c1; print "\n"; print "Not equal to \n";
			   Ppnil.pp_con c2; print "\n";
			   false)
		| SOME Type_k => true
		| SOME (SingleType_k _) => true
		| SOME k => (Ppnil.pp_kind k;
			     error' "con_structural_equiv returned bad kind"))
	      handle e => 
		(print "Failed!\n";
		 Subst.C.print csubst1;print "\n";
		 Subst.E.print esubst1;print "\n";
		 Ppnil.pp_con c1; print "\n"; 
		 Subst.C.print csubst2;print "\n";
		 Subst.E.print esubst2;print "\n";
		 Ppnil.pp_con c2; print "\n";
		 raise e)
	    end

	| SingleType_k c => true
	| Single_k c => true
	| Record_k lvk_seq => 
	   let 
	     fun folder (((l,v),k),(D,equal)) =
	       let 
		 val equal = equal andalso con_equiv'(D,Proj_c(c1,l),subst1,Proj_c(c2,l),subst2,k,sk)
		 val D = insert_kind(D,v,k)  (*This doesn't need to be precise*)
	       in  (D,equal)
	       end
	     val (D,equal) = Sequence.foldl folder (D,true) lvk_seq
	   in  equal
	   end
	| Arrow_k (openness,vklist,k) => 
	   let val D = insert_kind_list(D,vklist)
	       val args = map (Var_c o #1) vklist
	       val c1 = App_c(c1,args)
	       val c2 = App_c(c2,args)
	   in  con_equiv'(D,c1,subst1,c2,subst2,k,sk) 
	   end
	val _ = pop()
	val _ = if res then ()
	    else (print "con_equiv' returning false\n";
		  print "subkinding = "; print (Bool.toString sk); print "\n";
		  print "c1 = "; Ppnil.pp_con c1; print "\n";
		  print "c2 = "; Ppnil.pp_con c2; print "\n";
		  print "k = "; Ppnil.pp_kind k; print "\n";
(*		  print "min context for c1,c2 = "; 
		  NilContext.print_context (Nilcontext.cons_error_context (D,[c1,c2])); print "\n";*)
		  print "\n")
    in  res
    end
  and vklist_equiv (D, vklist1, subst1 as (esubst1,csubst1), 
		       vklist2, subst2 as (esubst2,csubst2)) =
      let 
	fun folder((v1,k1),(v2,k2),(D,csubst1,csubst2,match)) = 
	  let 
	    val k1 = subtimer("Tchk:Equiv:substExpConInKind",substExpConInKind (esubst1,csubst1)) k1
	    val k2 = subtimer("Tchk:Equiv:substExpConInKind",substExpConInKind (esubst2,csubst2)) k2

	    val match = match andalso kind_equiv (D,k1,k2)

	    val vnew = Name.derived_var v1

	    val csubst1 = Subst.C.sim_add csubst1 (v1,Var_c vnew)
	    val csubst2 = Subst.C.sim_add csubst2 (v2,Var_c vnew)
	    val D = insert_kind(D,vnew,k1)
	  in  (D,csubst1,csubst2,match)
	  end
      in  
	if (length vklist1 = length vklist2)
	  then Listops.foldl2 folder (D,csubst1,csubst2,true) (vklist1, vklist2)
	else (D, csubst1,csubst2, false)
      end
    
  and vlistopt_clist_equiv (D, vclist1, subst1 as (esubst1,csubst1), 
			       vclist2, subst2 as (esubst2,csubst2), sk) =
      let 
	fun folder((vopt1, c1), (vopt2, c2), (D, esubst1,esubst2, match)) = 
	  let 

	    val c1 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon (esubst1,csubst1)) c1
	    val c2 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon (esubst2,csubst2)) c2

	    val match = match andalso con_equiv(D,c1,c2,Type_k,sk)
	      
	    val (esubst1,esubst2,D) = 
	      (case (vopt1,vopt2) of
		 (NONE,NONE) => (esubst1,esubst2,D)
	       | (SOME v1,NONE) => let val vnew = Name.derived_var v1 
				   in (Subst.E.sim_add esubst1 (v1,Var_e vnew),esubst2,NilContext.insert_con(D,vnew,c1)) end
	       | (NONE,SOME v2) => let val vnew = Name.derived_var v2
				   in (esubst1,Subst.E.sim_add esubst2 (v2,Var_e vnew),NilContext.insert_con(D,vnew,c2)) end
	       | (SOME v1,SOME v2) => let val vnew = Name.derived_var v1
					  val esubst1 = Subst.E.sim_add esubst1 (v1,Var_e vnew)
					  val esubst2 = Subst.E.sim_add esubst2 (v2,Var_e vnew)
				      in (esubst1,esubst2,NilContext.insert_con(D,vnew,c1)) end)
	  in  (D,esubst1,esubst2,match)
	  end
      in  if (length vclist1 = length vclist2)
	    then 
	      let val (D, esubst1,esubst2, match) = foldl2 folder (D,esubst1,esubst2,true) (vclist1,vclist2)
	      in  (D, esubst1,esubst2, match)
	      end
	  else (D, esubst1,esubst2, false)
      end

  and con_structural_equiv (D,c1,subst1 as (esubst1,csubst1),c2,subst2 as (esubst2,csubst2),sk) : kind option =
   let fun base true = SOME Type_k
	 | base false = NONE
       fun kind_type_tuple 1 = Type_k
	 | kind_type_tuple len = NilUtil.kind_tuple(Listops.map0count (fn _ => Type_k) len)
       val res =   
	(case (c1,c2) of
	   (Prim_c(Record_c (labs1,vlistopt1),clist1), 
	    Prim_c(Record_c (labs2,vlistopt2),clist2)) =>
	   base (Listops.eq_list(eq_label,labs1,labs2) andalso
		 let fun combine(vlistopt,clist) =
		   (case vlistopt of
		      NONE => map (fn c => (NONE, c)) clist
		    | SOME vars => map2 (fn (v,c) => (SOME v, c)) (vars,clist))
		     val vclist1 = combine(vlistopt1, clist1)
		     val vclist2 = combine(vlistopt2, clist2)
		 in  #4(vlistopt_clist_equiv(D,vclist1,subst1,vclist2,subst2,sk))
		 end)
	 | (Prim_c(pcon1 as Sum_c{tagcount=tagcount1,totalcount=totalcount1,
				  known=known1}, clist1), 
	    Prim_c(pcon2 as Sum_c{tagcount=tagcount2,totalcount=totalcount2,
				  known=known2}, clist2)) =>
	   let val carriers = TilWord32.uminus(totalcount1,tagcount1)
	       val k = kind_type_tuple (TilWord32.toInt carriers)
	       val res1 = ((tagcount1 = tagcount2) andalso
			   (totalcount1 = totalcount2) andalso
			   (case (known1,known2,sk) of
			      (NONE,NONE,_) => true
			    | (SOME w1, SOME w2,_) => (w1=w2)
			    | (SOME w1, NONE, true) => true
			    | _ => false))
	   in  base(res1 andalso 
		    Listops.eq_list(fn (c1,c2) => con_equiv'(D,c1,subst1,c2,subst2,k,sk),clist1,clist2))
	   end
	 | (Prim_c(pcon1,clist1), Prim_c(pcon2,clist2)) => 
	   let
	     val sk' = sk andalso (NilUtil.covariant_prim pcon1)
	   in
	     base(NilUtil.primequiv(pcon1,pcon2) andalso 
		  Listops.eq_list(fn (c1,c2) => con_equiv'(D,c1,subst1,c2,subst2,Type_k,sk'),
				  clist1,clist2))
	   end
	 | (Mu_c(ir1,defs1), Mu_c(ir2,defs2)) => 
	   let 
	     val vc1 = Sequence.toList defs1
	     val vc2 = Sequence.toList defs2
	     fun folder ((v1,_),(v2,_),(D,csubst1,csubst2)) = 
	       let
		 val vnew = Name.derived_var v1
		 val D = NilContext.insert_kind(D,vnew,Type_k)
		 val csubst1 = Subst.C.sim_add csubst1 (v1,Var_c vnew)
		 val csubst2 = Subst.C.sim_add csubst2 (v2,Var_c vnew)
	       in  (D,csubst1,csubst2)
	       end
	     val (D,csubst1,csubst2) = foldl2 folder (D,csubst1,csubst2) (vc1,vc2)
	     fun pred ((_,c1),(_,c2)) = 
	       con_equiv'(D,c1,(esubst1,csubst1),c2,(esubst2,csubst2), Type_k, false) (* no subtyping *)
	   in  if ((ir1 = ir2) andalso Listops.eq_list(pred,vc1,vc2))
		 then SOME(kind_type_tuple (length vc1))
	       else NONE
	   end
         | (AllArrow_c {openness=o1,effect=eff1,isDependent=i1,
			tFormals=t1,eFormals=e1,fFormals=f1,body_type=b1},
	    AllArrow_c {openness=o2,effect=eff2,isDependent=i2,
			tFormals=t2,eFormals=e2,fFormals=f2,body_type=b2}) =>
	   base(o1 = o1 andalso (sub_effect(sk,eff1,eff2)) andalso f1 = f2 andalso 
		let val (D,csubst1,csubst2,match) = vklist_equiv(D,t1,subst1,t2,subst2)
		in  match andalso
		  let val (D,esubst1,esubst2,match) =  vlistopt_clist_equiv(D,e2,(esubst2,csubst2),e1,(esubst1,csubst1),sk)
		  in  match andalso
		    con_equiv'(D,b1,(esubst1,csubst1),b2,(esubst2,csubst2),Type_k, sk)
		  end
		end)

	 | (ExternArrow_c (clist1,c1), ExternArrow_c (clist2,c2)) => 
	   let fun mapper c = (NONE, c)
	       val (D,esubst1,esubst2,match) = vlistopt_clist_equiv(D,map mapper clist1,subst1,
								      map mapper clist2,subst2,
								      false)
	   in  base(con_equiv'(D,c1,(esubst1,csubst1),c2,(esubst2,csubst2),Type_k,false))
	   end
	 
	 | (Crecord_c lclist, _) => 
	   error' "Crecord given to con_structural_equiv: not WHNF"
	   
	 | (Let_c _, _) => error' "Let_c given to con_structural_equiv: not WHNF"
	   
	 | (Proj_c (c1,l1), Proj_c(c2,l2)) => 
	   (case (eq_label(l1,l2),con_structural_equiv(D,c1,subst1,c2,subst2,false)) 
	      of (true,SOME(kind as Record_k lvk_seq)) => 
		SOME(project_from_kind_nondep(kind,l1))
	       | _ => NONE)
	 | (App_c (f1,clist1), App_c (f2,clist2)) =>
	   (case con_structural_equiv(D,f1,subst1,f2,subst2,false) of
	      SOME(Arrow_k(openness,vklist,k)) =>
		let     
		  fun folder ((v,k),c1,c2, (subst,equal)) =
		    if equal then 
		      let val equal = con_equiv'(D,c1,subst1,c2,subst2,k,false)
			  val subst = Subst.C.sim_add subst (v,substExpConInCon subst1 c1)
		      in  (subst,equal)
		      end
		    else (subst,equal)
		  val (subst,equal) =    
		    Listops.foldl3 folder (Subst.C.empty(),true) (vklist,clist1,clist2)
		in if equal then SOME(subtimer("Tchk:Equiv:substConInKind",NilSubst.substConInKind subst) k)
		   else NONE
		end
	    | _ => NONE)
	 | (Var_c v, Var_c v') =>  (*If we get here, the substitution has been carried out!*)
	      if (eq_var(v,v')) 
		then SOME(subtimer("Tchk:Equiv:find_std_kind",NilContext.find_std_kind)(D,v))
	      else NONE
	 | (Annotate_c (_,c1), c2) => con_structural_equiv(D,c1,subst1,c2,subst2,sk)
	 | (c1,Annotate_c (_,c2))  => con_structural_equiv(D,c1,subst1,c2,subst2,sk)
	 | (Typecase_c _, _) => error' "Typecase_c not handled"
	 | _ => NONE)
   in res
   end 
*)
  and con_equiv (args as (D,c1,c2,k,sk)) : bool = 
    (!short_circuit andalso (alpha_equiv_con(c1,c2))) orelse
    ( if !show_calls then 
	(
         print "\nCon equiv called with arg1 = \n";
	 Ppnil.pp_con c1;
	 print "\n arg2 = \n";
	 Ppnil.pp_con c2;
	 print "\nkind =\n";
	 Ppnil.pp_kind k
	 ) else ();
      let val res = subtimer("Tchk:con_equiv'",con_equiv') (D,c1,c2,k,sk)
      in (
	  if !show_calls then print "\nCon Equiv Done\n" else ();
	  res
	  ) 
      end)

    
  and con_equiv' (D,c1,c2,k,sk) : bool = 
    let

      val con_head_normalize = 
	fn args => 
	( (if !show_calls then
	     (print "\nEntering con_head_normalize, arg is:\n";
	      Ppnil.pp_con (#2 args)) else ());
	  let val res = time_con_head_normalize "Equiv" args
	  in 
	    ( if !show_calls then (print "\nReturning with\n";Ppnil.pp_con res;print "\n") else ();
	      res ) 
	  end)

      val _ = push_eqcon(c1,c2,D)
        val res = 
        case k of
	  Type_k => 
	    let
	      val c1 = con_head_normalize(D,c1)
              val c2 = con_head_normalize(D,c2)
	    in (case con_structural_equiv(D,c1,c2,sk) of
		  NONE => (Ppnil.pp_con c1; print "\n"; print "Not equal to \n";
			   Ppnil.pp_con c2; print "\n";
			   false)
		| SOME Type_k => true
		| SOME (SingleType_k _) => true
		| SOME k => (Ppnil.pp_kind k;
			     error' "con_structural_equiv returned bad kind"))
	      handle e => 
		(print "Failed!\n";
		 Ppnil.pp_con c1; print "\n"; 
		 Ppnil.pp_con c2; print "\n";
		 raise e)
	    end

	| SingleType_k c => true
	| Single_k c => true
	| Record_k lvk_seq => 
	   let 
	     val c1 = con_head_normalize(D,c1)
	     val c2 = con_head_normalize(D,c2)
	     fun folder (((l,v),k),(D,equal)) =
	       let 
		 val equal = equal andalso con_equiv'(D,Proj_c(c1,l),Proj_c(c2,l),k,sk)
		 val D = insert_kind_equation(D,v,Proj_c(c1,l),k)  
	       in  (D,equal)
	       end
	   in
	     (case (c1,c2) of
		(Mu_c _,Mu_c _) => 
		  (case con_structural_equiv(D,c1,c2,sk) of
		     NONE => false
		   | SOME Type_k => true
		   | SOME (SingleType_k _) => true
		   | SOME k => (Ppnil.pp_kind k;
				error' "con_structural_equiv returned bad kind"))
	      | _ => #2 (Sequence.foldl folder (D,true) lvk_seq))
	   end
	| Arrow_k (openness,vklist,k) => 
	   let val D = insert_kind_list(D,vklist)
	       val args = map (Var_c o #1) vklist
	       val c1 = App_c(c1,args)
	       val c2 = App_c(c2,args)
	   in  con_equiv'(D,c1,c2,k,sk) 
	   end
	val _ = pop()
	val _ = if res then ()
	    else (print "con_equiv' returning false\n";
		  print "subkinding = "; print (Bool.toString sk); print "\n";
		  print "c1 = "; Ppnil.pp_con c1; print "\n";
		  print "c2 = "; Ppnil.pp_con c2; print "\n";
		  print "k = "; Ppnil.pp_kind k; print "\n";
(*		  print "min context for c1,c2 = "; 
		  NilContext.print_context (Nilcontext.cons_error_context (D,[c1,c2])); print "\n";*)
		  print "\n")
    in  res
    end
  (* The substitution returned maps variables in the second list to the first. *)
  and vklist_equiv (D, vklist1, vklist2) : context * con_subst * bool =
      let 
	fun folder((v1,k1),(v2,k2),(D,rename,match)) = 
	  let 
	    val k1 = subtimer("Tchk:Equiv:substConInKind",substConInKind rename) k1
	    val k2 = subtimer("Tchk:Equiv:substConInKind",substConInKind rename) k2
	                              (*Negligible time*)
	    val match = match andalso kind_equiv (D,k1,k2)

	    val vnew = Name.derived_var v1

	    val rename = Subst.C.sim_add rename (v1,Var_c vnew)
	    val rename = if (eq_var(v1,v2)) then rename
			 else Subst.C.sim_add rename (v2,Var_c vnew)
	    val D = insert_kind(D,vnew,k1)
	  in  (D,rename,match)
	  end
      in  
	if (length vklist1 = length vklist2)
	  then Listops.foldl2 folder (D,empty_subst(),true) (vklist1, vklist2)
	else (D, empty_subst(), false)
      end
    
  and vlistopt_clist_equiv (D, cRename, vclist1, vclist2,sk) : context * (exp_subst * con_subst) * bool =
      let fun folder((vopt1, c1), (vopt2, c2), (D, eRename, match)) = 
	      let val c1 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon (eRename,cRename)) c1
		  val c2 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon (eRename,cRename)) c2
		  val match = match andalso con_equiv(D,c1,c2,Type_k,sk)

		  val (eRename,D) = 
		    (case (vopt1,vopt2) of
		       (NONE,NONE) => (eRename,D)
		     | (SOME v1,NONE) => let val vnew = Name.derived_var v1 
					 in (Subst.E.sim_add eRename (v1,Var_e vnew),NilContext.insert_con(D,vnew,c1)) end
		     | (NONE,SOME v2) => let val vnew = Name.derived_var v2
					 in (Subst.E.sim_add eRename (v2,Var_e vnew),NilContext.insert_con(D,vnew,c2)) end
		     | (SOME v1,SOME v2) => let val vnew = Name.derived_var v1
						val eRename = Subst.E.sim_add eRename (v1,Var_e vnew)
						val eRename = Subst.E.sim_add eRename (v2,Var_e vnew)
					    in (eRename,NilContext.insert_con(D,vnew,c1)) end)
	      in  (D,eRename,match)
	      end
      in  if (length vclist1 = length vclist2)
	      then 
		  let val (D, eRename, match) = foldl2 folder (D,Subst.E.empty(),true) (vclist1,vclist2)
		  in  (D, (eRename, cRename), match)
		  end
	  else (D, (Subst.E.empty(), cRename), false)
      end

  and con_structural_equiv (D,c1,c2,sk) : kind option =
   let fun base true = SOME Type_k
	 | base false = NONE
       fun kind_type_tuple 1 = Type_k
	 | kind_type_tuple len = NilUtil.kind_tuple(Listops.map0count (fn _ => Type_k) len)
       val res =   
	(case (c1,c2) of
	     (Prim_c(Record_c (labs1,vlistopt1),clist1), 
	      (Prim_c(Record_c (labs2,vlistopt2),clist2))) =>
	     base (Listops.eq_list(eq_label,labs1,labs2) andalso
		   let fun combine(vlistopt,clist) =
		           (case vlistopt of
				NONE => map (fn c => (NONE, c)) clist1
			      | SOME vars => map2 (fn (v,c) => (SOME v, c)) (vars,clist))
		       val vclist1 = combine(vlistopt1, clist1)
		       val vclist2 = combine(vlistopt2, clist2)
		   in  #3(vlistopt_clist_equiv(D,empty_subst(),vclist1,vclist2,sk))
		   end)
	    | (Prim_c(pcon1 as Sum_c{tagcount=tagcount1,totalcount=totalcount1,
				     known=known1}, clist1), 
	       Prim_c(pcon2 as Sum_c{tagcount=tagcount2,totalcount=totalcount2,
				     known=known2}, clist2)) =>
	     let val carriers = TilWord32.uminus(totalcount1,tagcount1)
		 val k = kind_type_tuple (TilWord32.toInt carriers)
		 val res1 = (tagcount1 = tagcount2) andalso
		           (totalcount1 = totalcount2) andalso
			   (case (known1,known2,sk) of
				(NONE,NONE,_) => true
			      | (SOME w1, SOME w2,_) => (w1=w2)
			      | (SOME w1, NONE, true) => true
			      | _ => false)
	     in  base(res1 andalso 
		      Listops.eq_list(fn (c1,c2) => con_equiv(D,c1,c2,k,sk),
				      clist1,clist2))
	     end
	    | (Prim_c(pcon1,clist1), Prim_c(pcon2,clist2)) => 
	     let
		 val sk' = sk andalso (NilUtil.covariant_prim pcon1)
	     in
		 base(NilUtil.primequiv(pcon1,pcon2) andalso 
		      Listops.eq_list(fn (c1,c2) => con_equiv(D,c1,c2,Type_k,sk'),
				      clist1,clist2))
	     end
	    | (Mu_c(ir1,defs1), Mu_c(ir2,defs2)) => 
	     let 
	       fun do_mu () = 
		 let 
		   
		   val vc1 = Sequence.toList defs1
		   val vc2 = Sequence.toList defs2
		   fun build (D,rename) ((v1,_)::rest1,(v2,_)::rest2) = 
		     let
		       val vnew = Name.derived_var v1
		       val D = NilContext.insert_kind(D,vnew,Type_k)
		       val rename = Subst.C.sim_add rename (v1,Var_c vnew)
		       val rename = Subst.C.sim_add rename (v2,Var_c vnew)
		     in  build (D,rename) (rest1,rest2)
		     end
		     | build acc _ = acc
		   val (D,rename) = build (D,Subst.C.empty()) (vc1,vc2)
		   fun pred ((_,c1),(_,c2)) = 
		   let val c1 = subtimer("Tchk:Equiv:substConInCon",Subst.substConInCon rename) c1
		     val c2 = subtimer("Tchk:Equiv:substConInCon",Subst.substConInCon rename) c2
		   in con_equiv(D,c1,c2, Type_k, false) (* no subtyping *) end
		 in  if ((ir1 = ir2) andalso Listops.eq_list(pred,vc1,vc2))
		       then SOME(kind_type_tuple (length vc1))
		     else NONE
		 end
	     in
	       subtimer ("Equiv:Mu",do_mu) ()
	     end
	  | (AllArrow_c {openness=o1,effect=eff1,isDependent=i1,
			 tFormals=t1,eFormals=e1,fFormals=f1,body_type=b1},
	     AllArrow_c {openness=o2,effect=eff2,isDependent=i2,
			 tFormals=t2,eFormals=e2,fFormals=f2,body_type=b2}) =>
		base(o1 = o1 andalso (sub_effect(sk,eff1,eff2)) andalso f1 = f2 andalso 
		     let val (D,cRename,match) = vklist_equiv(D,t1,t2)
		     in  match andalso
			 let val (D,rename,match) = 
			   vlistopt_clist_equiv(D,cRename,e2,e1,sk)
			     val b1 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon rename) b1
			     val b2 = subtimer("Tchk:Equiv:substExpConInCon",Subst.substExpConInCon rename) b2
			 in  match andalso
			   type_equiv(D,b1,b2, sk)
			 end
		     end)

	      | (ExternArrow_c (clist1,c1), ExternArrow_c (clist2,c2)) => 
		let fun mapper c = (NONE, c)
		    val (D,_,match) = vlistopt_clist_equiv(D,Subst.C.empty(),
							   map mapper clist1,
							   map mapper clist2,
							   false)
		in  base(type_equiv(D,c1,c2,false))
		end

	      | (Crecord_c lclist, _) => 
		error' "Crecord given to con_structural_equiv: not WHNF"

	      | (Let_c _, _) => error' "Let_c given to con_structural_equiv: not WHNF"

	      | (Proj_c (c1,l1), Proj_c(c2,l2)) => 
		(case (eq_label(l1,l2),con_structural_equiv(D,c1,c2,false)) 
		   of (true,SOME(kind as Record_k lvk_seq)) => 
		     SOME(project_from_kind_nondep(kind,l1))
		    | _ => NONE)
	      | (App_c (f1,clist1), App_c (f2,clist2)) =>
		 (case con_structural_equiv(D,f1,f2,false) of
		    SOME(Arrow_k(openness,vklist,k)) =>
		      let     
			fun folder ((v,k),c1,c2, (D,subst,equal)) =
			  if equal then 
			      let val equal = con_equiv(D,c1,c2,k,false)
				  val D = insert_kind_equation(D,v,c1,k)
				  val subst = Subst.C.sim_add subst (v,c1)
			      in  (D,subst,equal)
			      end
			  else (D,subst,equal)
			val (D,subst,equal) = Listops.foldl3 folder (D,Subst.C.empty(),true) 
			  (vklist,clist1,clist2)
		      in if equal then SOME(subtimer("Tchk:Equiv:substConInKind",NilSubst.substConInKind subst) k) 
			 else NONE
		      end
		  | _ => NONE)
	      | (Var_c v, Var_c v') => if (eq_var(v,v')) 
					   then SOME(subtimer("Tchk:Equiv:find_max_kind",NilContext.find_max_kind)(D,v))
				       else NONE
	      | (Annotate_c _, _) => error' "Annotate_c not WHNF"
	      | (Typecase_c _, _) => error' "Typecase_c not handled"
	      | _ => NONE)

(*       val _ = (case res of
		  NONE => (print "con_structural_equiv returning NONE\n";
			   print "c1 = "; Ppnil.pp_con c1; print "\n";
			   print "c2 = "; Ppnil.pp_con c2; print "\n";
			   printl "WITH MINIMAL CONTEXT AS";
			   print_context (cons_error_context (D,[c1,c2]));
			   print "\n")
		| _ => ())*)
   in res
   end 

(* Term level type checking.  *)

  and niltrace_valid (D,nt) =
    let
      val free_vars = TraceOps.get_free_vars nt
      fun checker v = 
	(ignore (NilContext.find_kind (D,v)) (*Don't bother with standard kind if ignoring*)
	 handle Unbound =>
	   (NilContext.print_context D;
	    error  (locate "niltrace_valid") 
	    ("variable "^(var2string v)^" not in context")))
    in
      app checker free_vars
    end

  and bnds_valid (D,bnds) = let val (D,cbnds) = (foldl bnd_valid' (D,[]) bnds) in (D,rev cbnds) end
  and bnd_valid (state,bnd) = bnd_valid' (bnd,state)
  and bnd_valid'' (bnd,state) = 
    let
      val top_level = !top_bnds

      val (D,subst) = state

      val subtimer = fn fargs => if !profile orelse !bnd_profile then Stats.subtimer' fargs else #2 fargs

      (*PRE: The decorations have already been checked, D contains the bindings.
       * Should maybe take two contexts - one for recursive,m one not?
       *)
      fun function_valid openness D (var,Function {effect,recursive,isDependent,
						   tFormals,eFormals,fFormals,
						   body,body_type}) =
	let

	  val exp_valid      = subtimer("Tchk:Bnd:fnc:exp_valid",exp_valid)
	  val con_valid      = subtimer("Tchk:Bnd:fnc:con_valid",con_valid)
	  val kind_valid     = subtimer("Tchk:Bnd:fnc:kind_valid",kind_valid)
	  val subtype        = subtimer("Tchk:Bnd:fnc:subtype",subtype)
	  val top_level = !top_fnc
	  val _ = if top_level then (Trace.enter fv_trace;top_fnc := false) else ()
	  val _ = if (!trace)
			then (print "{Processing function_valid with var = ";
			      pp_var var; print "\n")
		    else ()

	  val D' = insert_kind_list (D,tFormals)
	  val D = foldl (fn ((v,nt,c),D) => (niltrace_valid(D,nt);insert_con(D,v,c))) D' eFormals
	  val D = foldl (fn (v,D) => insert_con (D,v,Prim_c (Float_c F64,[]))) D fFormals

	  val _ = if (!trace)
			then (print "}{Processing body in function_valid with var = ";
			      pp_var var; print "\n")
		    else ()

	  val _ = if top_level then ignore(Trace.enter expv_trace) else ()
	  val body_c = exp_valid (D,body)
	  val _ = if top_level then ignore(Trace.exit expv_trace) else ()
	  val D = if isDependent then D else D'

	  val _ = if (!trace)
			then (print "}{Processing body type against given type in function_valid with var = ";
			      pp_var var; print "\n")
		    else ()
	  val _ = 
	    if top_level then
	      let
		val synth_size = Measure.con_size body_c
		val dec_size   = Measure.con_size body_type
		val body_size   = Measure.exp_size body		  

		val _ = (print "Synthesized type :";print (Int.toString synth_size);print "\n";
			 print "Decorated type   :";print (Int.toString dec_size);print "\n";
			 print "Expression size  :";print (Int.toString body_size);print "\n")
		   
	      in () end
	    else ()

	  val _ = if top_level then (ignore(Trace.enter fnc_trace)) else ()
	  val _ = 
	    (subtype (D,body_c,body_type)) orelse
	    (perr_e_c_c (body,body_type,body_c);
	     (error (locate "function_valid") "Return expression has wrong type" handle e => raise e))   
	  val _ = if top_level then (ignore(Trace.exit fnc_trace)) else () 

(*	  val time = Time.toReal (Trace.exit fnc_trace)
	  val size1 = (Measure.con_size body_c) 
	  val size2 = Measure.con_size body_type
	  val size = size1 + size2
	  val _ = 
	    (print ("Time is                      : "^(Real.toString time)^"\n");
	     print ("Size of synthesized type is  : "^(Int.toString size1)^"\n");
	     print ("Size of decorated type is    : "^(Int.toString size2)^"\n");
	     print "Synthesized type is \n";if size > 20000 then print "too big\n" else Ppnil.pp_con body_c;
	     print "\nDecorated   type is \n";Ppnil.pp_con body_type;
	     if size > !size_max then (print ("New max is "^(Int.toString size)^"\n");size_max := size) else ();
	     print ("Rate is                      : "^(Real.toString((Real.fromInt size)/time))^" nodes/sec\n")
	     )
*)
	  val eFormals = map (fn (v,_,c) => (if isDependent then SOME v else NONE, c)) eFormals
	  val fFormals = Word32.fromInt (List.length fFormals) 

	  val con = AllArrow_c{openness=openness,effect=effect,isDependent=isDependent,
			       tFormals=tFormals,
			       eFormals=eFormals,
			       fFormals=fFormals,body_type=body_type}
	  val _ = if top_level then (Trace.exit fv_trace;top_fnc := true) else ()
	  val _ = if (!trace)
			then (print "Done processing function_valid with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  (var,con)
	end

      fun fbnd_valid (openness,defs) = 
	let
	  val function_type  = fn p => subtimer("Tchk:Bnd:fbnd:function_type",function_type p)
	  val con_valid      = subtimer("Tchk:Bnd:fbnd:con_valid",con_valid)
	  val insert_con     = subtimer("Tchk:Bnd:fbnd:insert_con",insert_con)
	  val function_valid = fn p => fn D => subtimer("Tchk:Bnd:fbnd:function_valid",function_valid p D)
	  val type_equiv     = subtimer("Tchk:Bnd:fbnd:type_equiv",type_equiv)
	  val top_level = !top_fbnd
	  val _ = if top_level then (Trace.enter fbnd_trace;top_fbnd := false) else ()
	  val origD = D
	  val _ = if (!trace)
			then (print "{Processing fbnd_valid \n")
		    else ()
		      
	  val bnd_types = Sequence.map_second (function_type openness) defs
	    
	  (*Checks that the decorations are well-formed*)
	  val _ = Sequence.map_second (curry2 con_valid D) bnd_types
	  val D = Sequence.foldl (fn ((v,c),D) => insert_con(D,v,c)) D bnd_types
	  val _ = if (!trace)
		      then (print "Processing fbnd_valid done with making context}\n")
		    else ()
	  val found_types = Sequence.map (function_valid openness D) defs
	  val _ = if top_level then (Trace.exit fbnd_trace;top_fbnd := false) else ()
	in D
	end

      fun do_closure D ({code,cenv,venv,tipe}) = 
	let
	  val ckind = con_valid (D,cenv)
	  val vcon = exp_valid (D,venv)
	  val (code_type) = 
	    (find_std_con (D,code)
	     handle NilContext.Unbound =>
	       (printl ("Code pointer "^(var2string code));
		print " not defined in context";
		(error (locate "bnd_valid") "Invalid closure" handle e => raise e)))
	  val {openness,effect,isDependent,tFormals,eFormals,fFormals,body_type} =
	    (case strip_arrow code_type of
	       SOME blob => blob
	     | NONE => (perr_e_c (Var_e code,code_type);
			(error (locate "bnd_valid") "Code pointer in closure of illegal type" handle e => raise e)))
	  val _ = (case openness of
		     Code => ()
		   | _ => (perr_e_c (Var_e code,code_type);
			   (error (locate "bnd_valid") "Code pointer in closure of illegal type" handle e => raise e)))

	  val (tformals,(last_tv,last_k)) = split tFormals
	  val tformals = map (fn (tv,k) => (tv,varConKindSubst last_tv cenv k)) tformals
	  val eformals = map (fn (vopt,c) => (vopt, varConConSubst last_tv cenv c)) eFormals
	  val (eformals,last_vc) = split eformals
	    
	  val D = foldl (fn ((v,k),D) => insert_kind(D,v,k)) D tformals
	  fun folder ((vopt,c),D) =
	    (case vopt of
	       NONE => D
	     | SOME v => insert_con(D,v,c))
	  val D' = foldl folder D eformals
	  val body_type = varConConSubst last_tv cenv body_type
	  val body_type = (case last_vc of
			     (NONE,_) => body_type
			   | (SOME v,_) => varExpConSubst v venv body_type)
	    
	  val _ = 
	    (sub_kind (D,Single_k(cenv),last_k) andalso 
	     type_equiv (D',vcon, #2 last_vc, true)) orelse
	    (perr_k_k (last_k,ckind);
	     perr_c_c (#2 last_vc,vcon);
	     (error (locate "bnd_valid") "Mismatch in closure" handle e => raise e))
	    
	  val closure_type = AllArrow_c {openness=Closure, effect=effect, 
					 isDependent=isDependent,
					 tFormals = tformals,
					 eFormals = eformals,
					 fFormals = fFormals,
					 body_type=body_type}
	    
	  val _ = con_valid(D,tipe)
	  val _ = 
	    (type_equiv (D,closure_type,tipe,false)) orelse
	    (perr_c_c (tipe,closure_type);
	     print "code_type is "; pp_con code_type; print "\n";
	     print "con is "; pp_con closure_type; print "\n";
	     (error (locate "bnd_valid") "Type error in closure" handle e => raise e))
	in ()
	end
      
      val con_valid      = subtimer ("Tchk:Bnd:con_valid",con_valid)
      val cbnd_valid     = subtimer ("Tchk:Bnd:cbnd_valid",cbnd_valid)
      val fbnd_valid     = subtimer ("Tchk:Bnd:fbnd_valid",fbnd_valid)
      val function_valid = subtimer ("Tchk:Bnd:function_valid",function_valid)
      val do_closure     = subtimer ("Tchk:Bnd:do_closure",do_closure)

      val _ = if top_level then (Trace.enter bnd_trace;top_bnds := false) else ()

      val res = 
	(case bnd 
	   of Con_b (phase, cbnd) => 
	     let
	       val D = cbnd_valid (cbnd,D)
	       val subst = cbnd::subst
	     in  (D,subst)
	     end
	    | Exp_b (var,nt,exp) =>
	     let

	       val _ = if (!trace)
			 then (print "{Processing Exp_b with var = ";
			       pp_var var; print "\n")
		       else ()
	       val _ = niltrace_valid (D,nt)
	       val bnd_con = exp_valid (D,exp)
	       val D = insert_con (D,var,bnd_con)
	       val _ = if (!trace)
			 then (print "Done processing Exp_b with var = ";
			       pp_var var; print "}\n")
		       else ()
	     in
	       (D,subst)
	     end
	    | (Fixopen_b defs) => (fbnd_valid(Open,defs),subst)
	    | (Fixcode_b defs) => (fbnd_valid(Code,defs),subst)
	    | Fixclosure_b (is_recur,defs) => 
	     let
	       val origD = D
	       val (vars,closures) = unzip (Sequence.toList defs)
	       val tipes = map (fn cl => #tipe cl) closures
	       val _ = map (curry2 con_valid D) tipes
	       val bnd_types = zip vars tipes
	       val D = insert_con_list (D,bnd_types)
	       val _ = if is_recur 
			 then app (do_closure D) closures
		       else app (do_closure origD) closures
	     in
	       (D,subst)
	     end)
      val _ = if top_level then (Trace.exit bnd_trace;top_bnds := true) else ()
    in res
    end

  and exp_analyze(D : context, exp : exp, con : con) : unit = 
    let
      val con' = exp_valid(D,exp)
    in
       ignore (subtimer("exp_analyze:st",type_equiv)(D,con',con,true) orelse 
	       (perr_c_c (con,con');
		e_error(D,exp,"Expression cannot be given type")))
    end
  and exp_valid (D : context, exp : exp) : con = 
      let 
	val _ = push_exp(exp,D)
	  val _ = if (!show_calls)
		      then (print "exp_valid called with expression =\n";
			    pp_exp exp; 
			    if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			    print "\n\n")
		  else ()
	val _ = 
	  if !debug then
	    assert (locate "exp_valid PRE")
	    [
	     assertWellFormed D
	     ]
	  else ()
	val res = subtimer("Tchk:exp_valid",exp_valid') (D,exp)
	val _ = if (!show_calls)
		  then (printl "exp_valid returned")
		else ()
	val _ = 
	  if !debug then
	    assert (locate "exp_valid POST")
	    [
	     (* assertRenamedCon (D,res) *)
	     ]
	  else ()
	  val _ = pop()
      in  res
      end
  and exp_valid' (D : context,exp : exp) : con = 
    let
      val subtimer = fn args => if !profile orelse !exp_profile then Stats.subtimer' args else #2 args

      (*Helper for functions*)
      fun do_formals (item_valid,sub_item,insert_item) (D,formals,actuals) = 
	let
	  fun do_item ((bind,formal_type),actual,D) = 
	    let
	      val actual_type = item_valid (D,actual)
	    in
	      if sub_item (D,actual_type,formal_type) then
		insert_item(D,bind,actual,actual_type)
	      else
		e_error(D,exp,"ExpValid:Formal/Actual parameter mismatch")
	    end
	in
	  (foldl2 do_item D (formals,actuals)) handle e => (print "Formal/Actual length mismatch?\n";raise e)
	end

      fun insert_con_opt (D,SOME v,_,con) = insert_con (D,v,con)
	| insert_con_opt (D,_,_,_) = D

      (*Do an application node. *)
      fun do_app (openness,app,cons,texps,fexps) =
	let           
	  val subtype    = subtimer("Tchk:Exp:App:st",subtype)

	  val con = exp_valid (D,app)
	    
	  val {openness=openness', tFormals, eFormals, fFormals, body_type, ...} = 
	    let
	      val con' = con_head_normalize(D,con)
		
	      (* We rename the bound variables in the function type because they may 
	       * be added to the context below. If this is a recursive function call, 
	       * they may already be in the context
	       *)
	      val con'' = NilRename.renameCon con'
	    in
	      case strip_arrow con'' of
		SOME c => c
	      | NONE => (perr_e_c (app,con); error (locate "exp_valid") "Application of non-arrow expression" )
	    end
	  
	  val _ = 
	    if same_openness (openness,openness') then
	      (case (openness,app) of
		 (Code,Var_e _) => ()
	       | (Code,_) => e_error(D,exp,"code applied to non-variable")
	       | _ => ())
	    else e_error(D,exp,"Error in application - different openness")

	  val D = do_formals (con_valid,sub_kind,insert_kind_equation) (D,tFormals,cons)
	  val D = do_formals (exp_valid,subtype,insert_con_opt)       (D,eFormals,texps)

	  val _ = 
	    let
	      fun do_float (i,[]) = if i <> 0 then e_error(D,exp,"Wrong number of float parameters") else ()
		| do_float (i,actual::rest) = 
		if is_float_c(con_head_normalize(D,exp_valid(D,actual))) then
		  do_float(i-1,rest)
		else
		  e_error(D,exp,"Expected float for float parameter")
	    in do_float (Word32.toInt fFormals,fexps)
	    end
	  
	  val subst = Subst.C.simFromList (zip (#1 (unzip tFormals)) cons)
	  val body_type = Subst.substConInCon subst body_type
	in
	  body_type
	end
      
      (*Type check and subtype a list of expressions against a list of types
       *)
      fun do_args (D,formals,actuals) =
	let
	  val subtype    = subtimer("Tchk:Exp:Arg:st",subtype)
	  fun do_one (formal,actual) = 
	    let val found = exp_valid(D,actual)
	    in
	      if subtype(D,found,formal) then ()
	      else e_error(D,exp,"Formal/actual parameter mismatch in arglist")
	    end
	in (app2 do_one (formals,actuals)) handle e => (print "Length Mismatch?";raise e)
	end

      (* Check a value*)
      fun value_valid (D,value) = 
	(case value of
	   int (intsize,_) => Prim_c (Int_c intsize,[])
	 | uint (intsize,_) => Prim_c (Int_c intsize,[])
	 | float (floatsize,string) => Prim_c (Float_c floatsize,[])
	 | vector (con,vec) =>  
	     let
	       val subtype    = subtimer("Tchk:Exp:Val:st",subtype)
	       val kind = con_valid (D,con)
	       fun check exp = 
		 let
		   val con' = exp_valid (D,exp)
		   val _ = 
		     (subtype (D,con',con)) orelse
		     (e_error(D,Const_e value,"Vector contains expression of incorrect type" handle e => raise e))
		 in()
		 end
	     in
	       (Array.app check vec;
		Prim_c (Vector_c,[con]))
	     end
	 | tag (atag,con) => (ignore (con_valid (D,con));Prim_c (Exntag_c,[con]))
	 | array (con,arr) => e_error(D,Const_e value,"Array's shouldn't happen")
	 | refcell expref => e_error(D,Const_e value,"Ref's shouldn't happen"))

      (*Check a switch
       *)
      fun switch_valid (D,switch) =
	let
	  local
	    val subtype    = subtimer("Tchk:Exp:Swt:default:st",subtype)
	  in
	  fun do_default (D,NONE,[],result_type) = e_error(D,Switch_e switch,"Case must be non-empty")
	    | do_default (D,SOME e,_,result_type) = if (subtype (D,exp_valid(D,e),result_type)) then ()
					else (e_error(D,Switch_e switch,"Type of default doesn't match expected type"))
	    | do_default _ = ()
	  end

(*	  val subtype    = subtimer("Tchk:Switch:subtype",subtype)
	  val con_valid   = subtimer("Tchk:Switch:con_valid",con_valid)
	  val con_head_normalize = subtimer("Tchk:Switch:HNF",con_head_normalize)
	  val exp_valid   = subtimer("Tchk:Switch:exp_valid",exp_valid)
*)
	  fun sum_exn (mk_itype,check_arg) (D,arg,bound,arms,default,result_type) = 
	    let
	      val result_type' = con_head_normalize(D,result_type)
	      val subtype    = subtimer("Tchk:Exp:Swt:Arm:st",subtype)
	      fun do_arm (index,tr,exp) = 
		let
		  val D = insert_con(D,bound,mk_itype (D,index))
		  val con = exp_valid(D,exp)
		  val _ = niltrace_valid(D,tr)
		in
		  subtype(D,con,result_type')
		end
	      
	      val argcon = exp_valid(D,arg)
		
	      val _ = 
		(con_valid(D,result_type);
		 
		 (check_arg(D,argcon)) orelse
		 (e_error(D,Switch_e switch,"Switch argument does not match found type"));
		 
		 if all do_arm arms then ()
		 else e_error(D,Switch_e switch,"Arms to switch are invalid");
		   
		 do_default (D,default,arms,result_type')
		 )
	    in result_type
	    end
	  val sum_exn = fn fargs => subtimer ("Tchk:Exp:sum_exn",sum_exn fargs)
	  val res = 
	    (case switch
	       of Sumsw_e {arg,sumtype,bound,arms,default,result_type} => 
		 let
		   val _ = con_valid(D,sumtype)
		   val sumtype = con_head_normalize(D,sumtype)
		   fun mk_sum (D,field) = convert_sum_to_special (sumtype,field)
		   fun check_arg (D,c) = subtimer("Tchk:Exp:Swt:Arg:st",subtype)(D,c,sumtype)
		 in
		   sum_exn (mk_sum,check_arg) (D,arg,bound,arms,default,result_type)
		 end
	       
		| Exncase_e {arg,bound,arms,default,result_type} =>
		 let
		   fun mk_itype (D,index) = strip_exntag(D,exp_valid(D,index))
		   fun is_exn (D,c) = is_exn_con (con_head_normalize (D,c))
		 in
		   sum_exn (mk_itype,is_exn) (D,arg,bound,arms,default,result_type)
		 end
	       
		| Intsw_e {size,arg,arms,default,result_type} =>
		 let 
		   val argcon = exp_valid (D,arg)
		   val (_,armexps) = unzip arms
		     
		   val result_type' = con_head_normalize(D,result_type)
		   val _ = 
		     (
		      con_valid (D,result_type);
		      
		      if same_intsize (size,strip_int(D,argcon)) then ()
		      else e_error(D,Switch_e switch, "Integer size mismatch in int switch");
			
		      if all (fn exp => subtimer("Tchk:Exp:Intsw:st",subtype) (D,exp_valid(D,exp),result_type')) armexps then ()
		      else e_error(D,Switch_e switch,"Branch arm types don't match");
			  
		      do_default (D,default,arms,result_type')
		      )
		 in
		   result_type
		 end
		| Typecase_e {arg=argcon,arms,default,result_type} => e_error(D,Switch_e switch,"Typecase_e not done"))
	       
	in res 
	end

      (* Check a primitive node
       *)
      fun prim_valid (D,prim,cons,exps) = 
	let 
	  val orig_exp = Prim_e(NilPrimOp prim,cons,exps)
	    
	  fun project_sum_xxx (D,argcon,argexp,k) = 
	    let
	      val subtype    = subtimer("Tchk:Exp:Prim:Proj:st",subtype)
	      val _ = con_valid(D,argcon)
	      val argcon = con_head_normalize(D,argcon)
	      val argcon' = convert_sum_to_special(argcon,k)
	      val argcon'' = exp_valid (D,argexp)
	      val _ = 
		(subtype(D,argcon'',argcon')) orelse   (*Already normal (#2)*)
		(e_error(D,orig_exp,"Type mismatch in project_sum"))
	    in projectSumType(D,argcon,k)  (*Already normal!*)
	    end

	  fun sum_helper(D,sumcon,sumtype) = 
	    let
	      val _ = con_analyze(D,sumcon,Type_k)

	      val sumcon_hnf = con_head_normalize(D,sumcon)

	      val (tagcount,totalcount,carrier) = 
		(case NilUtil.strip_sum sumcon_hnf of
		   SOME (tc,total,NONE,c) => (tc,total,c)
		 | SOME _ => (pp_con sumcon;
			      e_error(D,orig_exp,"inject type argument has special sum type"))
		 | NONE => (pp_con sumcon;
			    e_error(D,orig_exp,"inject given invalid type argument")))
	      val inj_type = convert_sum_to_special(sumcon_hnf, sumtype)
	      val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
	      val which = TilWord32.toInt(TilWord32.uminus(sumtype,tagcount))

	    in (inj_type,nontagcount,which,carrier)
	    end

	  fun inject_sum_nontag (check_con) (D,sumtype,sumcon,exps) = 
	    let
	      val subtype    = subtimer("Tchk:Exp:Prim:Inj:st",subtype)
	      val (inj_type,nontagcount,which,carrier) = sum_helper(D,sumcon,sumtype)
	      val con_k = 
		(case (Int.compare (which,0),Int.compare (nontagcount,1)) of
		   (LESS,_) => e_error (D,orig_exp, "Injecting value into non tag field")
		 | (_,LESS) => e_error(D,orig_exp,"Illegal injection - no non value fields!")
		 | (EQUAL,EQUAL) => carrier
		 | _ => Proj_c(carrier,NilUtil.generate_tuple_label (which + 1)))

	      val (D,cons) = check_con(D,con_k)
	    in
	      if all2 (fn (e,c) => subtype(D,exp_valid(D,e),c)) (exps,cons) then
		inj_type
	      else
		e_error(D,orig_exp,"Injected arguments are don't have expected typs")
	    end

	  fun inject_sum_tag (D,sumtype,sumcon) = 
	    let
	      val (inj_type,_,which,_) = sum_helper(D,sumcon,sumtype)
	    in
	      if which < 0 then inj_type
	      else e_error(D,orig_exp,"Illegal injection - sumtype out of range" )
	    end
	  val is_known = is_hnf o con_head_normalize

	  val res = 
	    (case (prim,cons,exps) of
	       (project k,[argcon],[argexp]) => project_sum_xxx(D,argcon,argexp,k)
	     | (project_known k,[argcon],[argexp]) => 
		 let val con_k = project_sum_xxx(D,argcon,argexp,k)
		 in if is_known(D,con_k) then con_k
		    else e_error(D,orig_exp,"project_known projects into unkown type")
		 end
	     | (project_known_record (k,l),[argcon],[argexp]) => 
		 let val con_k = project_sum_xxx(D,argcon,argexp,k)
		 in projectRecordType (D,con_k,l)
		 end
	     | (record labels,[],exps) =>
		 let val cons = map (curry2 exp_valid D) exps
		 in if (labels_distinct labels) then Prim_c (Record_c (labels,NONE),cons)
		    else e_error(D,orig_exp, "Fields not distinct" )
		 end	
	     | (select label,[],[exp]) => projectRecordType (D,exp_valid (D,exp),label)

	     | (inject sumtype,[sumcon],[]) => inject_sum_tag(D,sumtype,sumcon)
	     | (inject sumtype,[sumcon],exps as [_])  => inject_sum_nontag (fn (D,c) => (D,[c])) (D,sumtype,sumcon,exps)

	     | (inject_known sumtype,[sumcon],[]) => inject_sum_tag (D,sumtype,sumcon)
	     | (inject_known sumtype,[sumcon],exps as [_]) => 
		 let fun check (D,c) = if is_known(D,c) then (D,[c])
				       else e_error(D,orig_exp,"inject_known injects into unkown type")
		 in				     
		   inject_sum_nontag check (D,sumtype,sumcon,exps)
		 end

	     | (inject_known_record sumtype, [sumcon], exps) => 
		 let fun check (D,c) = (case strip_record(D,c) of
					  (_,SOME vars,cons) => 
					    (foldl2 (fn (v,c,D) => insert_con(D,v,c)) D (vars,cons),cons)
					| (_,NONE,cons)      => (D,cons))
		 in
		   inject_sum_nontag check (D,sumtype,sumcon,exps)
		 end
	     | (box_float floatsize,[],[exp]) => 
		 if same_floatsize(strip_float(D,exp_valid (D,exp)),floatsize) then 
		   Prim_c (BoxFloat_c floatsize,[])
		 else e_error(D,orig_exp,"Mismatched float size in box float")

	    | (unbox_float floatsize,[],[exp]) => 
	       if same_floatsize(strip_boxfloat(D,exp_valid (D,exp)),floatsize) then 
		 Prim_c (Float_c floatsize,[])
	       else e_error(D,orig_exp,"Mismatched float size in unbox float")

	    | (roll,[argcon],[exp]) => 
		let val _ = con_valid (D,argcon)
		  val subtype    = subtimer("Tchk:Exp:Prim:Roll:st",subtype)
		in if subtype(D,exp_valid (D,exp),expandMuType(D,argcon) ) then argcon
		   else e_error(D,orig_exp,"Error in roll")
		end

	    | (unroll,[con],[exp]) =>
		let val _ = con_valid (D,con)
		  val subtype    = subtimer("Tchk:Exp:Prim:Unroll:st",subtype)
		in if subtype(D,exp_valid (D,exp),con) then expandMuType (D,con)
		   else e_error(D,orig_exp,"Error in unroll")
		end

	    | (make_exntag,[argcon],[]) => 
		let val _ = con_analyze (D,argcon,Type_k)
		in Prim_c (Exntag_c,[argcon])
		end

	    | (inj_exn name,[],[exp1,exp2]) => 
		if subtimer("Tchk:Exp:Prim:IExn:st",subtype)(D,exp_valid (D,exp2),strip_exntag(D,exp_valid (D,exp1))) then
		  Prim_c (Exn_c,[])
		else e_error(D,orig_exp,"Type mismatch in exception injection")

	    | (make_vararg (openness,effect),cons,exps) =>
		  (error (locate "prim_valid") "make_vararg unimplemented....punting" handle e => raise e)
	    | (make_onearg (openness,effect),cons,exps) =>  
		    (error (locate "prim_valid") "make_onearg unimplemented....punting" handle e => raise e)
	    | (peq,cons,exps) => 
		      (error (locate "prim_valid") "Polymorphic equality should not appear at this level" handle e => raise e)
	    | (prim,cons,exps) => 
			(perr_e (Prim_e (NilPrimOp prim,cons,exps));
			 lprintl "No matching case in prim_valid";
			 (error (locate "prim_valid") "Illegal primitive application" handle e => raise e)))
	in
	  res
	end

      val prim_valid  = subtimer("Tchk:Exp:prim_valid",prim_valid)
      val value_valid = subtimer("Tchk:Exp:value_valid",value_valid)
      val switch_valid = subtimer("Tchk:Exp:switch_valid",switch_valid)
      val do_app      = subtimer("Tchk:Exp:do_app",do_app)
      val do_args     = subtimer("Tchk:Exp:do_args",do_args)
      val subtype    = subtimer("Tchk:Exp:subtype",subtype)
      val con_valid   = subtimer("Tchk:Exp:con_valid",con_valid)
      val con_head_normalize = subtimer("Tchk:Exp:HNF",con_head_normalize)
      val substConInCon      = fn s => subtimer("Tchk:Exp:substConInCon",substConInCon s)

      val res = 
	(case exp of
	   Var_e var => 
	     (find_con (D,var)
	      handle NilContext.Unbound =>
		(error (locate "exp_valid") ("Encountered undefined variable " ^ (Name.var2string var))))
	 | Const_e value => value_valid (D,value)
	 | Let_e (letsort,bnds,exp) => 
	     let
	       val (D,cbnds) = bnds_valid (D,bnds)
	       val con = exp_valid (D,exp)
	     in Let_c (Sequential,cbnds,con)
	     end
	 | Prim_e (NilPrimOp prim,cons,exps) => prim_valid (D,prim,cons,exps)
	 | Prim_e (PrimOp prim,cons,exps) =>   
	     let 
	       val kinds = map (curry2 con_valid D) cons
	       val (total,arg_types,return_type) = NilPrimUtil.get_type prim cons
	       val _ = con_valid(D,return_type)
	       val _ = app (ignore o (curry2 con_valid D)) arg_types
	       val _ = do_args (D,arg_types,exps)
	     in
	       return_type
	     end
	 | Switch_e switch => switch_valid (D,switch)
	 | App_e args => do_app args
	 | ExternApp_e (exp,args) =>
	     let
	       val con = exp_valid(D,exp)

	       val (formals,return) =
		 (case strip_externarrow (con_head_normalize(D,con))
		    of SOME value => value
		     | NONE => e_error(D,exp,"Extern application of non extern arrow value"))

	       val _ = do_args (D,formals,args)
	     in
	       return
	     end
	   
	 | Raise_e (exp,con) => 
	     let
	       val _ = con_valid (D,con)
	       val exn_con = exp_valid (D,exp)
	       val _ = 
		 (is_exn_con (con_head_normalize (D,exn_con))) orelse
		 (e_error(D,exp,"Non exception raised - Ill formed expression") handle e => raise e)
	     in
	       con
	     end
	 | Handle_e {body,bound,handler,result_type} => 
	     let
	       val body_con = exp_valid(D,body)
	       val D = insert_con(D,bound,Prim_c(Exn_c,[]))
	       val handler_con = exp_valid(D,handler)
	       val _ = con_valid (D,result_type)
	     in
	       if subtype(D,body_con,result_type) then
		 if subtype(D,handler_con,result_type) then
		   result_type
		 else
		   e_error(D,exp,"Handler has wrong type")
	       else
		 e_error(D,exp,"Handled expression has wrong type")
	     end
	   )
    in
      res
    end
    and bnd_valid' (bnd,state) = 
	let 
	  val (D,subst) = state
	  val _ = push_bnd(bnd,D)
	  val _ = if (!show_calls)
		    then (print "bnd_valid called with bnd =\n";
			  Ppnil.pp_bnd bnd;
			  if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			  print "\n\n")
		  else ()
	  val res = (bnd_valid''(bnd,state)
		     handle e => (if !stack_trace then show_stack() else (); raise e))
	  val _ = if (!show_calls)
		    then (printl "bnd_valid returned")
		  else ()
	  val _ = pop()
	in  res
      end

      val exp_valid = wrap "exp_valid" exp_valid
      val exp_valid = subtimer ("Tchk:exp_valid",exp_valid)
      val con_valid = wrap "con_valid" con_valid
      val con_valid = subtimer ("Tchk:con_valid",con_valid)
      val kind_valid = wrap "kind_valid" kind_valid
      val kind_valid = subtimer ("Tchk:kind_valid",kind_valid)
      val bnds_valid = subtimer ("Tchk:bnds_valid",bnds_valid)


      fun import_valid' (ImportValue (label,var,tr,con),D) =
	let
	    val _ = if (!trace)
			then (print "{Processing ImportValue with var = ";
			      pp_var var; print "\n")
		    else ()
	  val kind = con_valid(D,con)
	  val D = insert_con(D,var,con)
	    val _ = if (!trace)
			then (print "Done processing ImportValue with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  D
	end
	| import_valid' (ImportType (label,var,kind),D) = 
	let
	    val _ = if (!trace)
			then (print "{Processing ImportType with var = ";
			      pp_var var; print "\n")
		    else ()
	  val _ = kind_valid (D,kind)
	  val D = insert_kind(D,var,kind)
	    val _ = if (!trace)
			then (print "Done processing ImportType with var = ";
			      pp_var var; print "}\n")
		    else ()
	in
	  D
	end

      fun import_valid (D,import) = import_valid' (import,D)

      fun export_valid D (ExportValue (label,var)) = 
	let
	  val con = exp_valid(D,Var_e var)
	in  ()
	end
	| export_valid D (ExportType (label,var)) = 
	let
	  val kind = con_valid(D,Var_c var)
	in ()
	end

      fun module_valid' (D,MODULE {bnds,imports,exports}) = 
	let
	  val _ = NilContext.is_well_formed (kind_valid,con_valid,sub_kind) D
	  val _ = print "  Done checking context\n"
	  val D = foldl import_valid' D imports
          val _ = print "  Done validating imports\n"

	  val _ = top_bnds := !show_top
	  val _ = top_fnc := !show_top
	  val _ = top_expb := !show_top
	  val _ = top_fbnd := !show_top
	  val (D,_) = bnds_valid(D,bnds)
	  val _ = top_bnds := false;
	  val _ = top_fnc := false;
	  val _ = top_expb := false;
	  val _ = top_fbnd := false;

          val _ = print "  Done validating module\n"
	  val _ = app (export_valid D) exports
          val _ = print "  Done validating exports\n"
	in
	  ()
	end

      fun module_valid (D,module) = 
	let 
	  val _ = clear_stack ()
	  val _ = push_mod(module,D)

	  val _ = 
	    if (!assertions) then
	      assert (locate "module_valid")
	      [
	       (NilRename.noShadowsMod module,fn () => print "Typechecker called with shadowed code")
	       ]
	    else ()

	  val _ = if (!show_calls)
		    then (print "module_valid called with module =\n";
                          Ppnil.pp_module 
                                        {module = module,
                                         header = "",
                                         name = "",
                                         pass = ""};
			  if !show_context then (print "\nand context"; NilContext.print_context D) else ();
			  print "\n\n")
		  else ()
	  val res = module_valid'(D,module)

	  val _ = if (!show_calls)
		    then (printl "module_valid returned")
		  else ()

	  val _ = pop()
	in  res
	end
    
      val module_valid = wrap "module_valid" module_valid
      val module_valid = timer ("Module_valid", module_valid)

      val con_equiv = fn (D,c1,c2,k) => subtimer("Tchk:Top:con_equiv",con_equiv) (D,c1,c2,k,false)
      and sub_con = fn (D,c1,c2,k) => con_equiv (D,c1,c2,k,true)

end
