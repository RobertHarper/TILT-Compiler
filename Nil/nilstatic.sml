(*$import NILSTATIC Nil Ppnil NilContext NilError NilSubst Stats Normalize NilUtil TraceOps Measure Trace Alpha Trail *)
structure NilStatic :> NILSTATIC where type context = NilContext.context = 
struct	

  (* IMPORTS *)
  structure Annotation = Annotation

  open Nil 
  open Prim

  val number_flatten = Nil.flattenThreshold

  (* Stats ******************************************************)

  val trace               = Stats.ff "nilstatic_trace"
  val stack_trace         = Stats.ff "nilstatic_show_stack"
  val assertions          = Stats.ff "nilstatic_assertions"

  val profile             = Stats.ff "nil_profile"
  val local_profile       = Stats.ff "nilstatic_profile"

  val import_profile      = Stats.ff "nilstatic_import_profile"
  val exp_profile         = Stats.ff "nilstatic_exp_profile"
  val bnd_profile         = Stats.ff "nilstatic_bnd_profile"

  val equiv_total_profile = Stats.ff "nilstatic_equiv_total_profile"
  val equiv_profile       = Stats.ff "nilstatic_equiv_profile"
  val con_valid_prof      = Stats.ff "nilstatic_con_valid_profile"
  val con_valid_top       = Stats.ff "nilstatic_con_valid_top_profile"

  val debug               = Stats.ff "nil_debug"

  val show_calls          = Stats.ff "nil_show_calls"
  val show_context        = Stats.ff "nil_show_context"

  val compare_paths       = Stats.ff "nilstatic_compare_paths"

  val alpha_equiv_success    = Stats.counter "Alpha Equiv Checks Succeeded"
  val alpha_equiv_fails      = Stats.counter "Alpha Equiv Checks Failed"
  val kind_standardize_calls = Stats.counter "Kind Standardize Calls"



(*  

val timer = Stats.subtimer'
val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer' args args2 else #2 args args2
val flagtimer = fn (flag,name,f) => fn args => ((if !profile orelse !local_profile orelse !flag 
						    then Stats.subtimer' (name,f) args  else f args)
						  handle e => (print "Error in ";print name;print "\n";raise e))
*)


  fun subtimer (_,f) args = f args
  fun flagtimer (flag,name,f) args = (f args ) (*handle e => (print name;print "\n";raise e)*)



  (* Pretty Printing *)

  val pp_kind   = Ppnil.pp_kind
  val pp_con    = Ppnil.pp_con
  val pp_exp    = Ppnil.pp_exp
  val pp_kind'  = Ppnil.pp_kind'
  val pp_con'   = Ppnil.pp_con'
  val pp_exp'   = Ppnil.pp_exp'
  val pp_var    = Ppnil.pp_var
  val pp_label  = Ppnil.pp_label
  val pp_label' = Ppnil.pp_label'
  val pp_list   = Ppnil.pp_list


  (*From Normalize*)

  val expandMuType       = flagtimer (import_profile,"Tchk:expandMuType",Normalize.expandMuType)
  val projectRecordType  = flagtimer (import_profile,"Tchk:projectRecordType",Normalize.projectRecordType)
  val projectSumType     = flagtimer (import_profile,"Tchk:projectSumType",Normalize.projectSumType)
  val removeDependence   = flagtimer (import_profile,"Tchk:removeDependence",Normalize.removeDependence)

  val reduce_hnf'        = flagtimer (import_profile,"Tchk:reduce_hnf'",Normalize.reduce_hnf')
  val is_hnf             = Normalize.is_hnf

  fun con_head_normalize args = #2( Normalize.reduce_hnf args)

  val con_head_normalize = flagtimer (import_profile,"Tchk:CHNF:",con_head_normalize)

  val context_beta_reduce = Normalize.context_beta_reduce
  val context_reduce_hnf  = Normalize.context_reduce_hnf

  (*From NilRename*)
  val alphaCRenameExp   = NilRename.alphaCRenameExp
  val alphaCRenameCon   = NilRename.alphaCRenameCon
  val alphaCRenameKind  = NilRename.alphaCRenameKind
  val alphaECRenameCon  = NilRename.alphaECRenameCon
  val alphaECRenameKind = NilRename.alphaECRenameKind

  (*From NilContext*)
  type context = NilContext.context

  val empty   = NilContext.empty

  val insert_con              = flagtimer (import_profile,"Tchk:insert_con",NilContext.insert_con)
  val insert_con_list         = flagtimer (import_profile,"Tchk:insert_con_list",NilContext.insert_con_list)
  val insert_kind             = flagtimer (import_profile,"Tchk:insert_kind",NilContext.insert_kind)
  val insert_kind_equation    = flagtimer (import_profile,"Tchk:insert_kind_equation",NilContext.insert_kind_equation)
  val insert_equation         = flagtimer (import_profile,"Tchk:insert_equation",NilContext.insert_equation)
  val insert_kind_list        = flagtimer (import_profile,"Tchk:insert_kind_list",NilContext.insert_kind_list)
  val insert_stdkind          = flagtimer (import_profile,"Tchk:insert_stdkind",NilContext.insert_stdkind)
  val insert_stdkind_equation = flagtimer (import_profile,"Tchk:insert_stdkind_equation",NilContext.insert_stdkind_equation)

  val find_con      = flagtimer (import_profile,"Tchk:find_con",NilContext.find_con)
  val find_max_kind = flagtimer (import_profile,"Tchk:find_max_kind",NilContext.find_max_kind)           

  val kind_standardize    = flagtimer (import_profile,"Tchk:kind_standardize",NilContext.kind_standardize)
  val kind_of             = flagtimer (import_profile,"Tchk:kind_of",NilContext.kind_of)

  val exp_error_context   = NilContext.exp_error_context
  val con_error_context   = NilContext.con_error_context
  val kind_error_context  = NilContext.kind_error_context
  val exps_error_context  = NilContext.exps_error_context
  val cons_error_context  = NilContext.cons_error_context
  val kinds_error_context = NilContext.kinds_error_context

  val print_context       = NilContext.print_context

  (* Substitutions *)

  structure Subst = NilSubst

  type con_subst = Subst.con_subst
  type exp_subst = Subst.exp_subst

  val substConInExp  = fn s => flagtimer (import_profile,"Tchk:substConInExp", Subst.substConInExp  s)
  val substConInCon  = fn s => flagtimer (import_profile,"Tchk:substConInCon", Subst.substConInCon  s)
  val substConInKind = fn s => flagtimer (import_profile,"Tchk:substConInKind",Subst.substConInKind s)
  val substConInBnd  = fn s => flagtimer (import_profile,"Tchk:substConInBnd", Subst.substConInBnd  s)
  val substConInCBnd = fn s => flagtimer (import_profile,"Tchk:substConInCBnd",Subst.substConInCBnd s)
  val substExpInExp  = fn s => flagtimer (import_profile,"Tchk:substExpInExp", Subst.substExpInExp  s)
  val substExpInCon  = fn s => flagtimer (import_profile,"Tchk:substExpInCon", Subst.substExpInCon  s)

  val substExpConInCon  = fn s => flagtimer (import_profile,"Tchk:substExpConInCon", Subst.substExpConInCon  s)
  val substExpConInKind = fn s => flagtimer (import_profile,"Tchk:substExpConInKind",Subst.substExpConInKind s)

  val varConConSubst  = fn v => fn c => flagtimer (import_profile,"Tchk:varConConSubst", Subst.varConConSubst  v c)
  val varConKindSubst = fn v => fn c => flagtimer (import_profile,"Tchk:varConKindSubst",Subst.varConKindSubst v c)
  val varExpConSubst  = fn v => fn e => flagtimer (import_profile,"Tchk:varConExpSubst", Subst.varExpConSubst  v e)


  (*From NilUtil*)
  val makeLetC               = NilUtil.makeLetC
  val generate_tuple_label   = NilUtil.generate_tuple_label
  val function_type          = NilUtil.function_type
  val convert_sum_to_special = NilUtil.convert_sum_to_special
  val kind_type_tuple        = NilUtil.kind_type_tuple
  val exp_tuple              = NilUtil.exp_tuple
  val con_tuple              = NilUtil.con_tuple

  val bool_con               = NilUtil.bool_con

  val same_openness          = NilUtil.same_openness
  val same_effect            = NilUtil.same_effect
  val primequiv              = NilUtil.primequiv
  val sub_phase              = NilUtil.sub_phase

  val alpha_subequiv_con = fn st => flagtimer (import_profile,"Tchk:alpha_equiv",NilUtil.alpha_subequiv_con st)
  val alpha_subequiv_con = fn st => fn args => if alpha_subequiv_con st args then (alpha_equiv_success();true)
					       else (alpha_equiv_fails();false)

  val project_from_kind_nondep = NilUtil.project_from_kind_nondep

  val singletonize      = flagtimer (import_profile,"Tchk:singletonize",NilUtil.singletonize)
  val get_arrow_return  = NilUtil.get_arrow_return

  val strip_var         = NilUtil.strip_var
  val strip_exntag      = NilUtil.strip_exntag
  val strip_recursive   = NilUtil.strip_recursive
  val strip_boxfloat    = NilUtil.strip_boxfloat
  val strip_float       = NilUtil.strip_float
  val strip_int         = NilUtil.strip_int
  val strip_sum         = NilUtil.strip_sum
  val strip_arrow       = NilUtil.strip_arrow
  val strip_externarrow = NilUtil.strip_externarrow
  val strip_record      = NilUtil.strip_record
  val strip_crecord     = NilUtil.strip_crecord
  val strip_proj        = NilUtil.strip_proj
  val strip_prim        = NilUtil.strip_prim
  val strip_app         = NilUtil.strip_app
  val is_exn_con        = NilUtil.is_exn_con
  val is_float_c        = NilUtil.is_float_c 
  val is_var_e          = NilUtil.is_var_e

  val map_annotate      = NilUtil.map_annotate
  val strip_annotate    = NilUtil.strip_annotate

  val sub_effect        = NilUtil.sub_effect

  (*From Name*)
  val eq_var           = Name.eq_var
  val eq_var2          = Name.eq_var2 (*Curried*)
  val eq_label         = Name.eq_label
  val var2string       = Name.var2string
  val label2string     = Name.label2string
  val fresh_named_var  = Name.fresh_named_var
  val fresh_var        = Name.fresh_var
  val derived_var      = Name.derived_var

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
  val eq_list    = Listops.eq_list

  (* XXX CS: detects conflicts between namespaces  that ought not occur! *)
  val labels_distinct = Listops.no_dups Name.compare_label

  val no_dups         = Sequence.no_dups

  (*From PrimUtil*)
  val same_intsize   = NilPrimUtil.same_intsize
  val same_floatsize = NilPrimUtil.same_floatsize

  (*From Util *)
  val eq_opt    = Util.eq_opt
  val map_opt   = Util.mapopt
  val split_opt = Util.split_opt
  val printl    = Util.printl
  val lprintl   = Util.lprintl
  val lprint    = Util.lprint
  val printem   = Util.printem
  val curry2    = Util.curry2
  val curry3    = Util.curry3

  fun error s s' = Util.error s s'

  (*From NilError*)
  val c_all  = NilError.c_all
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
    
  val o_perr_e  = NilError.o_perr_e
  val o_perr_c  = NilError.o_perr_c
  val o_perr_k  = NilError.o_perr_k
  val o_perr_e_c  = NilError.o_perr_e_c
  val o_perr_c_c  = NilError.o_perr_c_c
  val o_perr_k_k  = NilError.o_perr_k_k
  val o_perr_c_k_k  = NilError.o_perr_c_k_k
  val o_perr_e_c_c  = NilError.o_perr_e_c_c

  val locate = NilError.locate "NilStatic"
  val assert = NilError.assert

  fun error' s = error "nilstatic.sml" s

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
    
  fun e_error (D,exp,explanation) = 
    (
     printem ["\n","TYPE ERROR: Problem with expression\n",
		explanation,"\n"];
     Ppnil.pp_exp exp;
     lprintl "WITH MINIMAL CONTEXT AS";
     print_context (exp_error_context (D,exp));
     Util.raise_error explanation
       )

  fun c_error (D,con,explanation) = 
      (
       printem ["\n","TYPE ERROR: Problem with constructor\n",
		 explanation,"\n"];
       Ppnil.pp_con con;
       lprintl "WITH MINIMAL CONTEXT AS";
       print_context (con_error_context (D,con));
       Util.raise_error explanation
       )

  fun k_error (D,kind,explanation) = 
    (
     printem ["\n","TYPE ERROR: Problem with kind\n",
		explanation,"\n"];
     Ppnil.pp_kind kind;
     lprintl "WITH MINIMAL CONTEXT AS";
     print_context (kind_error_context (D,kind));
     Util.raise_error explanation
       )

  fun ck_error (D,con,kind,explanation) = 
    (
     printem ["\n","TYPE ERROR: Problem with constructor and kind:\n\t",
		explanation,"\n","Con is:\n"];
     Ppnil.pp_con con;
     print "\nKind is \n";
     Ppnil.pp_kind kind;
     lprintl "\nWITH MINIMAL CONTEXT AS";
     print_context (con_error_context (D,con));
     Util.raise_error explanation
    )


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
			then (printem ["****nilstatic.sml: stack depth = ",
				       Int.toString (!depth),"\n"])
		    else ();
		    if (!depth) > maxdepth
		      then (printem ["depth = ",Int.toString (!depth),"\n",
				     "maxdepth =",Int.toString (maxdepth),"\n"];
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
    fun pop() = ((depth := !depth - 1;
		  stack := (tl (!stack))) handle Empty => error (locate "pop") "Pop from empty stack")
    fun show_stack() = let val st = !stack
			   val _ = clear_stack()
			   fun show (EXP(e,context)) = 
				     (print "exp_valid called with expression =\n";
				      pp_exp e;
				      print "\nand minimal context"; 
				      NilContext.print_context (NilContext.exp_error_context (context,e));
				      print "\n\n")
			     | show (CON(c,context)) =
				     (print "con_valid called with constructor =\n";
				      pp_con c;
				      print "\nand minimal context"; 
				      NilContext.print_context (NilContext.con_error_context (context,c));
				      print "\n\n")
			     | show (EQCON(c1,c2,context)) =
				     (print "con_equiv called with constructor =\n";
				      pp_con c1; print "\nand\n";
				      pp_con c2;
				      print "\nand minimal context"; 
				      NilContext.print_context (NilContext.cons_error_context (context,[c1,c2]));
				      print "\n\n")
			     | show (KIND(k,context)) =
				     (print "kind_valid called with kind =\n";
				      pp_kind k;
				      print "\nand context"; 
				      NilContext.print_context (NilContext.kind_error_context (context,k));
				      print "\n\n")
			     | show (SUBKIND(k1,k2,context)) =
				     (print "sub_kind called with kind1 =\n";
				      pp_kind k1;
				      print "\n                 and kind2 =\n";
				      pp_kind k2;
				      print "\nand context"; 
				      NilContext.print_context (NilContext.kinds_error_context (context,[k1,k2]));
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
    fun wrap str f arg = (f arg)
      handle e => (printem ["Error while calling ",str, "\n"]; if !stack_trace then show_stack() else (); raise e)
  end


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

      val kind = kind_valid'(D,kind)
      val _ = if (!show_calls)
		then (printl "kind_valid returned")
	      else ()

      val _ = pop()
    in  kind
    end

  and kind_valid' (D : context, kind : kind) : kind = 
    (case kind of
       Type_k => kind
     | SingleType_k con => (con_analyze(D,con,Type_k);kind)
     | Single_k con => con_valid(D,con)
     | Record_k elts => 
	 let
	   fun folder (((l,v),k),D) = 
	     let val kstd = kind_valid (D,k)
	     in (((l,v),kstd),insert_stdkind (D,v,kstd))
	     end
	   val (elts,_) = Sequence.foldl_acc folder D elts
	   fun compare (((l1,_),_),((l2,_),_)) = Name.compare_label (l1,l2)
	   val _ = 
	     (no_dups compare elts) orelse
	     (k_error (D,kind,"Labels in record kind not distinct"))
	 in Record_k elts
	 end
     | Arrow_k (openness, formals, return) => 
	 let val (formals,D) = vklist_valid (D,formals)
	 in  Arrow_k(openness,formals,kind_valid (D,return))
	 end)
  and vklist_valid (D,vklist) = 
    let 
      fun folder ((v,k),D) = 
	let val kstd = kind_valid (D,k)
	in ((v,kstd),insert_stdkind (D,v,kstd))
	end
    in foldl_acc folder D vklist
    end

  and pcon_analyze (D,pcon,args) = 
    let
      val type_analyze = curry2 type_analyze
    in
      (case pcon of
	 (Record_c (labels,SOME vars)) =>
	   let
	     fun folder (v,c,D) = (type_analyze D c;insert_con (D,v,c))
	     val D = foldl2 folder D (vars,args)
	   in if labels_distinct labels then ()
	      else (c_error(D,Prim_c(pcon,args),"DepRecord contains duplicate field labels"))
	   end
       | (Record_c (labels,NONE)) => 
	   let val _ = app (type_analyze D) args
	   in if labels_distinct labels then ()
	      else (error (locate "pcon_valid") "Record contains duplicate field labels")
	   end
       | (Sum_c {known,totalcount,tagcount}) => 
	   let
	     val carriers = TilWord32.uminus(totalcount,tagcount)
	     val k = kind_type_tuple (TilWord32.toInt carriers)
	     val _ = 
	       (case args
		  of [con] => con_analyze(D,con,k)
		   | _ => error (locate "pcon_valid") "Wrong number of args to Sum_c")
	     val _ = 
	       (case known 
		  of SOME i => 
		    (Word32.<=(Word32.fromInt 0,i) andalso 
		     Word32.<(i,totalcount)) orelse
		    (perr_c (Prim_c (pcon,args));
		     error (locate "pcon_valid") "Illegal index to sum constructor")
		   | NONE => true)
	   in ()
	   end
       | (Vararg_c _) =>  app (type_analyze D) args
       | _ => ())
    end
  and con_analyze (D : context, constructor : con,kind : kind) : unit = 
    let val type_analyze = curry2 type_analyze
    in
      (case (constructor,kind) of
	 (constructor,SingleType_k c) => 
	 let val _ = type_analyze D constructor
	     val _ = type_equiv(D,constructor,c)
	 in ()
	 end
       | (_,Single_k c) => ck_error (D,constructor,kind,"Non standard kind given to con_analyze")
       | (Prim_c (pcon,args),  Type_k) =>  pcon_analyze(D,pcon,args)
       | (Mu_c (is_recur,defs),Record_k lvk_seq) =>
	 let 
	   val D = if is_recur 
		     then let fun folder((v,_),D) = insert_stdkind (D,v,Type_k)
			  in  Sequence.foldl folder D defs end
		   else D
	 in Sequence.app ((type_analyze D) o #2) defs
	 end
       | (AllArrow_c {openness,effect,isDependent,
		      tFormals,eFormals,fFormals,body_type},Type_k) =>
	 let
	   val (tFormals,D) = vklist_valid (D,tFormals)

	   fun folder ((vopt,c),D) = (type_analyze D c;
				      case vopt of 
					NONE => D
				      | SOME v => insert_con (D,v,c))
	   
	   val D = foldl folder D eFormals
	   val _ = type_analyze  D body_type
	 in ()
	 end
       | (ExternArrow_c (args,body),Type_k) => 
	 let
	   val _ = map (type_analyze D) args
	   val _ = type_analyze D body
	 in ()
	 end
       | (v as (Var_c var),kind) => 
	 let
	   val _ = sub_kind(D,find_max_kind (D,var),kind)
	     handle Unbound =>
	       (printem ["UNBOUND VARIABLE = ",var2string var,
			   " CONTEXT IS \n"];
		NilContext.print_context D;
		error  (locate "con_valid") ("variable "^(var2string var)^" not in context"))
	 in ()
	 end
       | (Let_c (Parallel,cbnds,con),_) => error' "Parallel bindings not supported yet"
       | (Let_c (Sequential,cbnds,con),kind) =>
	   let
	     fun check_bnd (D,maker,Tag) (var,formals,body) = 
	       let
		 val kind = lambda_valid(D,Tag,formals,body)
		 val var' = derived_var var
		 val bnd = maker (var',formals,body)
		 val con = Let_c (Sequential,[bnd],Var_c var')
		 val D = insert_stdkind_equation(D,var,con,kind)
	       in D
	       end
	     
	     val body_var_opt = strip_var con
	       
	     fun loop ([],D) = con_analyze(D,con,kind)
	       | loop (bnd::rest,D) =
	       (case bnd
		  of Open_cb (args as (var,formals,body)) =>
		    if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
		      ignore(sub_kind(D,lambda_valid(D,Open,formals,body),kind))
		    else loop(rest,check_bnd (D,Open_cb,Open) args)

		   | Code_cb (args as (var,formals,body)) => 
		      if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
			ignore(sub_kind(D,lambda_valid(D,Code,formals,body),kind))
		      else loop(rest,check_bnd (D,Code_cb,Code) args)
		   | Con_cb (var,con) => loop(rest,insert_stdkind_equation(D,var,con,con_valid(D,con))))

	   in loop (cbnds,D)
	   end
       | (Typeof_c exp,Type_k) => ignore(exp_valid(D,exp))
       | (Closure_c (code,env),kind1 as Arrow_k(Closure,_,_)) => 
	 let
	   val code_kind =  con_valid (D,code)
	   (*Or could synthesize on the environment*)
	   val ((v,klast),vklist,body_kind) = 
	     case code_kind of
	       Arrow_k (Code ,c_parm::vklist,body_kind) => (c_parm,vklist,body_kind)
	     | _ => (c_error(D,constructor,"Invalid closure: code has wrong kind"))
		 
	   val kind2 = Arrow_k(Closure,vklist,body_kind)
	   val _ = sub_kind(D,kind2,kind1)
	   val _ = (con_analyze(D,env,klast)) handle e => k_error(D,klast,"Illegal kind in Closure2?")
	 in ()
	 end
       | (Crecord_c entries,Record_k lvkseq) => 
	 let
	   val (labels,cons) = unzip entries
	   val (labels',vks) = unzip ((Sequence.maptolist (fn ((l,v),k) => (l,(v,k)))) lvkseq)
	   val _ = con_analyze_vk_list (D,cons,vks)
	 in
	   if eq_list (eq_label,labels,labels') then () 
	   else c_error(D,constructor,"Illegal labels")
	 end
       | (Proj_c (rvals,label),kind1) => 
	 let
	   (*This will never be dependent*)
	   val record_kind = con_valid (D,rvals)
	     
	   val kind2 = project_from_kind_nondep (record_kind,label)
	 in if sub_kind(D,kind2,kind1) then ()
	    else k_error(D,kind1, "Trying to analyze projection at wrong kind")
	 end
       | (App_c (cfun_orig,actuals),kind1) => 
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
		 
	   val D = con_analyze_vk_list(D,actuals,formals)
	 in if sub_kind(D,body_kind,kind1) then ()
	    else k_error(D,kind1, "Analyzed kind and return kind from app don't match")
	 end
       | (Typecase_c {arg,arms,default,kind=given_kind},kind) => 
	 let
	   val given_kind = kind_valid (D,given_kind)
	   val origD = D
	   fun doarm (pcon,args,body) = 
	     let
	       fun folder ((v,k),D) =
		 let val k = kind_valid (origD,k)
		 in
		   (Var_c v,insert_stdkind(D,v,k))
		 end
	       val (argcons,D) = foldl_acc folder D args
	       val _ = pcon_analyze (D,pcon,argcons)
	       val _ = con_analyze(D,body,given_kind)
	     in ()
	     end
	   val _ = app doarm arms
	   val _ = type_analyze D arg
	   val _ = con_analyze (D,default,given_kind)
	 in if sub_kind(D,given_kind,kind) then () else k_error(D,kind,"Return type for typecase doesn't match")
	 end
       | (Annotate_c (annot,con),kind) => con_analyze(D,con,kind)
       | _ => ck_error (D,constructor,kind,"Illegal Con/Kind combination in analysis")
	 )
    end
  and con_analyze_vk_list (D,cons,vks) = 
    let
      val origD = D
      fun folder (c,(v,k),(D,subst)) = 
	let val k = substConInKind subst k
	    val _ = con_analyze(origD,c,k)
	    val subst = Subst.C.sim_add subst (v,c)
	    val D = insert_kind_equation(D,v,c,k)
	in(D,subst)
	end
      val (D,_) = (foldl2 folder (D,Subst.C.empty()) (cons,vks)
		   handle e => (print "Problem with analyzing vk_list\n";raise e))
    in D
    end
  and type_analyze(D,c) = con_analyze(D,c,Type_k)
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
      val k = flagtimer (con_valid_top,"Tchk:con_valid",con_valid') (D,constructor)
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


  
  and con_valid' (D : context, constructor : con) : kind = 
    let
      val kind = 
	(case constructor of
	   (Prim_c (pcon,args)) => ((flagtimer (con_valid_prof,"Tchk:pcon_valid",pcon_analyze) (D,pcon,args);
				     SingleType_k(constructor))
				    handle e => c_error(D,constructor,"Prim con invalid"))
	   | (AllArrow_c {openness,effect,isDependent,
			  tFormals,eFormals,fFormals,body_type}) =>
	     (
	      type_analyze (D,constructor);
	      SingleType_k(constructor)
	      )
	 | (ExternArrow_c (args,body)) => 
	   (
	    type_analyze (D,constructor);
	    SingleType_k(constructor)
	    )
	 | (Mu_c (is_recur,defs)) =>
	   let
	     val D =
	       if is_recur then
		 let fun folder((v,_),D) = insert_stdkind (D,v,Type_k)
		 in  Sequence.foldl folder D defs
		 end
	       else D
		 
	     val _ = app (fn (_,c) => type_analyze (D,c)) (Sequence.toList defs)

	     fun mapper (i,(v,c)) = 
		 let val label = generate_tuple_label(i+1)
		 in((label,derived_var v),SingleType_k(Proj_c(constructor,label)))
		 end
	     val entries = Sequence.mapcount mapper defs
		 
	   in  Record_k(entries)
	   end
	 | (v as (Var_c var)) => 
	   let
	     val kind = (find_max_kind (D,var)
			 handle Unbound =>
			   (printem ["UNBOUND VARIABLE = ",var2string var,
				       " CONTEXT IS \n"];
			    NilContext.print_context D;
			    error  (locate "con_valid") ("variable "^(var2string var)^" not in context")))
	   in
	     kind
	   end
	 | (Let_c (Parallel,cbnds,con)) => error' "Parallel bindings not supported yet"
	 | (Let_c (Sequential,cbnds,con)) =>
	   let
		
	     fun check_bnd ((D,subst),maker,Tag) (var,formals,body) = 
	       let
		 val kind = lambda_valid(D,Tag,formals,body)
		 val var' = derived_var var
		 val bnd = maker (var',formals,body)
		 val con = Let_c (Sequential,[bnd],Var_c var')
		 val D = insert_stdkind_equation(D,var,con,kind)
		 val subst = Subst.C.addr(subst,var,con)
	       in (D,subst)
	       end
	     
	     val body_var_opt = strip_var con
	       
	     fun loop ([],(D,subst)) = substConInKind subst (con_valid(D,con))
	       | loop (bnd::rest,state as (D,subst)) =
	       (case bnd
		  of Open_cb (args as (var,formals,body)) =>
		    if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
		      substConInKind subst (lambda_valid(D,Open,formals,body))
		    else loop(rest,check_bnd (state,Open_cb,Open) args)
		   | Code_cb (args as (var,formals,body)) => 
		    if (null rest) andalso eq_opt(eq_var,SOME var,body_var_opt) then
		      substConInKind subst (lambda_valid(D,Code,formals,body))
		    else loop(rest,check_bnd (state,Code_cb,Code) args)
		   | Con_cb (var,con) => 
		      let val D = insert_stdkind_equation(D,var,con,con_valid(D,con))
			  val subst = Subst.C.addr (subst,var,con)
		      in loop(rest,(D,subst))
		      end)

	   in loop (cbnds,(D,Subst.C.empty()))
	   end
	 | (Typeof_c exp) => (SingleType_k(exp_valid (D,exp)))
	 | (Closure_c (code,env)) => 
	   let
	     val code_kind =  con_valid (D,code)
	     val ((v,closure_k),vklist,body_kind) = 
	       case code_kind of
		 Arrow_k (Code ,c_parm::vklist,body_kind) => (c_parm,vklist,body_kind)
	       | _ => (c_error(D,constructor,"Invalid closure: code component does not have correct kind"))
	     val _ = (con_analyze (D,env,closure_k)) handle e => k_error(D,closure_k,"Illegal kind in Closure?")
	     val kind = Arrow_k(Closure,vklist,body_kind)
	   in varConKindSubst v env kind
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
		Listops.no_dups (fn (x,y) => let val _ = (printem ["comparing: ",label2string x, " and ",
								   label2string y," : "])
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

	     val (formal_vars,formal_kinds) = unzip formals		   
	     val _ = app2 (fn (c,k) => con_analyze(D,c,k)) (actuals,formal_kinds)

	     fun folder (v,c,subst) = NilSubst.C.sim_add subst (v,c)
	     val subst = Listops.foldl2 folder (NilSubst.C.empty()) (formal_vars,actuals)
	   in  
	     substConInKind subst body_kind
	   end
	 | (Typecase_c {arg,arms,default,kind=given_kind}) => 
	   let
	     val given_kind = kind_valid (D,given_kind)
	     val origD = D
	     fun doarm (pcon,args,body) = 
	       let
		 fun folder ((v,k),D) =
		   let val k = kind_valid (origD,k)
		   in
		     (Var_c v,insert_kind(D,v,k))
		   end
		 val (argcons,D) = foldl_acc folder D args
		 val _ = pcon_analyze (D,pcon,argcons)
		 val _ = con_analyze(D,body,given_kind)
	       in ()
	       end
	     val _ = (app doarm arms;
		      type_analyze (D,arg);
		      con_analyze (D,default,given_kind)) 
	       handle e => (print "Problem with Typecase arms\n";raise e)
	   in
	     given_kind
	   end
	 | (Annotate_c (annot,con)) => con_valid (D,con)
	   )
    in
      kind
    end
  and lambda_valid (D,Tag,formals,body) = 
    let 
      val (formals,D) = vklist_valid (D,formals)
      val body_kind = con_valid(D,body)
    in Arrow_k(Tag,formals,body_kind)
    end		  

  (*PRE: kind1 and kind2 not necessarily normalized *)
  and sub_kind   (D,k1,k2) = subeq_kind false ((D,Trail.empty),k1,k2)
  and kind_equiv (D,k1,k2) = subeq_kind true  ((D,Trail.empty),k1,k2)
  and kind_equiv' args     = subeq_kind false args

  and subeq_phase true (p1,p2) = p1 = p2
    | subeq_phase false (p1,p2) = sub_phase(p1,p2)

  and subeq_kind is_eq ((D,T),kind1,kind2) = 
    let
      val _ = push_subkind(kind1,kind2,D)

      fun sub_one ((var1,kind1),(var2,kind2),(D,rename1,rename2)) = 
	let
	  val kind1 = alphaCRenameKind rename1 kind1
	  val kind2 = alphaCRenameKind rename2 kind2

	  val (D',rename1,rename2) = 
	    let
	      val var' = 
		if NilContext.bound_con(D,var1) then 
		  if NilContext.bound_con(D,var2) then
		    derived_var var1
		  else var2
		else var1
	      val D = insert_kind (D,var',kind1)
	      val rename1 = if eq_var (var1,var') then rename1 else Alpha.rename (rename1,var1,var')
	      val rename2 = if eq_var (var2,var') then rename2 else Alpha.rename (rename2,var2,var')
	    in (D,rename1,rename2)
	    end
	in
	  (subeq_kind is_eq ((D,T),kind1,kind2),(D',rename1,rename2))
	end

      fun sub_all D (vks1,vks2) = 
	foldl_all2 sub_one (D,Alpha.empty_context(),Alpha.empty_context()) (vks1,vks2)

      val res = 
      (case (kind1,kind2) 
	 of (Type_k, Type_k) => true
	  | (Type_k,_) => false
	  | (Single_k _,_) => subeq_kind is_eq ((D,T), kind_standardize(D,kind1), kind2)
	  | (_,Single_k _) => subeq_kind is_eq ((D,T), kind1, kind_standardize(D,kind2))
	  | (SingleType_k _ ,Type_k) => true
	  | (SingleType_k (c1),SingleType_k (c2)) => 
	     subtimer("SubKind:st",type_equiv')((D,T),c1,c2)
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
		val (formals_ok,(D,rename2,rename1)) = sub_all D (formals2,formals1)
		(* Notice reversal of order!!!*)
		val return1 = alphaCRenameKind rename1 return1
		val return2 = alphaCRenameKind rename2 return2
	      in
		formals_ok andalso 
		same_openness (openness1,openness2) andalso 
		  subeq_kind is_eq ((D,T),return1,return2)
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

  and subtype     (D,c1,c2)     = con_equiv  (D,c1,c2,Type_k,true)
  and type_equiv  (D,c1,c2)     = con_equiv  (D,c1,c2,Type_k,false)
  and type_equiv' ((D,T),c1,c2) = con_equiv_wrapper ((D,T),c1,c2,Type_k,false)

  (* Given a well-formed context D and well-formed constructors c1 and c2 of kind k,
   return whether they are equivalent at well-formed kind k. *)
  (* note: wrapper function at end of file redefines con_equiv! *)
  and con_equiv (D,c1,c2,k,sk) : bool = 
    flagtimer (equiv_total_profile,"Tchk:equiv_total", con_equiv_wrapper) ((D,Trail.empty),c1,c2,k,sk)
  and con_equiv_wrapper args = 
    let

      val alpha_subequiv_con  = fn st => flagtimer(equiv_profile,"Tchk:Equiv:alpha_subequiv",alpha_subequiv_con st)
      val alphaCRenameKind = fn rename => flagtimer(equiv_profile,"Tchk:Equiv:alphaCRenameKind",alphaCRenameKind rename)
      val alphaECRenameCon = fn rename => flagtimer(equiv_profile,"Tchk:Equiv:alphaECRenameCon",alphaECRenameCon rename)
      val alphaCRenameCon  = fn rename => flagtimer(equiv_profile,"Tchk:Equiv:alphaCRenameCon",alphaCRenameCon rename)
      val alphaCRenameExp  = fn rename => flagtimer(equiv_profile,"Tchk:Equiv:alphaCRenameExp",alphaCRenameExp rename)

      val kind_of = flagtimer(equiv_profile,"Tchk:Equiv:kind_of",kind_of)
      val type_of = flagtimer(equiv_profile,"Tchk:Equiv:type_of",Normalize.type_of)

      val find_kind_equation = flagtimer(equiv_profile,"Tchk:Equiv:find_kind_equation",NilContext.find_kind_equation)

      fun bind_kind_eqn'((D,alpha),var,con,kind) =
	if NilContext.bound_con (D,var) then
	  let
	    val vnew = Name.derived_var var
	    val D = insert_kind_equation(D,vnew,con,kind)
	    val alpha = Alpha.rename(alpha,var,vnew)
	  in (D,alpha)
	  end
	else (insert_kind_equation(D,var,con,kind),alpha)

      fun bind_kind_eqn(state as (D,alpha),var,con,kind) = 
	bind_kind_eqn'(state,var,alphaCRenameCon alpha con,alphaCRenameKind alpha kind)

      fun bind_eqn(state as (D,alpha),var,con) = 
	let val con = alphaCRenameCon alpha con
	in
	  if NilContext.bound_con (D,var) then
	    let
	      val vnew = Name.derived_var var
	      val D = insert_equation(D,vnew,con)
	      val alpha = Alpha.rename(alpha,var,vnew)
	    in (D,alpha)
	    end
	  else (insert_equation(D,var,con),alpha)
	end

      val bind_kind_eqn = flagtimer(equiv_profile,"Tchk:Equiv:bind_kind_eqn",bind_kind_eqn)
      val bind_kind_eqn' = flagtimer(equiv_profile,"Tchk:Equiv:bind_kind_eqn'",bind_kind_eqn')
      val bind_eqn      = flagtimer(equiv_profile,"Tchk:Equiv:bind_eqn",bind_eqn)

      fun instantiate_formals(state,vklist,actuals) = 
	let
	  fun folder ((var,k),actual,state) = bind_kind_eqn(state,var,actual,k)
	in foldl2 folder state (vklist,actuals)
	end

      fun con_equiv (args as (state,c1,c2,k,sk)) : bool = 
	let val res = (alpha_subequiv_con sk (c1,c2)) orelse (con_equiv' args)
	in res
	end

      and con_equiv' ((D,T),c1,c2,k,sk) = 
	let


	  fun compare_list2 ([],c) = false
	    | compare_list2 (a::b,c) = alpha_subequiv_con sk (a,c) orelse compare_list2 (b,c)

	  fun compare_list1 (a,[]) = false
	    | compare_list1 (a,b::c) = alpha_subequiv_con sk (a,b) orelse compare_list1 (a,c)

	  (*compare(D,c1s,done1,c2s,done2)
	   * PRE:  not (done1 and done2)
	   *    :  Forall ci in c1s and cj in c2s, not (alpha_sub_equiv_con sk (ci,cj))
	   *)
	  fun compare (D,cc1 as c1::_,done1,cc2 as c2::_,done2) = 
	    let
	      val (D,c1,path1) = 
		if done1 then (D,c1,false)
		else 
		  let
		    val ((D,alpha),c1,path) = context_beta_reduce ((D,Alpha.empty_context()),c1)
		    val c1 = alphaCRenameCon alpha c1
		  in (D,c1,path)
		  end

	      val (D,c2,path2) = 
		if done2 then (D,c2,false)
		else 
		  let
		    val ((D,alpha),c2,path) = context_beta_reduce ((D,Alpha.empty_context()),c2)
		    val c2 = alphaCRenameCon alpha c2
		  in (D,c2,path)
		  end

	      val eq = (((not done1) andalso compare_list1(c1,c2::cc2)) orelse
			((not done2) andalso compare_list2(c1::cc1,c2)))
	    in 
	      if eq then (D,c1,c2,true)
	      else
		let
		  val (c1,done1) = 
		    if path1 then
		      (case find_kind_equation (D,c1)
			 of SOME con => (con,false)
			  | NONE => (strip_annotate c1,true))
		    else (strip_annotate c1,true)
		  val (c2,done2) = 
		    if path2 then
		      (case find_kind_equation (D,c2)
			 of SOME con => (con,false)
			  | NONE => (strip_annotate c2,true))
		    else (strip_annotate c2,true)
		in if done1 andalso done2 then (D,c1,c2,false)
		   else compare(D,c1::cc1,done1,c2::cc2,done2)
		end
	    end
	  val _ = push_eqcon(c1,c2,D)
	  val res = 
	    case k of
	      Type_k => 
		if !compare_paths then
		  (let

		    val (D,c1,c2,eq) = compare (D,[c1],false,[c2],false)
		  in eq orelse con_structural_equiv((D,T),c1,c2,sk)
		  end handle any =>
		     (NilContext.print_context (NilContext.con_error_context (D,c1));
		      NilContext.print_context (NilContext.con_error_context (D,c2));
		      Ppnil.pp_con c1; print "\n";
		      Ppnil.pp_con c2; print "\n";
		      raise any))
		else
		  let
		    val (D,c1) = context_reduce_hnf(D,c1)
		    val (D,c2) = context_reduce_hnf(D,c2)
		  in con_structural_equiv((D,T),c1,c2,sk)
		  end
	      
	    | SingleType_k c => true
	    | Single_k c => true
	    | Record_k lvk_seq => 
		let 
		  val (D,c1) = context_reduce_hnf(D,c1)
		  val (D,c2) = context_reduce_hnf(D,c2)
		  fun folder (((l,v),k),(state as (D,alpha),equal)) =
		    let 
		      val k = alphaCRenameKind alpha k
		      val equal = equal andalso con_equiv'((D,T),Proj_c(c1,l),Proj_c(c2,l),k,sk)
		      val state = bind_kind_eqn'(state,v,Proj_c(c1,l),k)  
		    in  (state,equal)
		    end
		in (case (c1,c2) 
		      of (Mu_c _,Mu_c _) => alpha_subequiv_con sk (c1,c2) orelse con_structural_equiv((D,T),c1,c2,sk)
		       | _ => #2 (Sequence.foldl folder ((D,Alpha.empty_context()),true) lvk_seq))
		end
	    | Arrow_k (openness,vklist,k) => 
		let 
		  fun folder ((v,k),(D,alpha)) = 
		    let val k = alphaCRenameKind alpha k
		    in 
		      if NilContext.bound_con(D,v) then
			let val vnew = fresh_var()
			    val D = insert_kind(D,vnew,k)
			    val alpha = Alpha.rename (alpha,v,vnew)
			in (Var_c vnew,(D,alpha))
			end
		      else (Var_c v,(insert_kind(D,v,k),alpha))
		    end
		  val (args,(D,rename)) = foldl_acc folder (D,Alpha.empty_context()) vklist
		  val k = alphaCRenameKind rename k
		  val c1 = App_c(c1,args)
		  val c2 = App_c(c2,args)
		in  con_equiv'((D,T),c1,c2,k,sk) 
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
      and vklist_equiv ((D,T), vklist1, vklist2) : context * Alpha.alpha_context * bool =
	let 
	  fun folder((v1,k1),(v2,k2),(D,rename,match)) = 
	    let 
	      val k1 = alphaCRenameKind rename k1
	      val k2 = alphaCRenameKind rename k2

	      val match = match andalso kind_equiv' ((D,T),k1,k2)
	      val (D,rename) = 
		if NilContext.bound_con (D,v1) then
		  let
		    val vnew = Name.derived_var v1
		    val D = insert_kind(D,vnew,k1)
		    val rename = Alpha.rename(rename,v1,vnew)
		    val rename = Alpha.rename(rename,v2,vnew)
		  in (D,rename)
		  end
		else (insert_kind(D,v1,k1),Alpha.rename(rename,v2,v1))
	    in  (D,rename,match)
	    end
	in  
	  if (length vklist1 = length vklist2)
	    then Listops.foldl2 folder (D,Alpha.empty_context(),true) (vklist1, vklist2)
	  else (D, Alpha.empty_context(), false)
	end
      
      and vlistopt_clist_equiv ((D,T), cRename, vclist1, vclist2,sk) : context * (Alpha.alpha_context * Alpha.alpha_context) * bool =
	let 
	  fun folder((vopt1, c1), (vopt2, c2), (D, eRename, cRename, match)) = 
	    let 
	      val c1 = alphaECRenameCon (eRename,cRename) c1
	      val c2 = alphaECRenameCon (eRename,cRename) c2
	      val match = match andalso con_equiv((D,T),c1,c2,Type_k,sk)
		
	      fun bind(D,eRename,v,c) = 
		if NilContext.bound_exp (D,v) then 
		  let val vnew = Name.derived_var v
		  in (Alpha.rename (eRename,v,vnew),insert_con(D,vnew,c)) end
		else (eRename,insert_con(D,v,c))

	      val (eRename,D) = 
		(case (vopt1,vopt2) of
		   (NONE,NONE) => (eRename,D)
		 | (SOME v1,NONE) => bind(D,eRename,v1,c1)
		 | (NONE,SOME v2) => bind(D,eRename,v2,c2)
		 | (SOME v1,SOME v2) => 
		   if NilContext.bound_exp (D,v1) then
		     if NilContext.bound_exp (D,v2) then
		       let val vnew = Name.derived_var v1
			   val eRename = Alpha.rename (eRename,v1,vnew)
			   val eRename = Alpha.rename (eRename,v2,vnew)
		       in (eRename,insert_con(D,vnew,c1)) end
		     else (Alpha.rename (eRename,v1,v2),insert_con(D,v2,c2))
		   else (if eq_var(v1,v2) then eRename else Alpha.rename (eRename,v2,v1),
			 insert_con(D,v1,c1)))

	    in  (D,eRename,cRename,match)
	    end
	in  if (length vclist1 = length vclist2)
	      then 
		let val (D, eRename, cRename, match) = foldl2 folder (D,Alpha.empty_context(),cRename,true) (vclist1,vclist2)
		in  (D, (eRename, cRename), match)
		end
	    else (D, (Alpha.empty_context(), cRename), false)
	end

      and con_structural_equiv ((D,T),c1,c2,sk) : bool (*kind option *)=
	let

	  fun mu_equate ((D,T),con1,con2) =
	    (print "Checking trail\n";
	     (Trail.equal(T,(con1,con2))) orelse
	     let
	       val T = Trail.equate(T,(con1,con2))
	       val con1 = expandMuType(D,con1)
	       val con2 = expandMuType(D,con2)
	       val _ = print "Adding to trail and unrolling\n"
	     in con_equiv((D,T),con1,con2,Type_k,false)
	     end
	     )

	  val res =   
	      (case (c1,c2) of
		 (Prim_c(Record_c (labs1,vlistopt1),clist1), 
		  (Prim_c(Record_c (labs2,vlistopt2),clist2))) =>
		 (eq_list(eq_label,labs1,labs2) andalso
		  let fun combine(vlistopt,clist) =
		    (case vlistopt of
		       NONE => map (fn c => (NONE, c)) clist1
		     | SOME vars => map2 (fn (v,c) => (SOME v, c)) (vars,clist))
		      val vclist1 = combine(vlistopt1, clist1)
		      val vclist2 = combine(vlistopt2, clist2)
		  in  #3(vlistopt_clist_equiv((D,T),Alpha.empty_context(),vclist1,vclist2,sk))
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
		 in  
		   res1 andalso 
		   eq_list(fn (c1,c2) => con_equiv((D,T),c1,c2,k,sk),
					  clist1,clist2)
		 end
	       | (Prim_c(Vararg_c(o1,eff1),[argc1,resc1]),
		  Prim_c(Vararg_c(o2,eff2),[argc2,resc2])) => 
		 o1 = o1 andalso (sub_effect(sk,eff1,eff2)) andalso
		 con_equiv((D,T),argc2,argc1,Type_k,sk) andalso
		 con_equiv((D,T),resc1,resc2,Type_k,sk)
	       | (Prim_c(pcon1,clist1), Prim_c(pcon2,clist2)) => 
		 let
		   val sk' = sk andalso (NilUtil.covariant_prim pcon1)
		 in
		   NilUtil.primequiv(pcon1,pcon2) andalso 
		   eq_list(fn (c1,c2) => con_equiv((D,T),c1,c2,Type_k,sk'),
				   clist1,clist2)
		 end
	       | (Mu_c(ir1,defs1), Mu_c(ir2,defs2)) => 
		 let 
		   fun do_mu () = 
		     let 
		       
		       val vc1 = Sequence.toList defs1
		       val vc2 = Sequence.toList defs2
			 
		       fun build (D,rename) ((v1,_)::rest1,(v2,_)::rest2) = 
			 let
			   val (D,rename) = 
			     if NilContext.bound_con (D,v1) then
			       if NilContext.bound_con (D,v2) then
				 let val vnew = Name.derived_var v1
				     val rename = Alpha.rename (rename,v1,vnew)
				     val rename = Alpha.rename (rename,v2,vnew)
				 in (insert_kind(D,vnew,Type_k),rename) end
			       else (insert_kind(D,v2,Type_k),Alpha.rename(rename,v1,v2))
			     else (insert_kind(D,v1,Type_k),
				   if eq_var(v1,v2) then rename 
				   else Alpha.rename (rename,v2,v1))
			 in  build (D,rename) (rest1,rest2)
			 end
			 | build acc _ = acc

		       val (D,rename) = if (ir1 andalso ir2) then build (D,Alpha.empty_context()) (vc1,vc2) 
					else (D,Alpha.empty_context())

		       fun pred ((_,c1),(_,c2)) = 
			 let val c1 = alphaCRenameCon rename c1
			     val c2 = alphaCRenameCon rename c2
			 in con_equiv((D,T),c1,c2, Type_k, false) (* no subtyping *) end
		     in  
		       (ir1 = ir2) andalso eq_list(pred,vc1,vc2)
		     end

		 in
		   (flagtimer (equiv_profile,"Tchk:Equiv:Mu",do_mu) ())
		 end
	     | (AllArrow_c {openness=o1,effect=eff1,isDependent=i1,
			    tFormals=t1,eFormals=e1,fFormals=f1,body_type=b1},
		 AllArrow_c {openness=o2,effect=eff2,isDependent=i2,
			     tFormals=t2,eFormals=e2,fFormals=f2,body_type=b2}) =>
		 o1 = o1 andalso (sub_effect(sk,eff1,eff2)) andalso f1 = f2 andalso 
		 let val (D,cRename,match) = vklist_equiv((D,T),t1,t2)
		 in  match andalso
		   let val (D,rename,match) = 
		     vlistopt_clist_equiv((D,T),cRename,e2,e1,sk)
		       val b1 = alphaECRenameCon rename b1
		       val b2 = alphaECRenameCon rename b2
		   in  match andalso
		     con_equiv((D,T),b1,b2, Type_k,sk)
		   end
		 end

	     | (ExternArrow_c (clist1,c1), ExternArrow_c (clist2,c2)) => 
		 let fun mapper c = (NONE, c)
		   val (D,_,match) = vlistopt_clist_equiv((D,T),Alpha.empty_context(),
							  map mapper clist1,
							  map mapper clist2,
							  false)
		 in  type_equiv(D,c1,c2)
		 end

	     | (Crecord_c lclist, _) => 
		 error' "Crecord given to con_structural_equiv: not WHNF"
		 
	     | (Let_c _, _) => error' "Let_c given to con_structural_equiv: not WHNF"
		 
	     | (Proj_c (con1,l1), Proj_c(con2,l2)) => 
		 ((eq_label(l1,l2)) andalso (con_structural_equiv((D,T),con1,con2,false))) orelse
		 (case (con1,con2)
		    of (Mu_c _,Mu_c_) => mu_equate((D,T),c1,c2)
		     | _ => false)
	     | (Proj_c (Mu_c _,l1),Mu_c _) => mu_equate((D,T),c1,c2)
	     | (Mu_c _,Proj_c (Mu_c _,l1)) => mu_equate((D,T),c1,c2)

	     | (App_c (f1,clist1), App_c (f2,clist2)) =>
		 if con_structural_equiv((D,T),f1,f2,false) then
		   (case kind_of (D,f1) of
		      Arrow_k(openness,vklist,k) =>
			let     
			  fun folder ((v,k),c1,c2, (state as (D,alpha),equal)) =
			    if equal then 
			      let 
				val k = alphaCRenameKind alpha k
				val equal = con_equiv((D,T),c1,c2,k,false)
				val D = bind_kind_eqn'(state,v,c1,k)
			      in  (state,equal)
			      end
			    else (state,equal)
			  val (_,equal) = foldl3 folder ((D,Alpha.empty_context()),true) (vklist,clist1,clist2)
			in equal
			end
		    | _ => false)
		 else false
	     | (Var_c v, Var_c v') => eq_var(v,v')
	     | (Typecase_c {arg=arg1,arms=arms1,default=d1,kind=k1}, 
		Typecase_c {arg=arg2,arms=arms2,default=d2,kind=k2}) => 
		   let
		     fun arm_equiv ((pc1,f1,b1),(pc2,f2,b2)) = 
		       primequiv(pc1,pc2) andalso
		       let val (D,cRename,match) = vklist_equiv((D,T),f1,f2)
		       in  
			 match andalso
			 let 
			   val b1 = alphaCRenameCon cRename b1
			   val b2 = alphaCRenameCon cRename b2
			 in con_equiv((D,T),b1,b2, Type_k,sk)
			 end
		       end
		   in
		     con_structural_equiv((D,T),arg1,arg2,sk)
		     andalso con_equiv((D,T),d1,d2,k1,sk)
		     andalso kind_equiv'((D,T),k1,k2)
		     andalso eq_list (arm_equiv,arms1,arms2)
		   end
	     | (Annotate_c _, _) => error' "Annotate_c not WHNF"
	     | _ => false)
		 
	    val _ = if res then () 
		    else
		      (print "con_structural_equiv returning false\n";
		       print "c1 = "; Ppnil.pp_con c1; print "\n";
		       print "c2 = "; Ppnil.pp_con c2; print "\n";
		       printl "WITH MINIMAL CONTEXT AS";
		       print_context (cons_error_context (D,[c1,c2]));
		       print "\n")
	in res
	end 
      val res = con_equiv args
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
      Name.VarSet.app checker free_vars
    end

  and bnds_valid (D,bnds) = let val (etypes,(D,cbnds)) = (foldl_acc bnd_valid' (D,[]) bnds) in (D,rev cbnds,List.concat etypes) end
  and bnd_valid (state,bnd) = bnd_valid' (bnd,state)
  and bnd_valid'' (bnd,state) = 
    let
      val (D,cbnds) = state

      (*PRE: The decorations have already been checked, D contains the bindings.
       * Should maybe take two contexts - one for recursive,m one not?
       *)
      fun function_valid openness D (var,Function {effect,recursive,isDependent,
						   tFormals,eFormals,fFormals,
						   body,body_type}) =
	let
	  val D' = insert_kind_list (D,tFormals)
	  val D = foldl (fn ((v,nt,c),D) => (niltrace_valid(D,nt);insert_con(D,v,c))) D' eFormals
	  val D = foldl (fn (v,D) => insert_con (D,v,Prim_c (Float_c F64,[]))) D fFormals
	in
	  exp_analyze(D,body,body_type)
	end


      fun fbnd_valid (openness,defs) = 
	let
	  val origD = D
	  val bnd_types = Sequence.map_second (function_type openness) defs
	    
	  (*Checks that the decorations are well-formed*)
	  val _ = Sequence.map_second (curry2 type_analyze D) bnd_types
	  val D = Sequence.foldl (fn ((v,c),D) => insert_con(D,v,c)) D bnd_types
	  val _ = Sequence.app (function_valid openness D) defs
	in (D,Sequence.toList bnd_types)
	end
      (*tipe is already checked*)
      fun do_closure D ({code,cenv,venv,tipe}) = 
	let

	  val (code_type) = 
	    (find_con (D,code)
	     handle NilContext.Unbound =>
	       (printl ("Code pointer "^(var2string code));
		print " not defined in context";
		(error (locate "bnd_valid") "Invalid closure" handle e => raise e)))

	  val (effect,isDependent,tFormals,eFormals,fFormals,body_type) =
	    (case strip_arrow (con_head_normalize(D,code_type)) of
	       SOME {openness = Code,effect,isDependent,
		     tFormals,eFormals, fFormals,body_type} => 
		 (effect,isDependent,tFormals,eFormals,fFormals,body_type)
	     | SOME _ => 		     
		 (perr_e_c (Var_e code,code_type);
		  (error (locate "bnd_valid") "Code pointer in closure of illegal type" handle e => raise e))
	     | NONE => (perr_e_c (Var_e code,code_type);
			(error (locate "bnd_valid") "Code pointer in closure of illegal type" handle e => raise e)))

	  (* Split out the closure variables and classifiers*)
	  val ((clos_tv,clos_k),tFormals) =
	    (case tFormals
	       of c::ff => (c,ff)
		| _ => error (locate "bnd_valid") "Code doesn't expect type closure")

	  val ((clos_v_opt,clos_c),eFormals) = 
	    (case eFormals
	       of v::ff => (v,ff)
		| _ => error (locate "bnd_valid") "Code doesn't expect value closure")
	    

	  (*Check that the environments are appropriate*)
	  val _ = (con_analyze (D,cenv,kind_standardize(D,clos_k))) handle e => k_error(D,clos_k,"Illegal kind in Closure?")
	  val _ = exp_analyze  (D,venv,varConConSubst clos_tv cenv clos_c)

	  (* Create the closure type and substitute in the types of the
	   * environments
	   *)
	  val esubst = (case clos_v_opt 
			  of NONE   => Subst.E.empty()
			   | SOME v => Subst.E.sim_add (Subst.E.empty()) (v,venv))
	  val csubst = Subst.C.sim_add (Subst.C.empty()) (clos_tv,cenv)
	    
	  val closure_type = AllArrow_c {openness=Closure, effect=effect, 
					   isDependent=isDependent,
					   tFormals = tFormals,
					   eFormals = eFormals,
					   fFormals = fFormals,
					   body_type=body_type}
	  val closure_type = substExpConInCon (esubst,csubst) closure_type
	    
	  val _ = 
	    (type_equiv (D,closure_type,tipe)) orelse
	    (perr_c_c (tipe,closure_type);
	     print "code_type is "; pp_con code_type; print "\n";
	     print "con is "; pp_con closure_type; print "\n";
	     (error (locate "bnd_valid") "Type error in closure" handle e => raise e))
	in ()
	end

      val res = 
	(case bnd 
	   of Con_b (phase, cbnd) => 
	     let
	       val con = fn var => Let_c(Sequential,[cbnd],Var_c var)
	       val D = (case cbnd of 
			  Open_cb (args as (var,formals,body)) => 
			    insert_stdkind_equation(D,var,con var,lambda_valid(D,Open,formals,body))
			| Code_cb (args as (var,formals,body)) => 
			    insert_stdkind_equation(D,var,con var,lambda_valid(D,Code,formals,body))
			| Con_cb  (var,con) => insert_stdkind_equation(D,var,con,con_valid(D,con)))

	       val cbnds = cbnd::cbnds
	     in  ([],(D,cbnds))
	     end
	    | Exp_b (var,nt,exp) =>
	     let
	       val _ = niltrace_valid (D,nt)
	       val bnd_con = exp_valid (D,exp)
	       val D = insert_con (D,var,bnd_con)
	     in
	       ([(var,bnd_con)],(D,cbnds))
	     end
	    | (Fixopen_b defs) => let val (D,etypes) = fbnd_valid(Open,defs) in (etypes,(D,cbnds)) end
	    | (Fixcode_b defs) => let val (D,etypes) = fbnd_valid(Code,defs) in (etypes,(D,cbnds)) end
	    | Fixclosure_b (is_recur,defs) => 
	     let
	       val origD = D
	       val (vars,closures) = unzip (Sequence.toList defs)
	       val tipes = map (fn cl => #tipe cl) closures
	       val _ = app (curry2 type_analyze D) tipes
	       val bnd_types = zip vars tipes
	       val D = insert_con_list (D,bnd_types)
	       val _ = if is_recur 
			 then app (do_closure D) closures
		       else app (do_closure origD) closures
	     in
	       (bnd_types,(D,cbnds))
	     end)
    in res
    end
  and exp_analyze(D : context, exp : exp, con : con) : unit = 
    let
      val con' = exp_valid(D,exp)
    in
       ignore (subtimer("exp_analyze:st",subtype)(D,con',con) orelse 
	       (perr_c_c (con,con');
		e_error(D,exp,"Expression cannot be given required type")))
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

      fun conditionCode_valid (D,arg) = 
	(case arg 
	   of Exp_cc exp       => exp_analyze(D,exp,bool_con)
	    | And_cc (cc1,cc2) => (conditionCode_valid(D,cc1);
				   conditionCode_valid(D,cc2))
	    | Or_cc (cc1,cc2)  => (conditionCode_valid(D,cc1);
				   conditionCode_valid(D,cc2))
	    | Not_cc cc        => conditionCode_valid(D,cc))
	   
      (*Do an application node. *)
      fun do_app (openness,app,cons,texps,fexps) =
	let           
	  val subtype    = subtimer("Tchk:Exp:App:st",subtype)
	  val con = exp_valid (D,app)
	    
	  val {openness = openness', tFormals, eFormals, fFormals, body_type, isDependent,...} = 
	    (case strip_arrow (con_head_normalize(D,con)) of
	       SOME c => c
	     | NONE => (perr_c (con_head_normalize(D,con));
			e_error(D,exp,"Application of non-arrow expression" )))

	  val _ = 
	    if same_openness (openness,openness') then
	      (case (openness,app) of
		 (Code,Var_e _) => ()
	       | (Code,_) => e_error(D,exp,"code applied to non-variable")
	       | _ => ())
	    else e_error(D,exp,"Error in application - different openness")
	      
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
	      
	  local
	    fun bind_kind(D,cRename,rev_bnds,var,actual,kind) = 
	      if NilContext.bound_con (D,var) then
		let
		  val vnew = Name.derived_var var
		  val D = insert_kind_equation(D,vnew,actual,kind)
		  val cRename = Alpha.rename(cRename,var,vnew)
		in (D,cRename,(Con_cb(vnew,actual))::rev_bnds)
		end
	      else (insert_kind_equation(D,var,actual,kind),cRename,(Con_cb(var,actual))::rev_bnds)
	      
	    fun do_one ((var,kind),actual,(D,rename,cbnds)) = 
	      let
		val kind = alphaCRenameKind rename kind
		val kind = kind_standardize(D,kind)
		val _ = con_analyze(D,actual,kind)
	      in bind_kind (D,rename,cbnds,var,actual,kind)
	      end
	  in	      
	    val (D,rename,rev_bnds) = 
	      (foldl2 do_one (D,Alpha.empty_context(),[]) (tFormals,cons))
	      handle e => (print "Possible Formal/Actual length mismatch?\n";raise e)
	    val cbnds = rev rev_bnds
	  end

	  local
	    (*Expression variables aren't inter-dependent?*)

	    fun do_one ((var_opt,con),exp) = 
		let
		  val con = alphaCRenameCon rename con 
		  val _ = exp_analyze(D,exp,con)
		in (case var_opt of SOME var => SOME (var,con) | NONE => NONE)
		end
	    val pairs = ((zip eFormals texps)
			 handle _ => e_error(D,exp,"Mismatched formal/actual lengths"))
	  in
	    val types = List.mapPartial do_one pairs
	  end


	  val body_type = alphaCRenameCon rename body_type
	  val body_type = 
	    if isDependent then 
	      removeDependence types body_type
	    else
	      body_type
	in makeLetC cbnds body_type
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
	       val _ = type_analyze (D,con)
	       fun check exp = 
		 let
		   val con' = exp_valid (D,exp)
		   val _ = 
		     (subtype (D,con',con)) orelse
		     (e_error(D,Const_e value,"Vector contains expression of incorrect type" handle e => raise e))
		 in()
		 end
	     in
	       (Array.app (fn e => exp_analyze(D,e,con)) vec;
		Prim_c (Vector_c,[con]))
	     end
	 | tag (atag,con) => (type_analyze (D,con);Prim_c (Exntag_c,[con]))
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
	    | do_default (D,SOME e,_,result_type) = exp_analyze(D,e,result_type)
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
		(type_analyze(D,result_type);
		 
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
		   val _ = type_analyze(D,sumtype)
		   val sumtype = con_head_normalize(D,sumtype)
		   fun mk_sum (D,field) = convert_sum_to_special (sumtype,field)
		   fun check_arg (D,c) = subtimer("Tchk:Exp:Swt:Arg:st",subtype)(D,c,sumtype)
		 in
		   sum_exn (mk_sum,check_arg) (D,arg,bound,arms,default,result_type)
		 end

		| Ifthenelse_e {arg,thenArm,elseArm,result_type} => 
		 let
		   val _ = conditionCode_valid (D,arg)
		   val _ = type_analyze(D,result_type)
		   val _ = exp_analyze(D,thenArm,result_type)
		   val _ = exp_analyze(D,elseArm,result_type)
		 in result_type
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
		      type_analyze (D,result_type);
		      
		      if same_intsize (size,strip_int(D,argcon)) then ()
		      else e_error(D,Switch_e switch, "Integer size mismatch in int switch");
			
		      app (fn exp => subtimer("Tchk:Exp:Intsw:ea",exp_analyze) (D,exp,result_type')) armexps;
			  
		      do_default (D,default,arms,result_type')
		      )
		 in
		   result_type
		 end
		| Typecase_e {arg,arms,default,result_type} =>
		 let
		   val _ = type_analyze(D,result_type)
		   val origD = D
		   fun doarm (pcon,args,body) = 
		     let
		       fun folder ((v,k),D) =
			 let val k = kind_valid (origD,k)
			 in
			   (Var_c v,insert_kind(D,v,k))
			 end
		       val (argcons,D) = foldl_acc folder D args
		       val _ = pcon_analyze (D,pcon,argcons)
		       val _ = exp_analyze(D,body,result_type)
		     in ()
		     end
		   val _ = (app doarm arms;
			    type_analyze (D,arg);
			    exp_analyze (D,default,result_type))
		 in result_type
		 end)
	       
	in res 
	end

      (* Check a primitive node
       *)
      fun prim_valid (D,prim,cons,exps) = 
	let 
	  val orig_exp = Prim_e(NilPrimOp prim,cons,exps)
	    
	  fun project_sum_xxx (D,argcon,argexp,k) = 
	    let
	      val _ = type_analyze(D,argcon)
	      val argcon = con_head_normalize(D,argcon)
	      val argcon' = convert_sum_to_special(argcon,k)
	      val _ = exp_analyze (D,argexp,argcon')
	    in projectSumType(D,argcon,k)  (*Already normal!*)
	    end

	  fun sum_helper(D,sumcon,sumtype) = 
	    let
	      val _ = type_analyze(D,sumcon)

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
		let val _ = type_analyze (D,argcon)
		  val subtype    = subtimer("Tchk:Exp:Prim:Roll:st",subtype)
		in if subtype(D,exp_valid (D,exp),expandMuType(D,argcon) ) then argcon
		   else e_error(D,orig_exp,"Error in roll")
		end

	    | (unroll,[con],[exp]) =>
		let val _ = type_analyze (D,con)
		  val subtype    = subtimer("Tchk:Exp:Prim:Unroll:st",subtype)
		in if subtype(D,exp_valid (D,exp),con) then expandMuType (D,con)
		   else e_error(D,orig_exp,"Error in unroll")
		end

	    | (make_exntag,[argcon],[]) => 
		let val _ = type_analyze(D,argcon)
		in Prim_c (Exntag_c,[argcon])
		end

	    | (inj_exn name,[],[exp1,exp2]) => 
		if subtimer("Tchk:Exp:Prim:IExn:st",subtype)(D,exp_valid (D,exp2),strip_exntag(D,exp_valid (D,exp1))) then
		  Prim_c (Exn_c,[])
		else e_error(D,orig_exp,"Type mismatch in exception injection")
	    | (make_vararg (openness,effect),[argc,resc],[e]) =>
	       let
		 val etype = AllArrow_c{openness=openness,effect=effect,isDependent=false,
					tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,body_type=resc}
		 val _ = type_analyze(D,etype)
		 val _ = exp_analyze (D,e,etype)
	       in  Prim_c(Vararg_c(openness,effect),[argc,resc])
	       end
	    | (make_onearg (openness,effect),[argc,resc],[e]) =>  
	       let
		 val etype = Prim_c(Vararg_c(openness,effect),[argc,resc])
		 val _ = type_analyze(D,etype)
		 val _ = exp_analyze (D,e,etype)
	       in AllArrow_c{openness=openness,effect=effect,isDependent=false,
			     tFormals=[],eFormals=[(NONE,argc)],fFormals=0w0,body_type=resc}
	       end
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
	       val (D,cbnds,etypes) = bnds_valid (D,bnds)
	       val con = exp_valid (D,exp)
	     in removeDependence etypes (makeLetC cbnds con)
	     end
	 | Prim_e (NilPrimOp prim,cons,exps) => prim_valid (D,prim,cons,exps)
	 | Prim_e (PrimOp prim,cons,exps) =>   
	     let 
	       val _ = app (curry2 type_analyze D) cons
	       val (total,arg_types,return_type) = NilPrimUtil.get_type prim cons
	       val _ = do_args (D,arg_types,exps)
	     in  return_type
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
	     in return
	     end
	   
	 | Raise_e (exp,con) => 
	     let
	       val _ = type_analyze (D,con)
	       val exn_con = exp_valid (D,exp)
	       val _ = 
		 (is_exn_con (con_head_normalize (D,exn_con))) orelse
		 (e_error(D,exp,"Non exception raised - Ill formed expression") handle e => raise e)
	     in con
	     end
	 | Handle_e {body,bound,handler,result_type} => 
	     let
	       val _ = exp_analyze(D,body,result_type)
	       val D = insert_con(D,bound,Prim_c(Exn_c,[]))
	       val _ = exp_analyze(D,handler,result_type)
	       val _ = type_analyze (D,result_type)
	     in result_type
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
	  val res = ((bnd_valid''(bnd,state))
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
	  val _ = type_analyze(D,con)
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
	  val kind = kind_valid (D,kind)
	  val D = insert_stdkind(D,var,kind)
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

	  val (D,_,_) = bnds_valid(D,bnds)

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
      val module_valid = subtimer ("Module_valid", module_valid)

      val con_equiv = fn (D,c1,c2,k) => subtimer("Tchk:Top:con_equiv",con_equiv) (D,c1,c2,k,false)
      and sub_con = fn (D,c1,c2,k) => con_equiv (D,c1,c2,k,true)

end
