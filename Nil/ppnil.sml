(*$import Nil Ppprim Formatter PPNIL *)
(* Nil pretty-printer. *)

structure Ppnil	:> PPNIL =
  struct

    open Nil Formatter
    open Util Name Prim Ppprim

    val error = fn s => error "ppnil.sml" s
    val elide_prim = ref false
    val elide_record = ref false

    fun pp_region s1 s2 fmt = HOVbox((String s1) :: (fmt @ [String s2]))
    fun separate [] sep = []
      | separate [a] sep = [a]
      | separate (a::rest) sep = a :: sep :: (separate rest sep)
    fun pp_list_flat doer objs (left,sep,right,break) = 
      let 
	fun loop [] = [String right]
	  | loop [a] = [doer a, String right]
	  | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	val fmts = (String left) :: (loop objs)
      in fmts
      end
    fun pp_list doer objs (pref as (left,_,_,break)) = 
	let val fmts = pp_list_flat doer objs pref
	in (if break then Vbox0 else HOVbox0 1) (size left) 1 fmts
	end
    val pp_listid = pp_list (fn x => x)
    fun pp_list' doer objs = pp_list doer objs ("(", ",",")", false)
    fun pp_list'' doer objs = pp_list doer objs ("", ".","", false)

    fun pp_var v = String(var2string v)
    fun pp_label l = String(label2name l)
    fun pp_tag n = String(tag2string n)
    val pp_word = String o Word32.toString


(*
    fun pp_arrow CLOSURE = String "->"
      | pp_arrow CODE = String "-CODE>"
*)
    fun openness2s Open =  "Open"
      | openness2s Closure = "Closure"
      | openness2s Code = "Code"
    fun pp_openness openness = String(openness2s openness)
    fun pp_effect Partial = String "->"
      | pp_effect Total = String "=>"


(*    local
      (* these 3 functions copied from ilutil.sml; no recursive modules... *)
      fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
      fun generate_tuple_label (i : int) = symbol2label(generate_tuple_symbol i)
      fun loop [] _ = true
	| loop (l::rest) cur = eq_label(l,generate_tuple_label cur) andalso loop rest (cur+1) 
    in
      fun rdecs_is_tuple rdecs = loop(map (fn (RDEC(l,_)) => l) rdecs) 1
      fun rbnds_is_tuple rbnds = loop(map (fn (RBND(l,_)) => l) rbnds) 1
    end
*)
    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  fmt)
      end


    (* is it basic with respect to printing *)
    fun is_base_con con = 
      (case con of
	 (Prim_c _) => true
       | _ => false)

    fun con2path c = 
	let fun loop (Var_c v) labs = SOME(v,labs)
  	      | loop (Proj_c(c,l)) labs = loop c (l::labs)
	      | loop _ _ = NONE
 	in  loop c []
	end

    fun pp_path (v,[]) = pp_var v
      | pp_path (v,labs) = Hbox[pp_var v, String ".", pp_list'' pp_label labs]

    fun pp_labvar (l,v) = [pp_label l, String " > ", pp_var v]
    fun pp_kind kind =
	    (case kind of
		 Type_k => String "TYPE"
	       | Record_k lvk_seq => (pp_list (fn (lv,k) => HOVbox(pp_labvar lv
								   @ [String " : ", Break,
								      pp_kind k]))
				      (Sequence.toList lvk_seq) ("REC_K{", ",","}", true))
	       | Arrow_k (openness,ks,k) => 
		     HOVbox[String "Arrow_k(",
			    pp_openness openness,
			    String "; ",
			    pp_list (fn (v,k) => Hbox[pp_var v, String " : ", 
						      pp_kind k])
			    ks ("", ",","", false),
			    String "; ",
			    pp_kind k,
			    String ")"]
	       | SingleType_k c => (pp_region "SINGLE_TYPE(" ")" [pp_con c])
	       | Single_k c => (pp_region "SINGLE(" ")" [pp_con c]))


    and pp_conbnd (Con_cb(v,c)) : format = Hbox[pp_var v, String " = ", Break0 0 5, pp_con c]
      | pp_conbnd (Open_cb(v,vklist,c)) = 
	HOVbox[pp_var v, String " = ", Break,
	       HOVbox[String "FUN",
		      (pp_list' (fn (v,k) => Hbox[pp_var v, String " :: ", Break, pp_kind k])
		       vklist),
		      Break0 0 3,
		      String " = ",
		      pp_con c]]
      | pp_conbnd (Code_cb(v,vklist,c)) = 
	HOVbox[pp_var v, String " = ", Break0 0 5,
	       String "CODE", Break0 0 2,
	       (pp_list' (fn (v,k) => Hbox[pp_var v, String " :: ", pp_kind k])
		vklist),
	       Break0 0 3,
	       String " = ",
	       pp_con c]

	
    and pp_con arg_con : format = 
      (case arg_con of
	   Var_c v => pp_var v
         | Prim_c(Record_c ([],NONE),[]) => HOVbox[String "UNIT"]
         | Prim_c (primcon, nil) => HOVbox[pp_primcon primcon]
	 | Prim_c (primcon, conlist) => HOVbox[pp_primcon primcon,
					       pp_list' pp_con conlist]
	 | Crecord_c lc_list => (pp_list (fn (l,c) => HOVbox[pp_label l, String " = ", pp_con c])
				  lc_list ("CREC{", ",","}", false))
	 | Proj_c (c,l) => (case con2path arg_con of
				SOME path => pp_path path
			      | NONE => HOVbox[String "PROJ(", pp_con c, String ",", 
						pp_label l, String ")"])
	 | Typeof_c e => HOVbox[String "TYPEOF(", pp_exp e, String ")"]
	 | Let_c (letsort,cbnds,cbody) =>
		   Vbox0 0 1 [String (case letsort of
					  Sequential => "LET  "
					| Parallel => "LETP "),
			      Vbox(separate (map pp_conbnd cbnds) (Break0 0 0)),
			      Break,
			      String "IN   ",
			      pp_con cbody,
			      Break,
			      String "END"]
	 | Closure_c (c1,c2) => HOVbox[String "CLOSURE(", pp_con c1, String ",", 
				       pp_con c2, String ")"]
	 | Typecase_c {arg, arms, default, kind} =>
	       let fun pp_arm(pc,vklist,c) = HOVbox[pp_primcon pc, String " => ",
						      String "FUN",
						      (pp_list' (fn (v,k) => Hbox[pp_var v,pp_kind k])
						       vklist),
						      Break0 0 5,
						      pp_con c]
	       in HOVbox[String "TYPECASE(", pp_con arg, Break0 0 5,
			 pp_kind kind, Break0 0 5,
			 pp_list pp_arm arms ("","","",true), Break0 0 5,
			 String "DEFAULT: ",
			 pp_con default]
	       end
	 | App_c (con,conlist) => HOVbox[String "APP(",
					 pp_con con, String ",", Break0 0 2,
					 pp_list' pp_con conlist,
					 String ")"]
	 | Mu_c (flag,vcset) => HOVbox[if flag then String "MU(" else String "MU_NR(",
					   (pp_list' (fn (v,c) => HOVbox[pp_var v, String "=", pp_con c])
					    (Sequence.toList vcset)),
					   String ")"]
	 | AllArrow_c confun => pp_confun confun
	 | ExternArrow_c(cons,c) => pp_region "EXTERNARROW(" ")"
	                            [pp_list pp_con cons ("",",","",false),
				     String " --> ",
				     pp_con c]
	 | Annotate_c (Annotation.TYPECHECKED kind,con) => HOVbox[String "ANNOTE(Typechecked: ", 
						       pp_kind kind, 
						       String ",", 
						       pp_con con, String ")"]
	 | Annotate_c (annot,con) => HOVbox[String "ANNOTE(", String "annot not done", 
					    String ",", 
					    pp_con con, String ")"])


	
    and pp_primcon (Int_c intsize) = Hbox[String "INT", pp_is' intsize]
      | pp_primcon (Float_c fs) = Hbox[String "FLOAT", pp_fs' fs]
      | pp_primcon (BoxFloat_c fs) = Hbox[String "BOXFLOAT", pp_fs' fs]
      | pp_primcon Array_c = String "ARRAY"
      | pp_primcon Vector_c = String "VECTOR"
      | pp_primcon Loc_c = String "LOC"
      | pp_primcon Exn_c = String "EXN"
      | pp_primcon Exntag_c = String "EXNTAG"
      | pp_primcon (Record_c (labs,NONE)) = HOVbox[String "RECORD[", Break0 0 3,
						   pp_list pp_label labs ("",",","", false),
						   String "]"]
      | pp_primcon (Record_c (labs,SOME vars)) = 
	HOVbox[String "DEP_RECORD[", Break,
	       if (length labs = length vars)
		   then pp_list (HOVbox o pp_labvar) (Listops.zip labs vars)
		       ("",",","", false)
	       else HOVbox[pp_list pp_label labs ("",",","", false),
			   String " :LENGTH_MISMATCH: ",
			   pp_list pp_var vars ("",",","", false)],
	       String "]"]
      | pp_primcon (Sum_c {known = opt,tagcount,totalcount}) = 
	String ("SUM" ^ (case opt of NONE => "" | SOME i => "_" ^ (TilWord32.toDecimalString i)) ^
		"(" ^ (TilWord32.toDecimalString tagcount) ^ ","
		^ (TilWord32.toDecimalString totalcount) ^ ")")
      | pp_primcon (Vararg_c (oness,e)) = Hbox[String "VARARG[", pp_openness oness, pp_effect e, String "]"]

    and pp_confun {openness,effect,isDependent,tFormals,eFormals,fFormals,body_type} = 
	HOVbox[String "ALLARROW(",
		pp_openness openness,
	        String "; ",
	       (case effect of
		    Total => String "TOTAL; "
		  | Partial => String "PARTIAL; "),
	       (pp_list' (fn (v,k) => Hbox[pp_var v,String " :: ", pp_kind k]) tFormals),
	       String "; ", Break0 0 3,
	       (pp_list' (fn (vopt,c) =>
			  (case vopt of
			       NONE => pp_con c
			     | SOME v => if isDependent
					     then Hbox[pp_var v,String " :: ", pp_con c]
					 else error "non-dependent but has var")) eFormals),
	       String "; ", String (TilWord32.toDecimalString fFormals),
	       String "; ", Break0 0 3,
	       pp_con body_type,
	       String ")"]

    and pp_nilprimop (select label) = Hbox[String "select[", pp_label label, String "]"]
      | pp_nilprimop nilprimop = 
	String (case nilprimop of
		    record labels => "record"
		  | partialRecord (labels,missField) => "partialRecord_" ^ (Int.toString missField)
		  | select label => raise (BUG "pp_nilprimop: control should not reach here")
		  | inject w => "inject_dyn" ^ (TilWord32.toDecimalString w)
		  | inject_nonrecord w => "inject_nonrec_" ^ (TilWord32.toDecimalString w)
		  | inject_record w => "inject_rec_" ^ (TilWord32.toDecimalString w)
		  | project_sum w => "project_sum_dyn" ^ (TilWord32.toDecimalString w)
		  | project_sum_nonrecord w => "project_sum_nonrec_" ^ (TilWord32.toDecimalString w)
		  | project_sum_record (w,field) => ("project_sum_rec_" ^ (TilWord32.toDecimalString w) ^
						 "[" ^ (Name.label2string field) ^ "]")
		  | roll => "roll"
		  | unroll  => "unroll"
		  | make_exntag => "make_exntag"
		  | inj_exn s => "inj_exn[" ^ s ^ "]"
		  | peq => "peq"
		  | make_vararg (openness,effect) => "make_vararg" 
		  | make_onearg (openness,effect) => "make_onearg"
		  | box_float Prim.F64 => "box_float_64"
		  | box_float Prim.F32 => "box_float_32"
		  | unbox_float Prim.F64 => "unbox_float_64"
		  | unbox_float Prim.F32 => "unbox_float_32")

    and pp_exp exp = 
	(case exp of
	     Var_e var => pp_var var
	   | Const_e v => Ppprim.pp_value' (fn (Const_e v) => SOME v | _ => NONE) pp_exp pp_con v
           | Prim_e (NilPrimOp (record labels), cons, exps) =>
                 let
		     fun pp_le (label, exp) = HOVbox[pp_label label, String ">",pp_exp exp]
		     fun pp_lce (label, con, exp) = HOVbox[pp_label label, 
							   String ": ", pp_con con,
							   String "> ", pp_exp exp]
		 in
		     HOVbox
		     [String "record", 
		      if (length labels = length exps andalso 
			  length labels = length cons)
			  then (if !elide_record
				    then pp_list pp_le (Listops.zip labels exps) 
					("(",",",")",false)
				else pp_list pp_lce (Listops.zip3 labels cons exps) 
				    ("(",",",")",false))
		       else HOVbox[pp_list pp_label labels ("",",","", false),
				   String " :LENGTH_MISMATCH: ",
				   pp_list pp_exp exps ("",",","", false)]]
		 end
	   | Prim_e (prim,cons,exps) => 
		 let val p = (case prim of
				  PrimOp p => pp_prim' p
				| NilPrimOp p => pp_nilprimop p)
		     val c = pp_list pp_con cons ("[",",","]",false)
		     val e = pp_list pp_exp exps ("(",",",")",false)
		 in HOVbox(if (!elide_prim) then [p,e] else [p,c,e])
		 end
	   | ExternApp_e (efun,exps) => 
		 (pp_region ("ExternApp_(") ")" 
		  [pp_exp efun, String ", ", Break, 
		   pp_list pp_exp exps ("",",","",false)])
	   | App_e (openness,efun,cons,exps,fexps) => (pp_region ("App_" ^ (openness2s openness) ^ "(") ")" 
						       [pp_exp efun, String "; ", Break, 
							pp_list pp_con cons ("",",","",false), String "; ",
							pp_list pp_exp exps ("",",","",false), String "; ",
							pp_list pp_exp fexps (" ",",","",false)])
	   | Let_e (letsort,bnds,e) => Vbox0 0 1 [String (case letsort of
							      Sequential => "Let  "
							    | Parallel => "LetP "),
						  Vbox(separate (map pp_bnd bnds) (Break0 0 0)),
						  Break,
						  String "In   ",
						  pp_exp e,
						  Break,
						  String "End"]

	   | Raise_e (e,c) => pp_region "Raise(" ")" [pp_exp e, String ",", pp_con c]
	   | Handle_e (body,v,handler) => 
		 Vbox[HOVbox[String "Handle ", Break0 0 5,
			     pp_exp body],
		      Break0 0 0,
		      HOVbox[String "With ", pp_var v, 
			     String ": EXN = ", Break0 0 5,
			     pp_exp handler]]
	   | Switch_e sw => pp_switch sw)

	 
    and pp_switch sw =
	let fun pp_default NONE = String "NODEFAULT"
	      | pp_default (SOME e) = Hbox[String "DEFAULT = ", pp_exp e]
	in
	    case sw of
		Intsw_e {arg,size,arms,default,result_type} => 
		    HOVbox[String "INT_SWITCH(", 
			   pp_exp arg, String ": ",
			   pp_is' size, String ", ",
			   Break0 0 5,
			   (pp_list (fn (w,e) => Hbox[pp_word w, String ": ", pp_exp e])
			      arms ("","","", true)),
			   Break0 0 5,
			   pp_default default,
			   String ")",String" : ",pp_con result_type]
	      | Sumsw_e {arg,sumtype,bound,arms,default,result_type} => 
		    HOVbox[String "SUM_SWITCH(", 
			   pp_exp arg, String ": ",
			   pp_con sumtype, String ", ",
			   Break0 0 5,
			   pp_var bound, String ", ",  Break0 0 5,
			   (pp_list (fn (w,tr,e) => Hbox[pp_word w, String ": ", pp_trace tr, Break0 0 2,
								    String ":: ", pp_exp e])
			      arms ("","","", true)),
			   Break0 0 5,
			   pp_default default,
			   String ")",String" : ",pp_con result_type]
	      | Exncase_e {arg,bound,arms,default,result_type} => 
		    HOVbox[String "EXN_SWITCH(", 
			   pp_exp arg, String ": EXN, ",
			   Break0 0 5,
			   pp_var bound, String ", ",  Break0 0 5,
			   (pp_list (fn (t,tr,e) => Hbox[pp_exp t, String ": ", pp_trace tr,
								   String ": ", pp_exp e])
			      arms ("","","", true)),
			   Break0 0 5,
			   pp_default default,
			   String ")",String" : ",pp_con result_type]
	      | Typecase_e sw => error "can't print typecase"

	end

    and pp_trace TraceUnknown = String "Unknown"
      | pp_trace (TraceCompute v) = Hbox[String "Compute(", pp_var v, String ")"]
      | pp_trace (TraceKnown (TraceInfo.Compute path)) =
		Hbox[String "Compute(", pp_path path, String ")"]
      | pp_trace (TraceKnown (TraceInfo.Trace)) = String "Known_Trace"
      | pp_trace (TraceKnown (TraceInfo.Unset)) = String "Known_Unset"
      | pp_trace (TraceKnown (TraceInfo.Notrace_Int)) = String "Known_Int"
      | pp_trace (TraceKnown (TraceInfo.Notrace_Code)) = String "Known_Code"
      | pp_trace (TraceKnown (TraceInfo.Notrace_Real)) = String "Known_Real"
      | pp_trace (TraceKnown (TraceInfo.Label)) = String "Known_Label"
      | pp_trace (TraceKnown (TraceInfo.Locative)) = String "Known_Locative"


    and pp_bnd bnd =
	let fun help x y = (x,y)
	  in (case bnd of
	        Exp_b (v,t,e) => HOVbox[pp_var v, String " : ", pp_trace t,
					String " = ", Break0 0 2, pp_exp e]
	      | Con_b (Runtime,cb) => pp_conbnd cb
	      | Con_b (Compiletime,cb) => HOVbox[String "STATIC ", Break0 0 2, pp_conbnd cb]
	      | Fixopen_b fixset => let val fixlist = Sequence.toList fixset
				    in Vbox(separate (map (pp_fix false) fixlist) Break)
				    end
	      | Fixcode_b fixset => let val fixlist = Sequence.toList fixset
				    in Vbox(separate (map (pp_fix true) fixlist) Break)
				    end
	      | Fixclosure_b (recur,vceset) => 
		    let val vcelist = Sequence.toList vceset
		    in pp_list (fn (v,{code,cenv,venv,tipe}) => 
				HOVbox[if recur then String "& " else String "",
				       pp_var v, 
				       String " : ", pp_con tipe,
				       String " = ", Break0 0 5,
				       String "(",
				       pp_var code, String ",", Break0 0 5,
				       pp_con cenv, String ",", Break0 0 5,
				       pp_exp venv, String ")"])
			vcelist ("","","",true)
		    end)
	end

    and pp_fix is_code (v,(Function{effect,recursive,isDependent,
				    tFormals, eFormals, fFormals, 
				    body, body_type})) : format = 
	let val vkformats = (pp_list_flat (fn (v,k) => HOVbox[pp_var v, String " :: ", Break0 0 2, 
									pp_kind k]) 
			     tFormals ("",",","",false))
	    val vcformats = (pp_list_flat (fn (v,tr,c) => 
					HOVbox[pp_var v,
						String " : ", Break0 0 2, pp_trace tr,
					   	String " : ", Break0 0 2, pp_con c])
			     eFormals ("",",","",false))
	    val vfformats = (pp_list_flat (fn v => HOVbox[pp_var v, String " : Float"])
			     fFormals ("",",","",false))
	in
	    Vbox([String (case (is_code,recursive) of
			    (false,Arbitrary) => "/\\ "
			  | (false,NonRecursive) => "/NORECUR\\"
			  | (false,Leaf) => "/LEAF\\"
			  | (true, Arbitrary) => "/CODE\\"
			  | (true, NonRecursive) => "/NORECUR-CODE\\"
			  | (true, Leaf) => "/LEAF-CODE\\"),
		  if isDependent then String "DEP" else String "",
		    pp_var v, Break0 0 2,
		    pp_region "(" ")"
		    [HOVbox(vkformats @
			   (if (null vkformats) 
				  then [String " ;; "] else 
				[String " ;; ", Break0 0 0]) @
			   vcformats @ 
			   (if (null vfformats)
				then [String " ;; "] 
			    else [String " ;; ", Break0 0 0] @ vfformats))],
		    Break0 0 0,
		    String (case effect of 
				Total => " -> " 
			      | Partial => " => "),
		    pp_con body_type, String " =", Break,
		    pp_exp body])
	end

    fun pp_bnds bnds = pp_list pp_bnd bnds ("[",",","]",true)

    fun pp_module (MODULE{bnds,imports,exports}) = 
	let 
	    fun pp_importentry (ImportValue (l,v,nt,c)) = 
		Hbox[pp_label l, String " > ", pp_var v, String " : ", 
		     pp_trace nt, String " : ", pp_con c]
	      |  pp_importentry (ImportType (l,v,k)) = 
		Hbox[pp_label l, String " = ", pp_var v, String " : ", pp_kind k]
	    fun pp_exportentry (ExportValue (l,v)) = 
		Hbox[pp_label l, String " = ", pp_var v]
	      |  pp_exportentry (ExportType (l,v)) = 
		Hbox[pp_label l, String " = ", pp_var v]
	in  Vbox0 0 1 [pp_bnds bnds,
		       Break,
		       String "IMPORTS:", Break,
		       pp_list pp_importentry imports ("","","",true), Break,
		       String "EXPORTS:", Break,
		       pp_list pp_exportentry exports ("","","",true), Break]
	end

    fun help pp = pp
    fun help' pp obj = (wrapper pp TextIO.stdOut obj; ())

    val pp_list' = pp_list
    val pp_var' = help pp_var
    val pp_label'  = help pp_label
    val pp_con' = help pp_con
    val pp_kind' = help pp_kind
    val pp_bnd' = help pp_bnd
    val pp_trace' = help pp_trace
    val pp_conbnd' = help pp_conbnd
    val pp_bnds' = help pp_bnds
    val pp_exp' = help pp_exp
    val pp_module' = help pp_module

    val pp_var = help' pp_var
    val pp_label  = help' pp_label
    val pp_con = help' pp_con
    val pp_kind = help' pp_kind
    fun pp_list doer data = help' (pp_list' doer data)
    val pp_bnd = help' pp_bnd
    val pp_trace = help' pp_trace
    val pp_conbnd = help' pp_conbnd
    val pp_bnds = help' pp_bnds
    val pp_exp = help' pp_exp
    val pp_module = 
         fn {module, name:string, pass:string, header:string} =>
            (print "PASS: "; print pass; print "\n";
             print header; print "\n\n";
             help' pp_module module)

  end
