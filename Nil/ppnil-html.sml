(*$import Nil Ppprim Formatter String PPNIL *)
(* Nil pretty-printer. *)

structure PpnilHtml :> PPNIL =
  struct

    open Nil Formatter
    open Util Name Prim Ppprim

    val error = fn s => error "ppnil-html.sml" s
    val elide_prim = ref false
    val elide_bnd = ref false

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
    fun pp_list'' doer objs = pp_list doer objs ("", ",","", false)
    fun pp_list''' doer objs = pp_list doer objs ("", ".", "", false)

    fun pp_var v = 
	let val s = var2string v
            val n = String.size s
        in String0 n ("<A HREF=\"#" ^ s ^ "\">" ^ s ^ "</A>")
        end
    fun pp_bound_var v = 
	let val s = var2string v
            val n = String.size s
        in String0 n ("<A NAME=\"" ^ s ^ "\">" ^ s ^ "</A>")
        end

    fun pp_label l = String(label2string l)
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
    fun pp_effect Partial = String " -P-> "
      | pp_effect Total = String " -T-> "


    fun wrapper pp out obj = 
      let 
        val old_bailout = ! Formatter.Bailout
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (Formatter.Bailout := false;
          TextIO.output (out, "<HTML><BODY><PRE>\n");
          output_fmt (fmtstream,fmt); 
          TextIO.output (out, "</PRE></BODY></HTML>\n");
          Formatter.Bailout := old_bailout;
	  close_fmt fmtstream;
	  fmt)
      end


    (* is it basic with respect to printing *)
    fun is_base_con con = 
      (case con of
	 (Prim_c _) => true
       | _ => false)

    fun pp_labvar (l,v) = [pp_label l, String " > ", pp_bound_var v]
    fun pp_kind kind =
	    (case kind of
		 Type_k => String "TYPE"
	       | Record_k lvk_seq => (pp_list (fn (lv,k) => Hbox(pp_labvar lv
								   @ [String " :: ",
								      pp_kind k]))
				      (Sequence.toList lvk_seq) ("{", ",","}", true))
	       | Arrow_k (openness,ks,k) => 
		     HOVbox[String "Arrow_k(",
			    pp_openness openness,
			    String "; ",
			    pp_list (fn (v,k) => Hbox[pp_bound_var v, String " : ", 
						      pp_kind k])
			    ks ("(", ",",")", false),
			    String " => ",
			    pp_kind k,
			    String ")"]
	       | SingleType_k c => (pp_region "ST(" ")" [pp_con c])
	       | Single_k c => (pp_region "S(" ")" [pp_con c]))


    and pp_conbnd (Con_cb(v,c)) : format = Hbox[pp_bound_var v, String " = ", pp_con c]
      | pp_conbnd (Open_cb(v,vklist,c,k)) = 
	HOVbox[pp_bound_var v, String " = ", Break,
	       HOVbox[String "FUN_C",
		      (pp_list' (fn (v,k) => Hbox[pp_bound_var v, String " :: ", pp_kind k])
		       vklist),
		      Break0 0 5,
		      String " : ", pp_kind k, String " = ",
		      Break0 0 5,
		      pp_con c]]
      | pp_conbnd (Code_cb(v,vklist,c,k)) = 
	HOVbox[pp_bound_var v, String " =Code= ", Break,
	       (pp_list' (fn (v,k) => Hbox[pp_bound_var v, String " :: ", pp_kind k])
		vklist),
	       Break0 0 5,
	       String " :: ",
	       pp_kind k,
	       String " = ",
	       Break0 0 5,
	       pp_con c]


    and pp_labcon (l,c) = Hbox [pp_label l, String " : ", pp_con c]
    and pp_labvarcon (l,v,c) = Hbox[pp_label l, String " > ", pp_bound_var v, String " : ", pp_con c]
	
    and pp_con arg_con : format = 
      (case arg_con of
	   Var_c v => pp_var v
         | Prim_c (Record_c (labs,NONE), conlist) => 
              pp_list pp_labcon (Listops.zip labs conlist) ("{",",","}", false)
         | Prim_c (Record_c (labs,SOME vars), conlist) =>
	       if (length labs = length vars) then
		   pp_list pp_labvarcon (Listops.zip3 labs vars conlist) ("{",",","}", false)
               else
		   HOVbox[String "DEP_RECORD[", Break,
			  HOVbox[pp_list pp_label labs ("",",","", false),
				 String " :LENGTH_MISMATCH: ",
				 pp_list pp_bound_var vars ("",",","", false)],
			  String "]",
			  pp_list' pp_con conlist]
         | Prim_c (primcon, nil) => HOVbox[pp_primcon primcon]
	 | Prim_c (primcon, conlist) => HOVbox[pp_primcon primcon,
					       pp_list' pp_con conlist]
	 | Crecord_c lc_list => (pp_list (fn (l,c) => HOVbox[pp_label l, String " = ", pp_con c])
				  lc_list ("{", ",","}", false))
	 | Proj_c (c,l) => HOVbox[pp_con c, String ".", pp_label l]
	 | Typeof_c e => HOVbox[String "TYPEOF_C(", pp_exp e, String ")"]
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
	 | Closure_c (c1,c2) => HOVbox[String "CLOSURE_C(", pp_con c1, String ",", 
				       pp_con c2, String ")"]
	 | Typecase_c {arg, arms, default, kind} =>
	       let fun pp_arm(pc,vklist,c) = HOVbox[pp_primcon pc, String " => ",
						      String "FUN_C",
						      (pp_list' (fn (v,k) => Hbox[pp_bound_var v,pp_kind k])
						       vklist),
						      Break0 0 5,
						      pp_con c]
	       in HOVbox[String "TYPECASE_C(", pp_con arg, Break0 0 5,
			 pp_kind kind, Break0 0 5,
			 pp_list pp_arm arms ("","","",true), Break0 0 5,
			 String "DEFAULT: ",
			 pp_con default]
	       end
	 | App_c (con,conlist) => HOVbox[String "APP_C(",
(*
					       pp_arrow arrow,
					       String ",",
*)
					       pp_con con,
					       String ";",
					       pp_list'' pp_con conlist,
					       String ")"]
	 | Mu_c (flag,vcset) => HOVbox[if flag then String "MU_C(" else String "MU_C_NR(",
					   (pp_list' (fn (v,c) => HOVbox[pp_bound_var v, String "=", pp_con c])
					    (Sequence.toList vcset)),
					   String ")"]
(*	 | Listcase_c {arg,arms,default} =>HOVbox[String "LISTCASE_C ",
						  pp_con arg,
						  Break0 0 5,
						  (pp_list (fn (lc,cf) => Hbox[pp_listcon lc,
									       pp_confun cf])
						   arms ("","","", true)),
						  (case default of 
						       NONE => String "NONE"
						     | SOME con => Hbox[String "SOME ",
									   pp_con con])]
	 | All_c confun => HOVbox[String "ALL_C(", pp_confun confun, String ")"] 
*)
	 | AllArrow_c confun => pp_confun confun
	 | ExternArrow_c(cons,c) => pp_region "ExternArrow(" ")"
	                            [pp_list pp_con cons ("",",","",false),
				     String " --> ",
				     pp_con c]
	 | Annotate_c (Annotation.TYPECHECKED kind,con) => HOVbox[String "ANNOTE_C(Typechecked: ", 
						       pp_kind kind, 
						       String ",", 
						       pp_con con, String ")"]
	 | Annotate_c (annot,con) => HOVbox[String "ANNOTE_C(", String "annot not done", 
					    String ",", 
					    pp_con con, String ")"])

(*
    and pp_listcon (Nil_c k) = Hbox[String "NIL_C(", pp_kind k, String ")"]
      | pp_listcon Cons_c = String "CONS_C"
*)
	
    and pp_primcon (Int_c intsize) = Hbox[String "INT", pp_is' intsize]
      | pp_primcon (Float_c fs) = Hbox[String "FLOAT", pp_fs' fs]
      | pp_primcon (BoxFloat_c fs) = Hbox[String "BOXFLOAT", pp_fs' fs]
      | pp_primcon Array_c = String "ARRAY"
      | pp_primcon Vector_c = String "VECTOR"
      | pp_primcon Ref_c = error "no ref_c soon!!"
      | pp_primcon Exn_c = String "EXN"
      | pp_primcon Exntag_c = String "EXNTAG"
       | pp_primcon (Sum_c {known = opt,tagcount,totalcount}) = 
	String ("SUM" ^ (case opt of NONE => "" | SOME i => "_" ^ (TilWord32.toDecimalString i)) ^
		"(" ^ (TilWord32.toDecimalString tagcount) ^ ","
		^ (TilWord32.toDecimalString totalcount) ^ ")")
      | pp_primcon (Vararg_c (oness,e)) = Hbox[String "VARARG[", pp_openness oness, pp_effect e, String "]"]

    and pp_confun (openness,effect,vklist,vlistopt,clist,numfloats,con) = 
	pp_region "AllArrow(" ")"
	[HVbox[pp_openness openness,
	       String "; ",
	       (pp_list'' (fn (v,k) => Hbox[pp_bound_var v, String " :: ", pp_kind k]) vklist),
	       String "; ", Break0 0 5,
	       (case vlistopt of
		   SOME vars => 
		       if (length vars = length clist)
			   then (pp_list'' (fn (v,c) => Hbox[pp_bound_var v,String " :: ", 
							    pp_con c]) 
				 (Listops.zip vars clist))
		       else HOVbox[pp_list pp_bound_var vars ("",",","", false),
				   String " :LENGTH_MISMATCH: ",
				   pp_list pp_con clist ("",",","", false)]
		  | NONE => (pp_list'' pp_con clist)),
	       String "; ", String (TilWord32.toDecimalString numfloats),
               pp_effect effect, Break0 0 5,
	       pp_con con]]

    and pp_nilprimop nilprimop = 
	String (case nilprimop of
		    record labels => "record"
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
		     fun pp_rbnd (label, exp) = HOVbox[pp_label label, String "=",pp_exp exp]
		 in
		     HOVbox
		     [if (length labels = length exps)
			   then pp_list pp_rbnd (Listops.zip labels exps) 
			       ("{",",","}",false)
		       else HOVbox[String "record", pp_list pp_label labels ("",",","", false),
				   String " :LENGTH_MISMATCH: ",
				   pp_list pp_exp exps ("",",","", false)]]
		 end
           | Prim_e (NilPrimOp (select label), _, [exp]) =>
                Hbox[pp_exp exp, String ".", pp_label label]
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
		  [pp_exp efun, String ", ", 
		   pp_list pp_exp exps ("",",","",false)])
	   | App_e (openness,efun,cons,exps,fexps) => (pp_region ("App_" ^ (openness2s openness) ^ "(") ")" 
						       [pp_exp efun, String "; ", 
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

	   | Raise_e (e,c) => pp_region "RAISE(" ")" [pp_exp e, String ",", pp_con c]
	   | Handle_e (body,v,handler) => 
		 Vbox[HOVbox[String "HANDLE ",
			     pp_exp body],
		      Break0 0 0,
		      HOVbox[String "WITH ", pp_bound_var v, 
			     String ": EXN . ",
			     pp_exp handler]]
	   | Switch_e sw => pp_switch sw)

	 
    and pp_function (Function(effect,recursive,vklist,dependent,vclist,vflist,exp,c)) = 
	HOVbox[String(case recursive of Leaf => "/Leaf\\" | Nonleaf => "/\\"),
	       String(if dependent then "DEPENDENT" else ""),
	       (pp_list' (fn (v,k) => Hbox[pp_bound_var v, String "::",pp_kind k])
		vklist),
	       (pp_list' (fn (v,c) => Hbox[pp_bound_var v, String ":", pp_con c])
		vclist),
	       (pp_list' (fn v => Hbox[pp_bound_var v,String ":Float"])
		vflist),
	       String ":",
	       pp_con c,
	       String "=",
	       Break0 0 5,
	       pp_exp exp]

    and pp_switch sw =
	let fun pp_default NONE = String "NODEFAULT"
	      | pp_default (SOME e) = Hbox[String "DEFAULT = ", pp_exp e]
	in
	    case sw of
		Intsw_e {arg,size,arms,default} => 
		    HOVbox[String "INT_SWITCH(", 
			   pp_exp arg, String ": ",
			   pp_is' size, String ", ",
			   Break0 0 5,
			   (pp_list (fn (w,e) => Hbox[pp_word w, String ": ", pp_exp e])
			      arms ("","","", true)),
			   Break0 0 5,
			   pp_default default,
			   String ")"]
	      | Sumsw_e {arg,sumtype,bound,arms,default} => 
		    HOVbox[String "SUM_SWITCH(", 
			   pp_exp arg, String ": ",
			   pp_con sumtype, String ", ",
			   Break0 0 5,
			   pp_bound_var bound, String ", ",  Break0 0 5,
			   (pp_list (fn (w,e) => Hbox[pp_word w, String ": ", pp_exp e])
			      arms ("","","", true)),
			   Break0 0 5,
			   pp_default default,
			   String ")"]
	      | Exncase_e {arg,bound,arms,default} => 
		    HOVbox[String "EXN_SWITCH(", 
			   pp_exp arg, String ": EXN, ",
			   Break0 0 5,
			   pp_bound_var bound, String ", ",  Break0 0 5,
			   (pp_list (fn (t,e) => Hbox[pp_exp t, String ": ", pp_exp e])
			      arms ("","","", true)),
			   Break0 0 5,
			   pp_default default,
			   String ")"]
	      | Typecase_e sw => error "can't print typecase"

	end

    and pp_trace TraceUnknown = String "Unknown"
      | pp_trace (TraceCompute v) = Hbox[String "Compute(", pp_var v, String ")"]
      | pp_trace (TraceKnown (TraceInfo.Compute(v,labs))) = 
	Hbox[String "Compute(", pp_var v, 
	     String ".", pp_list''' pp_label labs, String ")"]
      | pp_trace (TraceKnown (TraceInfo.Notrace_Int)) = String "Known_Int"
      | pp_trace (TraceKnown (TraceInfo.Notrace_Real)) = String "Known_Real"
      | pp_trace (TraceKnown _) = String "Known"

    and pp_bnd bnd =
	let fun help x y = (x,y)
	  in (case bnd of
	        Exp_b (v,t,e) => HOVbox[pp_bound_var v, String " : ", pp_trace t,
					String " = ", pp_exp e]
	      | Con_b (Runtime,cb) => pp_conbnd cb
	      | Con_b (Compiletime,cb) => HOVbox[String "COMPILE_TIME ", Break, pp_conbnd cb]
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
				       pp_bound_var v, 
				       HOVbox(if (!elide_bnd) then [] else [String " : ", pp_con tipe]),
				       String " = ",
				       String "(",
				       pp_var code, String ",",
				       pp_con cenv, String ",",
				       pp_exp venv, String ")"])
			vcelist ("","","",true)
		    end)
	end

    and pp_fix is_code (v,Function(effect,recursive,vklist,dep,vclist,vflist,e,c)) : format = 
	let val vkformats = (pp_list_flat (fn (v,k) => HOVbox[pp_bound_var v, String " :: ", Break, pp_kind k]) 
			     vklist ("",",","",false))
	    val vcformats = (pp_list_flat (fn (v,c) => HOVbox[pp_bound_var v, String " : ", Break, pp_con c]) 
			     vclist ("",",","",false))
	    val vfformats = (pp_list_flat (fn v => HOVbox[pp_bound_var v, String " : Float"])
			     vflist ("",",","",false))
	    val temp = (length vclist) + (length vflist)
	in
	    Vbox([String (case (is_code,recursive) of
			    (false,Arbitrary) => "/\\ "
			  | (false,NonRecursive) => "/NORECUR\\"
			  | (false,Leaf) => "/LEAF\\"
			  | (true, Arbitrary) => "/CODE\\"
			  | (true, NonRecursive) => "/NORECUR-CODE\\"
			  | (true, Leaf) => "/LEAF-CODE\\"),
		  if dep then String "DEP" else String "",
		    pp_bound_var v, Break,
		    pp_region "(" ")"
		    [HVbox(vkformats @
		     (if (temp > 0) then [String " ;; ", Break0 0 8] else [String " ;; "]) @
			  vcformats @ [String " ;; "] @ vfformats)],
		    Break0 0 5,
                    pp_effect effect,
		    pp_con c, String " =", Break,
		    pp_exp e])
	end

    fun pp_bnds bnds = pp_list pp_bnd bnds ("[",",","]",true)

    fun pp_module (MODULE{bnds,imports,exports}) = 
	let 
	    fun pp_importentry (ImportValue (l,v,c)) = 
		Hbox[pp_label l, String " > ", pp_bound_var v, String " : ", pp_con c]
	      |  pp_importentry (ImportType (l,v,k)) = 
		Hbox[pp_label l, String " > ", pp_bound_var v, String " :: ", pp_kind k]
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
    val pp_conbnd = help' pp_conbnd
    val pp_bnds = help' pp_bnds
    val pp_exp = help' pp_exp
    fun pp_module {module, name:string, pass:string, header:string} = 
        let val st = TextIO.openOut (name ^ "." ^ pass ^ ".html")
        in (TextIO.output(st, header);
            TextIO.output(st, "\n\n");
            wrapper pp_module' st module; 
            TextIO.closeOut st;
            ())
        end

  end
