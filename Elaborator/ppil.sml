(*$import Ppprim FORMATTER Bool PPIL *)
(* Il pretty-printer. *)
structure Ppil :> PPIL =
struct

    structure Formatter = Formatter
    datatype display = VAR_ONLY | VALUE_ONLY | VAR_VALUE
    val convar_display = ref VALUE_ONLY

    open Util Listops Name 
    open Il Formatter
    open Prim Ppprim Tyvar

    val error = fn s => error "ppil.sml" s


    fun pp_region s1 s2 fmt = HOVbox((String s1) :: (fmt @ [String s2]))
    fun separate [] sep = []
      | separate [a] sep = [a]
      | separate (a::rest) sep = a :: sep :: (separate rest sep)
    fun pp_list doer objs (left,sep,right,break) = 
      let 
	fun loop [] = [String right]
	  | loop [a] = [doer a, String right]
	  | loop (a::rest) = (doer a) :: (String sep) :: Break :: (loop rest)
	val fmts = (String left) :: (loop objs)
      in (if break then Vbox0 else HOVbox0 1) (size left) 1 fmts
      end

    val pp_listid = pp_list (fn x => x)

    fun pp_var v = String(var2string v)
    fun pp_label l = String(label2string l)
    fun pp_commalist pobj objlist = pp_list pobj objlist ("(",",",")",false)
    fun pp_semicolonlist pobj objlist = pp_list pobj objlist ("(",";",")",true)
    fun pp_pathlist pobj objlist = pp_list pobj objlist ("",".","",false)


    fun pp_fixity fixity = 
      (case fixity of
	 Fixity.NONfix => String "NONfix"
       | Fixity.INfix (a,b) => String ("INfix" ^ (if (a mod 2 = 0) then "l" else "r")
				       ^ (Int.toString (a div 2))))


    (* -----------  these functions copied from ilutil.sml; no recursive modules... *)
    local fun loop (MOD_VAR v) acc = SOME(PATH(v,acc))
	    | loop (MOD_PROJECT (m,l)) acc = loop m (l::acc)
	    | loop m _ = NONE
    in    
	fun mod2path (m : mod) = loop m []
	fun exp2path (e : exp) = 
	    (case e of
		 VAR v => SOME(PATH (v,[]))
	       | MODULE_PROJECT (m,l) => mod2path (MOD_PROJECT(m,l))
	       | _ => NONE)
	fun con2path (c : con) =
	    (case c of
		CON_VAR v => SOME(PATH(v,[]))
	      | CON_MODULE_PROJECT (m,l) => mod2path(MOD_PROJECT(m,l))
	      | _ => NONE)
    end
    local
	fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
	fun generate_tuple_label (i : int) = symbol_label(generate_tuple_symbol i)
	fun loop [] _ = true
	  | loop ((l,_)::rest) cur = eq_label(l,generate_tuple_label cur) andalso loop rest (cur+1) 
    in
	fun rdecs_is_tuple rdecs = loop rdecs 1
	fun rbnds_is_tuple rbnds = loop rbnds 1
    end
    (* -----------  the above functions copied from ilutil.sml; no recursive modules... *)



    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  fmt)
      end
    fun pp_arrow PARTIAL = String "->"
      | pp_arrow TOTAL = String "=>"
    fun complete2string oneshot = 
	(case (oneshot_deref oneshot) of
	     (SOME TOTAL) => "=>"
	   | (SOME PARTIAL) => "->"
	   | NONE => "?>")

    val member = fn (a,lst) =>
      let val _ = (print "member: lst of length "; print (Int.toString (length lst)); print "\n")
	val res = member(a,lst)
	val _ = (print "returning: "; print (Bool.toString res); print "\n")
      in res
      end

    (* is it basic with respect to printing *)
    fun is_base_con con = 
      (case con of
	 (CON_INT _) => true
       | CON_FLOAT _ => true
       | CON_UINT _ => true
       | CON_ANY => true
       | CON_VAR _ => true
       | (CON_TYVAR tyvar) => (case tyvar_deref tyvar of
				       NONE => true
				     | SOME c => is_base_con c)
       | _ => false)


    fun pp_recordcon (seen : (context,con) tyvar list) (isflex,rdecs) : format = 
	let val is_tuple = rdecs_is_tuple rdecs
	    val format = (case (is_tuple,isflex) of
			      (true,false) => ("{", " *","}", true)
			    | (true,true) => ("{?", " *","?}", true)
			    | (false,false) => ("{", ",","}", true)
			    | (false,true) => ("{?", ",","?}", true))
	    val doer = if is_tuple
			   then (fn (l,c) => pp_con seen c)
		       else
			   (fn (l,c) => Hbox[pp_label l,
					     String " = ",
					     pp_con seen c])
	in pp_list doer rdecs format
	end
    
    and pp_con (seen : (context,con) tyvar list) (arg_con : con) : format = 
      (case arg_con of
	 CON_OVAR ocon => pp_con seen (CON_TYVAR (ocon_deref ocon))
       | CON_VAR var => pp_var var
       | CON_TYVAR tyvar =>
	     let val varname = if (tyvar_isconstrained tyvar)
				   then ("C" ^ tyvar2string tyvar)
			       else tyvar2string tyvar
		 val stamp = Int.toString(stamp2int (tyvar_stamp tyvar))
		 val varname = varname ^ "_" ^ stamp
	     in (case (tyvar_deref tyvar) of
		     NONE => Hbox[String varname]
		   | (SOME con) => (if (member_eq(eq_tyvar,tyvar,seen)) then
					(print "tyvar is "; print varname; print "\n";
					 print "seen_refs("; print (Int.toString (length seen));
					 print ")var is: ";
					 map (fn tv => print (tyvar2string tv)) seen;
					 print "\n\n";
					 error "circular";
					 Hbox0 0 [String ("KNOT_" ^ varname)])
				    else
					let val seen = tyvar :: seen
					in (case (!convar_display) of
						VALUE_ONLY => pp_con seen con
					      | VAR_ONLY => Hbox0 0 [String varname]
					      | VAR_VALUE => (pp_region "(" ")"
							      [String varname,
							       String "==", pp_con seen con]))
					end))
	     end
       | CON_INT is    => Hbox[String "INT", pp_is' is]
       | CON_UINT is   => Hbox[String "UINT", pp_is' is]
       | CON_FLOAT fs  => Hbox[String "FLOAT", pp_fs' fs]
       | CON_VECTOR c => Hbox[pp_con seen c, Break, String "VECTOR"]
       | CON_ARRAY c => Hbox[pp_con seen c, Break, String "ARRAY"]
       | CON_ANY     => String "ANY"
       | CON_REF c   => pp_region "REF("  ")" [pp_con seen c]
       | CON_TAG c  => pp_region "NAME(" ")" [pp_con seen c]
       | CON_ARROW (cons,c2,closed,comp) => 
	     HOVbox [String "(",	
		     (case cons of
			  [c1] => pp_con seen c1
			| _ => pp_list (pp_con seen) cons ("[", ",","]", false)),
			  String " ",
			  String ((if closed then "CODE" else "") ^ 
				  (complete2string comp)),
			  Break,
			  pp_con seen c2, String ")"]
       | CON_APP (c1,c2) => pp_region "CON_APP(" ")"
	                              [pp_con seen c1,
				       String ",",
				       Break,
				       pp_con seen c2]
       | CON_MU c => pp_region "CON_MU(" ")" [pp_con seen c]
       | CON_RECORD [] => String "UNIT"
       | CON_FLEXRECORD (ref(FLEXINFO(_,true,[]))) => String "UNIT"
       | CON_FLEXRECORD (ref(FLEXINFO(_,false,[]))) => String "FLEXUNIT"
       | CON_FLEXRECORD (ref(INDIRECT_FLEXINFO rf)) => pp_con seen (CON_FLEXRECORD rf)
       | (CON_RECORD rdecs) => pp_recordcon seen (false,rdecs)
       | CON_FLEXRECORD (ref (FLEXINFO (_,isrigid,rdecs))) => pp_recordcon seen (not isrigid,rdecs)
       | CON_FUN (vlist,con) => HOVbox[String "/-\\",
				       pp_list pp_var vlist ("(", ",",")", false),
				       pp_con seen con]
       | CON_SUM {names,noncarriers,carrier,special} => 
	      HOVbox[String (case special of 
				 NONE => "SUM"
			       | SOME x => "SUM_" ^ (Int.toString x)),
		     String "[", Break,
		     pp_commalist pp_label names,
		     String ";", Break,
		     String (Int.toString noncarriers),
		     String ";", Break,
		     pp_con seen carrier,
		     String "]"]
       | CON_TUPLE_INJECT conlist => pp_list (pp_con seen) conlist ("(", ",",")",false)
       | CON_TUPLE_PROJECT (i,c) => HOVbox[pp_con seen c, String ("#" ^ (Int.toString i))]
       | CON_MODULE_PROJECT (module,label) => 
	      (case con2path arg_con of
		   NONE => pp_region "CON_MPROJ(" ")"
		             [pp_mod seen module,
			      String ", ",
			      pp_label label]
		 | SOME path => HOVbox[String "CON_PATH(", pp_path path, String ")"]))


    and pp_mod seen module =
	  (case module of
	     MOD_VAR var => pp_var var
	   | MOD_STRUCTURE sbnds => pp_list (pp_sbnd seen) sbnds ("STR[",", ","]", true)
	   | MOD_FUNCTOR (a,v,s1,m,s2) => HOVbox[String "FUNC(",
					       pp_var v,
					       String ": ",
					       pp_signat seen s1,
					       pp_arrow a, Break,
					       pp_mod seen m, Break,
					       String ": ", Break,
					       pp_signat seen s2,
					       String ")"]
	   | MOD_APP (m1,m2) => pp_region "MAPP(" ")"
	                         [pp_mod seen m1,
				  String ", ",
				  Break,
				  pp_mod seen m2]
	   | MOD_PROJECT (m,l) => pp_region "MPROJ(" ")"
                                    [pp_mod seen m,
				     String ",",
				     pp_label l]
	   | MOD_LET (v,m,body) =>
		 Vbox0 0 1 [String "MOD_LET ",
			    pp_var v,
			    String " = ",
			    pp_mod seen m,
			    Break,
			    String "IN  ",
			    pp_mod seen body,
			    Break,
			    String "END"]
	   | MOD_SEAL(m,s) => pp_listid [pp_mod seen m, pp_signat seen s] ("MOD_SEAL(", ",", ")", true))
	
    and pp_phrase_class full seen pc = 
	let val pp_con = pp_con seen
	    val pp_exp = pp_exp seen
	    val pp_kind = pp_kind seen
	    val pp_mod = pp_mod seen
	    val pp_signat = pp_signat seen
	    fun help leftstr rightstr items = 
		HOVbox(if full
			   then (String leftstr) :: (items @ [String rightstr])
		       else items)
		    
	in  
	    (case pc of
		 PHRASE_CLASS_EXP  (e,c,eopt,inline) => 
		     help (if inline then "PC_EXP_INLINE" else "PC_EXP(") ")"
		     ([pp_exp e, String ": ", pp_con c] @
		      (case eopt of
			   NONE => []
			 | SOME e => [String "= ", pp_exp e]))
	       | PHRASE_CLASS_CON  (c,k,copt,inline) =>
		     help (if inline then "PC_CON_INLINE" else "PC_CON(") ")"
		     ([pp_con c, String ": ", pp_kind k] @
		      (case copt of
			   NONE => []
			 | SOME c => [String "= ", pp_con c]))
	       | PHRASE_CLASS_MOD  (m,false,s)  => 
		     help "PC_MOD(" ")" [pp_mod m, String ": ", pp_signat s]
	       | PHRASE_CLASS_MOD  (m,true,s)  => 
		     help "PC_$POLY$MOD(" ")" [pp_mod m, String ": ", pp_signat s]
	       | PHRASE_CLASS_SIG  (v,s)  => 
		     help "PC_SIG(" ")" [pp_var v, String " = ", pp_signat s]
	       | PHRASE_CLASS_OVEREXP _ => String "PC_OVEREXP")
	end


    and pp_kind seen kind = (case kind of
			    KIND_TUPLE 1 => String "TYPE"
			  | KIND_TUPLE i => String ("KIND(" ^ (Int.toString i) ^ ")")
			  | KIND_ARROW (i,j) => String ("KIND(" ^ (Int.toString i) ^
							" -> " ^ (Int.toString j) ^ ")"))

    and pp_elist seen exps = pp_list (pp_exp seen) exps ("[",",","]",false)
    and pp_clist seen cons = pp_list (pp_con seen) cons ("[",",","]",false)

    and pp_exp seen exp = 
      let val pp_con = pp_con seen
	  val pp_exp = pp_exp seen
	  val pp_clist = pp_clist seen
	  val pp_elist = pp_elist seen
	  val pp_mod = pp_mod seen
	  val pp_fbnd = pp_fbnd seen
	  val pp_bnd = pp_bnd seen
      in  
       (case exp of
	 OVEREXP (c,_,exp) => (case oneshot_deref exp of
				 NONE => String "OVEREXP_NONE"
			       | (SOME e) => pp_exp e)
       | SCON scon => pp_value' (fn (SCON scon) => SOME scon | _ => NONE) pp_exp pp_con scon
       | ETAPRIM (prim,cons) => HOVbox[pp_prim' prim, pp_clist cons]
       | ETAILPRIM (ilprim,cons) => HOVbox[pp_ilprim' ilprim, pp_clist cons]
       | PRIM (prim,cons,elist) => HOVbox[pp_prim' prim,
					  pp_clist cons,
					  pp_elist elist]
       | ILPRIM (prim,cons,elist) => HOVbox[pp_ilprim' prim,
					    pp_clist cons,
					    pp_elist elist]
       | VAR var => pp_var var
       | APP (e1,e2) => pp_region "APP(" ")" [pp_exp e1, String ",", Break, pp_exp e2]
       | EXTERN_APP (c,e1,elist) => 
	     pp_region "EXTERN_APP(" ")" [pp_con c, String ";", Break, 
					  pp_exp e1, String ";", Break, 
					  pp_elist elist]
       | FIX (r,a,[FBND(v',v,c,cres,e)]) => 
		  HOVbox[String ((case a of TOTAL => "/TOTAL" | PARTIAL => "/") ^
				 (if r then "\\" else "NONRECUR\\")),
			 pp_var v', Break0 0 5,
			 String " (", pp_var v,	 String " : ", pp_con c, String ")", Break0 0 5,
			 String " : ", pp_con cres, String " =", Break,
			 pp_exp e]
       | FIX (r,a,fbnds) => HOVbox[String ((case a of TOTAL => "TOTALFIX " | PARTIAL => "FIX") ^
					   (if r then "\\" else "-LEAF\\")),
				 pp_list pp_fbnd fbnds ("[",",","]", true),
				 String "END "]
       | RECORD [] => String "unit"
       | RECORD rbnds =>  let val (format,doer) = if (rbnds_is_tuple rbnds)
						    then (("(", ",",")", false), 
							  fn (l,e) => (pp_exp e))
						  else
						    (("{", ",","}", false), 
						     fn (l,e) => 
						     Hbox[pp_label l,
							  String " = ",
							  pp_exp e])
			  in pp_list doer rbnds format
			  end
       | RECORD_PROJECT (e,l,_) => HOVbox[pp_region "(" ")" [pp_exp e], String "#", pp_label l]
       | SUM_TAIL (i,c,e) => pp_region "SUM_TAIL(" ")" [pp_con c, String ",", pp_exp e]
       | HANDLE (body,handler) => Vbox[HOVbox[String "HANDLE ",
					      pp_exp body],
				       Break0 0 0,
				       HOVbox[String "WITH ",
					      pp_exp handler]]
       | RAISE (c,e) =>  pp_region "RAISE(" ")" [pp_con c, String ", ", pp_exp e]
       | LET (bs,e) => Vbox0 0 1 [String "LET ",
				  Vbox(separate (map pp_bnd bs) (Break0 0 0)),
				  Break,
				  String "IN  ",
				  pp_exp e,
				  Break,
				  String "END"]
       | NEW_STAMP con => pp_region "NEW_STAMP(" ")" [pp_con con]
       | EXN_INJECT (s,e1,e2) => pp_region "EXN_INJECT(" ")" [String s, String ",",
							      pp_exp e1, String ",", pp_exp e2]
       | ROLL (con,e) => pp_region "ROLL(" ")"
			  [pp_con con, String ",", pp_exp e]
       | UNROLL (con1,con2,e) => pp_region "UNROLL(" ")"
			  [pp_con con1, String ",", 
			   pp_con con2, String ",", pp_exp e]
       | INJ {sumtype,field,inject} => 
	     pp_region "INJ(" ")"
	     [String (Int.toString field), String ", ",
	      pp_con sumtype, 
	      case inject of
		  NONE => String ""
		| SOME e => HOVbox[String ",", pp_exp e]]
       | CASE {sumtype,bound,arg,arms,tipe,default} =>
	     pp_region "CASE(" ")"
	     ((pp_exp arg) :: (String ":") :: (pp_con sumtype) :: (String ",") :: Break ::
	      (String "result_type = ") :: (pp_con tipe) :: (String ",") :: Break ::
	      (String "bound = ") :: (pp_var bound) :: (String ",") :: Break ::
	      (pp_list (fn NONE => String "NONE" 
	                | SOME e => pp_exp e) arms ("[",", ","]",true)) ::
	      (String ", ") :: Break ::
	      (case default of
		   NONE => [String "NODEFAULT"]
		 | SOME e => [String "DEFAULT: ", pp_exp e]))
       | EXN_CASE {arg=earg,arms=elist,default=eopt,tipe} => 
	     pp_region "EXN_CASE(" ")"
			  [pp_con tipe,
			   String ",",
			   pp_exp earg,
			   String ",",
			   Break,
			   (pp_list (fn (e1,c,e2) => HOVbox[pp_exp e1,
							    String " : ", 
							    pp_con c, 
							    String " => ",
							    pp_exp e2]) elist ("[",", ","]",true)),
			   (case eopt of
			       NONE => String "NONE"
			     | SOME e => HOVbox[String "SOME", pp_exp e])]
       | MODULE_PROJECT (m,l) => HOVbox[pp_mod m, String ".", pp_label l]
       | SEAL    (exp,con) => pp_region "SEAL(" ")" [pp_exp exp, String ",", pp_con con])
      end

	 
    and pp_fbnd seen (FBND(vname,varg,carg,cres,exp)) = 
      HOVbox[pp_var vname,
	     (pp_region "=" "" [String "(",
				pp_var varg,
				String " : ",
				pp_con seen carg,
				String ") : ",
				pp_con seen cres,
				String " -> ",
				pp_exp seen exp])]

    and pp_signat seen signat = 
      (case signat of
	 SIGNAT_VAR v => pp_var v 
       | SIGNAT_OF p =>  HOVbox[String "SIGS_OF(",
				pp_path p,
				String ")"]
       | SIGNAT_STRUCTURE (NONE,sdecs) => pp_sdecs seen sdecs
       | SIGNAT_STRUCTURE (SOME p,sdecs) => HOVbox[String "SIGS_NORM(",
						   pp_path p, String ", ", Break,
						   pp_sdecs seen sdecs,
						   String ")"]
       | SIGNAT_FUNCTOR (v,s1,s2,a) => HOVbox0 1 8 1 
	                                        [String "SIGF(",
						 pp_var v,
						 String ", ",
						 pp_signat seen s1,
						 String " ",
						 pp_arrow a,
						 Break0 1 8,
						 pp_signat seen s2,
						 String ")"])
	     
    and pp_path (PATH (v,ls)) = 
	  (case ls of
	     [] => pp_var v
	   | _ => HOVbox[Hbox[pp_var v, String "."], 
			 pp_list pp_label ls ("",".","",false)])

    and pp_fixity_list lf_list = pp_list (fn (l,f) => HOVbox[pp_label l, String " : ", pp_fixity f])
                                         lf_list ("(",", ",")", false)
    and pp_bnd' seen bnd = 
	let fun help x y = (x,y)
	  in (case bnd of
		BND_EXP (v,e) => help (pp_var v) [pp_exp seen e]
	      | BND_MOD (v,false,m) => help (pp_var v) [pp_mod seen m]
	      | BND_MOD (v,true,m) => help (pp_var v) [String " $POLY$ ", 
						       pp_mod seen m]
	      | BND_CON (v,c) => help (pp_var v) [pp_con seen c])
	  end

    and pp_dec' seen dec = 
      let fun help x y = (x,y)
      in (case dec of
	    DEC_EXP (v,c,NONE, _) => help (pp_var v) [pp_con seen c]
	  | DEC_EXP (v,c,SOME e, inline) => 
		help (pp_var v) [pp_con seen c, String (if inline then " == " else " = "), 
				 pp_exp seen e]
	  | DEC_CON (v,k,NONE, _) => help (pp_var v) [pp_kind seen k]
	  | DEC_CON (v,k,SOME c, inline) => 
		help (pp_var v) [pp_kind seen k, String (if inline then " == " else " = "), 
				 pp_con seen c]
	  | DEC_MOD (v,false,s) => help (pp_var v) [pp_signat seen s]
	  | DEC_MOD (v,true,s) => help (pp_var v) [String " $POLY$ ", 
						   pp_signat seen s])
      end

    and pp_dec seen dec = let val (f,fs) = pp_dec' seen dec
			  in HOVbox(f ::  (String " = ") :: (Break0 0 3) :: fs)
			  end
    and pp_bnd seen bnd = let val (f,fs) = pp_bnd' seen bnd
			  in HOVbox(f ::  (String " = ") :: (Break0 0 3) :: fs)
			  end

    and pp_sdecs seen sdecs = pp_list (pp_sdec seen) sdecs ("[",", ", "]", true)
    and pp_sbnds seen sbnds = pp_list (pp_sbnd seen) sbnds ("[",", ", "]", true)

    and pp_sdec seen (SDEC(label,dec)) = let val (f,fs) = pp_dec' seen dec
					 in Depth(HOVbox((Hbox[pp_label label,
							      String " > ",
							       f, String " : "])::
							 (Break0 0 3) :: fs))
					 end
    and pp_sbnd seen (SBND(label,bnd)) = let val (f,fs) = pp_bnd' seen bnd
					 in Depth(HOVbox((Hbox[pp_label label,
							      String " > ",
							       f, String " = "])::
							 (Break0 0 3) :: fs))
					 end


    fun pp_bnds  bnds = pp_list (pp_bnd []) bnds ("[",",","]",true)
    fun pp_decs  decs = pp_list (pp_dec []) decs ("[",",","]",true)

    fun help pp = pp
    fun help' pp obj = (wrapper pp TextIO.stdOut obj; ())

    val pp_var' = help pp_var
    val pp_label'  = help pp_label
    val pp_con' = help (pp_con [])
    val pp_kind' = help (pp_kind [])
    val pp_value' = pp_value' (fn (SCON scon) => SOME scon | _ => NONE) (pp_exp []) (pp_con [])
    val pp_prim' = help pp_prim'
    val pp_mod' = help (pp_mod [])
    val pp_exp' = help (pp_exp [])
    val pp_signat' = help (pp_signat [])
    val pp_list' = pp_list
    val pp_commalist' = pp_commalist
    val pp_semicolonlist' = pp_semicolonlist
    val pp_pathlist' = pp_pathlist
    val pp_path' = help pp_path
    val pp_bnd' = help (pp_bnd [])
    val pp_bnds' = help pp_bnds
    val pp_sbnd' = help (pp_sbnd [])
    val pp_sbnds' = help (pp_sbnds [])
    val pp_dec' = help (pp_dec [])
    val pp_sdec' = help (pp_sdec [])
    val pp_decs' = help pp_decs
    val pp_sdecs' = help (pp_sdecs [])
    val pp_phrase_class' = help (pp_phrase_class true [])

    fun pp_context_entry' (CONTEXT_SDEC sdec) = HOVbox[String "CONTEXT_SDEC: ", pp_sdec [] sdec]
      | pp_context_entry' (CONTEXT_SIGNAT (l,v,s)) = HOVbox[String "CONTEXT_SIGNAT: ",
							    pp_label l, String " > ", pp_var v,
							    String " = ", pp_signat [] s]
      | pp_context_entry' (CONTEXT_FIXITY _) = String "CONTEXT_FIXITY ???"
      | pp_context_entry' (CONTEXT_OVEREXP _) = String "CONTEXT_OVEREXP ???"

    fun pp_context' (CONTEXT{pathMap, ordering, ...}) = 
	let fun lookup (PATH p) = let val SOME (lab,pc) = Name.PathMap.find(pathMap,p)
				  in  (lab,p,pc)
				  end
	    val label_pathpc_list = map lookup (rev ordering)
	    fun doer(lbl,path,pc) = HOVbox[pp_label lbl,
					   String " --> ",
					   pp_path (PATH path),
					   String " = ",
					   pp_phrase_class true [] pc]
	in  pp_list doer label_pathpc_list ("[",", ", "]", true)
	end


    val pp_var = help' pp_var
    val pp_label  = help' pp_label
    val pp_con = help' (pp_con [])
    val pp_kind = help' (pp_kind [])
    val pp_value = help' pp_value'
    val pp_prim = help' Ppprim.pp_prim'
    val pp_mod = help' (pp_mod [])
    val pp_exp = help' (pp_exp [])
    val pp_context_entry = help' pp_context_entry'
    val pp_context = help' pp_context'
    val pp_signat = help' (pp_signat [])
    fun pp_list doer data = help' (pp_list' doer data)
    fun pp_commalist pobj objlist = pp_list pobj objlist ("(",", ",")",false)
    fun pp_semicolonlist pobj objlist = pp_list pobj objlist ("(","; ",")",true)
    fun pp_pathlist pobj objlist = pp_list pobj objlist ("",".","",false)
    val pp_path = help' pp_path
    val pp_bnd = help' (pp_bnd [])
    val pp_bnds = help' pp_bnds
    val pp_sbnd = help' (pp_sbnd [])
    val pp_sbnds = help' (pp_sbnds [])
    val pp_dec = help' (pp_dec [])
    val pp_sdec = help' (pp_sdec [])
    val pp_decs = help' pp_decs
    val pp_sdecs = help' (pp_sdecs [])
    val pp_phrase_class = help' (pp_phrase_class true [])

  end
