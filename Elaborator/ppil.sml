(* Il pretty-printer. *)
functor Ppil(structure Il : IL
	     structure AstHelp : ASTHELP
	     structure Ppprim : PPPRIM
	     sharing Il.Prim = Ppprim.Prim)
	: PPIL  = 
  struct

    structure Il = Il
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
    fun pp_tag n = String(tag2string n)


    fun pp_fixity fixity = 
      (case fixity of
	 Fixity.NONfix => String "NONfix"
       | Fixity.INfix (a,b) => String ("INfix" ^ (if (a mod 2 = 0) then "l" else "r")
				       ^ (Int.toString (a div 2))))
    fun pp_kind kind = String (case kind of
				 KIND_TUPLE 1 => "TYPE"
			       | KIND_TUPLE i => ("KIND(" ^ (Int.toString i) ^ ")")
			       | KIND_ARROW (i,j) => ("KIND(" ^ (Int.toString i) ^
						      " -> " ^ (Int.toString j) ^ ")"))

    local
      (* these 3 functions copied from ilutil.sml; no recursive modules... *)
      fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
      fun generate_tuple_label (i : int) = symbol_label(generate_tuple_symbol i)
      fun loop [] _ = true
	| loop ((l,_)::rest) cur = eq_label(l,generate_tuple_label cur) andalso loop rest (cur+1) 
    in
      fun rdecs_is_tuple rdecs = loop rdecs 1
      fun rbnds_is_tuple rbnds = loop rbnds 1
    end

    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  fmt)
      end
    fun pp_complete oneshot = 
      String (case (oneshot_deref oneshot) of
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
	 (CON_INT _ | CON_FLOAT _ | CON_UINT _ | CON_ANY | CON_VAR _) => true
       | (CON_TYVAR tyvar) => (case tyvar_deref tyvar of
				       NONE => true
				     | SOME c => is_base_con c)
       | _ => false)



    fun pp_con (seen : (decs,con) tyvar list) (arg_con : con) : format = 
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
       | CON_ARROW (c1,c2,comp) => HOVbox [String "(",	
					   pp_con seen c1,String " ",
					   pp_complete comp,
					   Break,
					   pp_con seen c2, String ")"]
       | CON_APP (c1,c2) => pp_region "CON_APP(" ")"
	                              [pp_con seen c1,
				       String ",",
				       Break,
				       pp_con seen c2]
       | CON_MUPROJECT(i,c) => pp_region ("CON_MUPROJECT(" ^ (Int.toString i) ^ "; ") ")"
	                              [pp_con seen c]
       | CON_RECORD [] => String "UNIT"
       | CON_FLEXRECORD (ref(FLEXINFO(_,true,[]))) => String "UNIT"
       | CON_FLEXRECORD (ref(FLEXINFO(_,false,[]))) => String "FLEXUNIT"
       | CON_FLEXRECORD (ref(INDIRECT_FLEXINFO rf)) => pp_con seen (CON_FLEXRECORD rf)
       | (CON_RECORD _ | CON_FLEXRECORD (ref (FLEXINFO _))) =>
	     let 
		 val (isflex,rdecs) = 
		     (case arg_con of
			  CON_RECORD rdecs => (false,rdecs)
			| CON_FLEXRECORD (ref (FLEXINFO(_,true,rdecs))) => (false,rdecs)
			| CON_FLEXRECORD (ref (FLEXINFO(_,false,rdecs))) => (true,rdecs)
                        | _ => error "must have record or direct flex_record here")
		 val is_tuple = rdecs_is_tuple rdecs
		 val format = (case (is_tuple,isflex) of
				   (true,false) => ("{", " *","}", false)
				 | (true,true) => ("{?", " *","?}", false)
				 | (false,false) => ("{", ",","}", false)
				 | (false,true) => ("{?", ",","?}", false))
		 val doer = if is_tuple
				then (fn (l,c) => pp_con seen c)
			    else
				(fn (l,c) => Hbox[pp_label l,
						  String " = ",
						  pp_con seen c])
	     in pp_list doer rdecs format
	     end
       | CON_FUN (vlist,con) => HOVbox[String "/-\\",
				       pp_list pp_var vlist ("(", ",",")", false),
				       pp_con seen con]
       | CON_SUM (iopt,conlist) => pp_list (pp_con seen) conlist ("SUM"^ (case iopt of 
									      NONE => ""
									    | SOME x => (Int.toString x))
								  ^"(", 
							   ",",")", false)
       | CON_TUPLE_INJECT conlist => pp_list (pp_con seen) conlist ("(", ",",")",false)
       | CON_TUPLE_PROJECT (i,c) => HOVbox[pp_con seen c, String ("#" ^ (Int.toString i))]
       | CON_MODULE_PROJECT (module,label) => pp_region "CON_MPROJ(" ")"
				                 [pp_mod seen module,
						  String ", ",
						  pp_label label])

    and pp_db (db : Ast.db) = 
      let 
	open AstHelp
	fun pp_def (sym,NONE) = pp_sym' sym
	  | pp_def (sym, SOME ty) = HOVbox[pp_sym' sym, 
					   String " of ",
					   pp_ty' ty]
	val (name,tparams,defs) = db_strip db
      in HOVbox((case tparams of
		  [] => []
		| _ => [pp_list pp_tyvar' tparams ("(",", ",")",false)]) @
		[pp_sym' name,
		 String " = ",
		 pp_list pp_def defs (""," | ","",true)])
      end

    and pp_tb (tb : Ast.tb) = 
      let 
	open AstHelp 
	val (name,tparams,ty) = tb_strip tb
      in 
	HOVbox((case tparams of
		  [] => []
		| _ => [pp_list pp_tyvar' tparams ("(",", ",")",false)]) @
	       [pp_sym' name,
		String " = ",
		pp_ty' ty])
      end

    and pp_mod seen module =
	  (case module of
	     MOD_VAR var => pp_var var
	   | MOD_STRUCTURE sbnds => pp_list (pp_sbnd seen) sbnds ("STR[",", ","]", true)
	   | MOD_FUNCTOR (v,s,m) => HOVbox[String "FUNC(",
					   pp_var v,
					   String ", ",
					   pp_signat seen s,
					   String ", ", Break,
					   pp_mod seen m,
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
	   | MOD_SEAL(m,s) => pp_listid [pp_mod seen m, pp_signat seen s] ("MOD_SEAL(", ",", ")", true))
	     
    and pp_inline seen (INLINE_MODSIG (m,s)) = HOVbox[pp_mod seen m, String ":", Break, pp_signat seen s]
      | pp_inline seen (INLINE_EXPCON (e,c)) = HOVbox[pp_exp seen e, String ":", pp_con seen c]
      | pp_inline seen (INLINE_CONKIND (c,k)) = HOVbox[pp_con seen c, String ":", pp_kind k]
      | pp_inline seen (INLINE_OVER _) = String "INLINE_OVER"

    and pp_context seen (CONTEXT entries) = 
      let fun dolv l v fmts = HOVbox((pp_label l) :: (String ">") :: (pp_var v) :: 
				     (String "=") :: (Break0 0 3) :: fmts)
	fun helper (CONTEXT_INLINE (l,v,inl)) = HOVbox[pp_label l, String " >> ", pp_var v, 
						       String " = ", pp_inline seen inl]
	  | helper (CONTEXT_SDEC(SDEC(l,dec))) = 
	    (case dec of
		 (DEC_EXP(v,c)) => dolv l v [pp_con seen c]
	       | (DEC_CON(v,k,SOME c)) => dolv l v [pp_con seen c, String ":", pp_kind k]
	       | (DEC_CON(v,k,NONE)) => dolv l v [String ":", pp_kind k]
	       | (DEC_MOD(v,s)) => dolv l v [pp_signat seen s]
	       | (DEC_FIXITY vf_list) => HOVbox[String "FIXITY", pp_fixity_list vf_list]
	       | (DEC_EXCEPTION (tag,c)) => HOVbox[String "EXCEPTION: ", pp_tag tag, String " = ",
						   pp_con seen c])
	  | helper (CONTEXT_SIGNAT(l,v,s)) = dolv l v [String " OMEGA = ",pp_signat seen s]
	  | helper (CONTEXT_SCOPED_TYVAR syms) = pp_list AstHelp.pp_sym'
	                                             syms ("TYVARS[", ", ", "]", false)

      in pp_list helper entries ("CONTEXT(", ", ", ")", true)
      end


    and pp_exp seen exp = 
      (case exp of
	 OVEREXP (c,_,exp) => (case oneshot_deref exp of
				 NONE => String "OVEREXP_NONE"
			       | (SOME e) => pp_exp seen e)
       | SCON scon => pp_value' (pp_exp seen) scon
       | PRIM (prim,cons) => HOVbox[pp_prim' prim,
				    pp_list (pp_con seen) cons ("[",",","]",false)]
       | ILPRIM ip => pp_ilprim' ip
       | VAR var => pp_var var
       | APP (e1,e2) => pp_region "APP(" ")" [pp_exp seen e1, String ",", Break, pp_exp seen e2]
       | FIX ([FBND(v',v,c,cres,e)],var) => if eq_var(v',var)
						  then HOVbox[String "/\\",  pp_var v',
							      String " (", pp_var v, Break0 0 5,
							      String " : ",
							      pp_con seen c, String ")", Break0 0 5,
							      String " :", pp_con seen cres, 
							      String " =", Break,
							      pp_exp seen e]
						else error "FIX of one thing not a lambda"
       | FIX (fbnds, var) => HOVbox[String "FIX ",
						  pp_list (pp_fbnd seen) fbnds ("[",",","]", true),
						  String "IN  ",
						  pp_var var,
						  String "END "]
(*       | SEQ elist => pp_list (pp_exp seen) elist ("(", ";",")", true) *)
       | RECORD [] => String "unit"
       | RECORD rbnds =>  let val (format,doer) = if (rbnds_is_tuple rbnds)
						    then (("(", ",",")", false), 
							  fn (l,e) => (pp_exp seen e))
						  else
						    (("{", ",","}", false), 
						     fn (l,e) => 
						     Hbox[pp_label l,
							  String " = ",
							  pp_exp seen e])
			  in pp_list doer rbnds format
			  end
       | RECORD_PROJECT (e,l,_) => HOVbox[pp_region "(" ")" [pp_exp seen e], String "#", pp_label l]
       | SUM_TAIL (c,e) => pp_region "SUM_TAIL(" ")" [pp_con seen c, String ",", pp_exp seen e]
       | HANDLE (body,handler) => Vbox[HOVbox[String "HANDLE ",
					      pp_exp seen body],
				       Break0 0 0,
				       HOVbox[String "WITH ",
					      pp_exp seen handler]]
       | RAISE (c,e) =>  pp_region "RAISE(" ")" [pp_con seen c, String ", ", pp_exp seen e]
       | LET (bs,e) => Vbox0 0 1 [String "LET ",
				  Vbox(separate (map (pp_bnd seen) bs) (Break0 0 0)),
				  Break,
				  String "IN  ",
				  pp_exp seen e,
				  Break,
				  String "END"]
       | NEW_STAMP con => pp_region "NEW_STAMP(" ")" [pp_con seen con]
       | EXN_INJECT (e1,e2) => pp_region "EXN_INJECT(" ")" [pp_exp seen e1, String ",", pp_exp seen e2]
       | MK_REF (exp)  =>  pp_region "REF(" ")" 
			  [pp_exp seen exp]
       | GET (exp)  => pp_region "GET(" ")" [pp_exp seen exp]
       | SET (e1,e2) => pp_region "SET(" ")" [pp_exp seen e1, String ",", pp_exp seen e2]
       | ROLL (con,e) => pp_region "ROLL(" ")"
			  [pp_con seen con, pp_exp seen e]
       | UNROLL (con,e) => pp_region "UNROLL(" ")"
			  [pp_con seen con, String ",", pp_exp seen e]
       | INJ  (conlist, i, e) => pp_region "INJ(" ")"
			  [pp_list (pp_con seen) conlist ("[",", ","]",false), 
			   String ("," ^ (Int.toString i) ^ ","), pp_exp seen e]
       | PROJ (conlist,i,e) => pp_region "PROJ(" ")" 
			  [pp_list (pp_con seen) conlist ("[",", ","]",false),
			   String ((Int.toString i) ^ ","), 
			   pp_exp seen e]
       | TAG (name,c) => pp_region "TAG(" ")" [pp_tag name, pp_con seen c]
       | CASE (cs,earg,elist,edef) => pp_region "CASE(" ")"
			  ((pp_con seen (CON_SUM (NONE,cs))) :: (String ",") :: Break ::
			   (pp_exp seen earg) :: (String ",") :: Break ::
			   (pp_list (fn NONE => String "NONE" 
			 | SOME e => pp_exp seen e) elist ("[",", ","]",true)) ::
			   (case edef of
			      NONE => []
			    | SOME e => [String ", DEFAULT: ", pp_exp seen e]))
       | EXN_CASE (earg,elist,eopt) => pp_region "EXN_CASE(" ")"
			  [pp_exp seen earg,
			   String ",",
			   Break,
			   (pp_list (fn (e1,c,e2) => HOVbox[pp_exp seen e1,
							    String " : ", 
							    pp_con seen c, 
							    String " => ",
								pp_exp seen e2]) elist ("[",", ","]",true)),
			   (case eopt of
			       NONE => String "NONE"
			     | SOME e => HOVbox[String "SOME", pp_exp seen e])]
       | MODULE_PROJECT (m,l) => HOVbox[pp_mod seen m, String ".", pp_label l]
       | SEAL    (exp,con) => pp_region "SEAL(" ")" [pp_exp seen exp, String ",", pp_con seen con])

	 
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
	 SIGNAT_STRUCTURE sdecs => pp_list (pp_sdec seen) sdecs ("SIGS[",", ", "]", true)
       | SIGNAT_FUNCTOR (v,s1,s2,comp) => HOVbox0 1 8 1 
	                                        [String "SIGF(",
						 pp_var v,
						 String ", ",
						 pp_signat seen s1,
						 String " ",
						 pp_complete comp,
						 Break0 1 8,
						 pp_signat seen s2,
						 String ")"])
	     
    and pp_path path = 
	  (case path of
	     SIMPLE_PATH v => pp_var v
	   | COMPOUND_PATH (v,ls) => HOVbox[Hbox[pp_var v, String "."], 
					    pp_list pp_label ls ("",".","",false)])

    and pp_fixity_list lf_list = pp_list (fn (l,f) => HOVbox[pp_label l, String " : ", pp_fixity f])
                                         lf_list ("(",", ",")", false)
    and pp_bnd' seen bnd = 
	let fun help x y = (x,y)
	  in (case bnd of
		BND_EXP (v,e) => help (pp_var v) [pp_exp seen e]
	      | BND_MOD (v,m) => help (pp_var v) [pp_mod seen m]
	      | BND_CON (v,c) => help (pp_var v) [pp_con seen c]
	      | BND_FIXITY vf_list => help (String "FIXITY") [pp_fixity_list vf_list])
	  end

    and pp_dec' seen dec = 
      let fun help x y = (x,y)
      in (case dec of
	    DEC_EXP (v,c) => help (pp_var v) [pp_con seen c]
	  | DEC_MOD (v,s) => help (pp_var v) [pp_signat seen s]
	  | DEC_CON (v,k,NONE) => help (pp_var v) [pp_kind k]
	  | DEC_CON (v,k,SOME c) => help (pp_var v) [pp_con seen c, String " = ", pp_kind k]
	  | DEC_EXCEPTION (n,c) =>  help (pp_tag n) [pp_con seen c]
	  | DEC_FIXITY vf_list => help (String "FIXITY") [pp_fixity_list vf_list])
      end

    and pp_dec seen dec = let val (f,fs) = pp_dec' seen dec
			  in HOVbox(f ::  (String " = ") :: (Break0 0 3) :: fs)
			  end
    and pp_bnd seen bnd = let val (f,fs) = pp_bnd' seen bnd
			  in HOVbox(f ::  (String " = ") :: (Break0 0 3) :: fs)
			  end
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
    fun pp_sbnds sbnds = pp_list (pp_sbnd []) sbnds ("[",",","]",true)
    fun pp_decs  decs = pp_list (pp_dec []) decs ("[",",","]",true)
    fun pp_sdecs sdecs = pp_list (pp_sdec []) sdecs ("[",",","]",true)

    fun help pp = pp
    fun help' pp obj = (wrapper pp TextIO.stdOut obj; ())

    val pp_var' = help pp_var
    val pp_label'  = help pp_label
    val pp_con' = help (pp_con [])
    val pp_kind' = help pp_kind
    val pp_value' = pp_value' (pp_exp [])
    val pp_prim' = help pp_prim'
    val pp_mod' = help (pp_mod [])
    val pp_exp' = help (pp_exp [])
    val pp_context' = help (pp_context [])
    val pp_signat' = help (pp_signat [])
    val pp_list' = pp_list
    fun pp_commalist' pobj objlist = pp_list' pobj objlist ("(",", ",")",false)
    fun pp_semicolonlist' pobj objlist = pp_list' pobj objlist ("(","; ",")",true)
    fun pp_pathlist' pobj objlist = pp_list' pobj objlist ("",".","",false)
    val pp_path' = help pp_path
    val pp_bnd' = help (pp_bnd [])
    val pp_bnds' = help pp_bnds
    val pp_sbnd' = help (pp_sbnd [])
    val pp_sbnds' = help pp_sbnds
    val pp_dec' = help (pp_dec [])
    val pp_sdec' = help (pp_sdec [])
    val pp_decs' = help pp_decs
    val pp_sdecs' = help pp_sdecs

    val pp_var = help' pp_var
    val pp_label  = help' pp_label
    val pp_con = help' (pp_con [])
    val pp_kind = help' pp_kind
    val pp_value = help' pp_value'
    val pp_prim = help' Ppprim.pp_prim'
    val pp_mod = help' (pp_mod [])
    val pp_exp = help' (pp_exp [])
    val pp_context = help' (pp_context [])
    val pp_signat = help' (pp_signat [])
    fun pp_list doer data = help' (pp_list' doer data)
    fun pp_commalist pobj objlist = pp_list pobj objlist ("(",", ",")",false)
    fun pp_semicolonlist pobj objlist = pp_list pobj objlist ("(","; ",")",true)
    fun pp_pathlist pobj objlist = pp_list pobj objlist ("",".","",false)
    val pp_path = help' pp_path
    val pp_bnd = help' (pp_bnd [])
    val pp_bnds = help' pp_bnds
    val pp_sbnd = help' (pp_sbnd [])
    val pp_sbnds = help' pp_sbnds
    val pp_dec = help' (pp_dec [])
    val pp_sdec = help' (pp_sdec [])
    val pp_decs = help' pp_decs
    val pp_sdecs = help' pp_sdecs

  end
