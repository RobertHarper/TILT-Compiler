(* Il pretty-printer. *)
functor Ppil(structure Il : IL
	     structure AstHelp : ASTHELP
	     structure Formatter : FORMATTER
	     sharing AstHelp.Formatter = Formatter)
	: PPIL  = 
  struct

    structure Il = Il
    structure Formatter = Formatter
    datatype display = VAR_ONLY | VALUE_ONLY | VAR_VALUE
    val convar_display = ref VALUE_ONLY

    open Il Formatter
    open Util Name Prim Tyvar

    val error = error "ppil.sml"


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

    fun pp_scon scon =
      (case scon of
	 INT i => String (makestring (Word32.toInt i))
       | UINT i => String ("u" ^ (makestring (Word32.toInt i)))
       | STRING s => String ("\"" ^ s ^ "\"")
       | BOOL b => String (makestring b)
       | FLOAT s => String s
       | CHAR c => String ("#\"" ^ (str c) ^ "\""))
    fun pp_fixity fixity = 
      (case fixity of
	 Fixity.NONfix => String "NONfix"
       | Fixity.INfix (a,b) => String ("INfix" ^ (if (a mod 2 = 0) then "l" else "r")
				       ^ (makestring (a div 2))))
    fun pp_kind kind = String (case kind of
				 KIND_TUPLE 1 => "TYPE"
			       | KIND_TUPLE i => ("KIND(" ^ (makestring i) ^ ")")
			       | KIND_ARROW (i,j) => ("KIND(" ^ (makestring i) ^
						      " -> " ^ (makestring j) ^ ")"))

    local
      (* these 3 functions copied from ilutil.sml; no recursive modules... *)
      fun generate_tuple_symbol (i : int) = Symbol.labSymbol(makestring i)
      fun generate_tuple_label (i : int) = symbol2label(generate_tuple_symbol i)
      fun loop [] _ = true
	| loop (l::rest) cur = eq_label(l,generate_tuple_label cur) andalso loop rest (cur+1) 
    in
      fun rdecs_is_tuple rdecs = loop(map (fn (RDEC(l,_)) => l) rdecs) 1
      fun rbnds_is_tuple rbnds = loop(map (fn (RBND(l,_)) => l) rbnds) 1
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
      let val _ = (print "member: lst of length "; print (length lst); print "\n")
	val res = member(a,lst)
	val _ = (print "returning: "; print res; print "\n")
      in res
      end

    (* is it basic with respect to printing *)
    fun is_base_con con = 
      (case con of
	 (CON_INT | CON_FLOAT | CON_UINT | CON_CHAR  | CON_ANY | CON_VAR _) => true
       | (CON_TYVAR tyvar) => (case tyvar_deref tyvar of
				       NONE => true
				     | SOME c => is_base_con c)
       | _ => false)

    fun pp_con seen arg_con : format = 
      (case arg_con of
	 CON_OVAR ocon => pp_con seen (ocon_deref ocon) 
       | CON_VAR var => pp_var var
       | CON_TYVAR tyvar =>
	   (case (tyvar_deref tyvar) of
	      NONE => if (tyvar_isconstrained tyvar)
			then Hbox [String "!", pp_var (tyvar_getvar tyvar)]
		      else pp_var (tyvar_getvar tyvar)
	    | (SOME con) => let val var = tyvar_getvar tyvar
			    in if (member_eq(eq_tyvar,tyvar,seen)) then
			      (print "var is "; (wrapper pp_var) std_out var; print "\n";
			       print "seen_refs("; print (length seen);
			       print ")var is: ";
			       map (fn v => (wrapper pp_var) std_out (tyvar_getvar v)) seen;
			       print "\n\n";
			       error "circular";
			       Hbox0 0 [String "KNOT_", pp_var var])
			       else
				 let val seen = tyvar :: seen
				 in (case (!convar_display) of
				       VALUE_ONLY => pp_con seen con
				     | VAR_ONLY => Hbox0 0 [String "C", pp_var var]
				     | VAR_VALUE => (pp_region "(" ")"
						     [pp_var var, String "==", pp_con seen con]))
				 end
			    end)
       | CON_INT     => String "INT"
       | CON_UINT    => String "UINT"
       | CON_FLOAT   => String "FLOAT"
       | CON_LIST c => Hbox[pp_con seen c, Break, String "LIST"]
       | CON_VECTOR c => Hbox[pp_con seen c, Break, String "VECTOR"]
       | CON_ARRAY c => Hbox[pp_con seen c, Break, String "ARRAY"]
       | CON_CHAR    => String "CHAR"
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
       | CON_MUPROJECT(i,c) => pp_region "[" ("]_" ^ (makestring i))
			      [pp_con seen c]
       | CON_RECORD [] => String "UNIT"
       | CON_RECORD rdecs => let val (format,doer) = if (rdecs_is_tuple rdecs)
							   then (("{", " *","}", false), 
								 fn (RDEC(l,c)) => pp_con seen c)
							 else
							   (("{", ",","}", false), 
							    fn (RDEC(l,c)) => 
							    Hbox[pp_label l,
								 String " = ",
								 pp_con seen c])
				 in pp_list doer rdecs format
				 end
       | CON_FUN (vlist,con) => HOVbox[String "/-\\",
				       pp_list pp_var vlist ("(", ",",")", false),
				       pp_con seen con]
       | CON_SUM (iopt,conlist) => pp_list (pp_con seen) conlist ("SUM"^ (case iopt of 
									      NONE => ""
									    | SOME x => (makestring x))
								  ^"(", 
							   ",",")", false)
       | CON_TUPLE_INJECT conlist => pp_list (pp_con seen) conlist ("(", ",",")",false)
       | CON_TUPLE_PROJECT (i,c) => HOVbox[pp_con seen c, String ("#" ^ (makestring i))]
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
(*	   | MOD_DATATYPE (dblist,tblist,sbnds) => 
	       HOVbox[String "DATATYPE(",
		      Break,
		      pp_list pp_db dblist ("DBs = ",", ","", true),
		      Break,
		      pp_list pp_tb tblist ("TBs = ",", ","", true),
		      Break,
		      pp_list (pp_sbnd seen) sbnds ("sbnds = [",", ","]", true),
		      String ")"] *)
	   | MOD_STRUCTURE sbnds => pp_list (pp_sbnd seen) sbnds ("STR[",", ","]", true)
	   | MOD_FUNCTOR (v,s,m) => HOVbox[String "FUNC(",
					   pp_var v,
					   String ", ",
					   pp_signat seen s,
					   Break,
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
	     
    and pp_inline seen (INLINE_MODSIG (m,s)) = HOVbox[pp_mod seen m, String ":", pp_signat seen s]
      | pp_inline seen (INLINE_EXPCON (e,c)) = HOVbox[pp_exp seen e, String ":", pp_con seen c]
      | pp_inline seen (INLINE_OVER _) = String "INLINE_OVER"

    and pp_context seen (CONTEXT entries) = 
      let fun dolv l v fmts = HOVbox((pp_label l) :: (String ">") :: (pp_var v) :: 
				     (String "=") :: (Break0 0 3) :: fmts)
	fun helper (CONTEXT_INLINE (l,inl)) = HOVbox[pp_label l, String ">>", pp_inline seen inl]
	  | helper (CONTEXT_VAR (l,v,c)) = dolv l v [pp_con seen c]
	  | helper (CONTEXT_CONVAR(l,v,k,SOME c)) = dolv l v [pp_con seen c, String ":", pp_kind k]
	  | helper (CONTEXT_CONVAR(l,v,k,NONE)) = dolv l v [String ":", pp_kind k]
	  | helper (CONTEXT_MODULE(l,v,s)) = dolv l v [pp_signat seen s]
	  | helper (CONTEXT_SIGNAT(l,v,s)) = dolv l v [pp_signat seen s, String " : OMEGA"]
	  | helper (CONTEXT_SCOPED_TYVAR syms) = pp_list AstHelp.pp_sym'
	                                             syms ("TYVARS[", ", ", "]", false)
	  | helper (CONTEXT_FIXITY vf_list) = HOVbox[String "FIXITY", pp_fixity_list vf_list]
      in pp_list helper entries ("CONTEXT(", ", ", ")", true)
      end

    and pp_prim seen prim = 
      let
	open Prim
	fun pp_tt INT_TT = String "INT_TT"
	  | pp_tt REAL_TT = String "REAL_TT"
	  | pp_tt BOTH_TT = String "BOTH_TT"

	fun pp_prim0 prim = 
	  (case prim of
(*	   | NILprim  {instance} => String "nil" *)
	     SOFT_VTRAPprim tt => String "SOFT_VTRAP"
	   | SOFT_ZTRAPprim tt => String "SOFT_ZTRAP"
	   | HARD_VTRAPprim tt => String "HARD_VTRAP"
	   | HARD_ZTRAPprim tt => String "HARD_ZTRAP")
	     
	fun pp_prim1 prim = 
	  (case prim of
(*	   | NOTprim => String "NOT" *)
	     MK_REFprim {instance : 'Type} => String "MK_REF"
	   | DEREFprim {instance : 'Type} => String "DEREF"
(*
	   | SIZEprim => String "SIZE"
	   | CHRprim => String "CHR"
	   | ORDprim => String "ORD"
	   | EXPLODEprim => String "EXPLODE"
	   | IMPLODEprim => String "IMPLODE"
*)
	   | NEG_FLOATprim => String "NEG_FLOAT"
	   | ABS_FLOATprim => String "ABS_FLOAT"
(*
	   | SQRTprim => String "SQRT"
	   | SINprim => String "SIN"
	   | COSprim => String "COS"
	   | ARCTANprim => String "ARCTAN"
	   | EXPprim => String "EXP"
	   | LNprim => String "LN"
*)
	   | NOT_INTprim  => String "NOT_INTp"
	   | NEG_INTprim  => String "NEG_INTp"
	   | ABS_INTprim => String "ABS_INT"
	   | NOT_UINTprim  => String "NOT_UINTp"
	   | FLOAT2INTprim => String "FLOAT2INT"
	   | INT2FLOATprim => String "INT2FLOAT"
	   | INT2UINTprim => String "INT2UINT"
	   | UINT2INTprim => String "UINT2INT"
(*
	   | OPEN_INprim => String "OPEN_INprim"
	   | OPEN_OUTprim => String "OPEN_OUT"
	   | INPUTprim => String "INPUT"
	   | LOOKAHEADprim => String "LOOKAHEAD"
	   | CLOSE_INprim => String "CLOSE_IN"
	   | END_OF_STREAMprim => String "END_OF_STREAM"
	   | CLOSE_OUTprim => String "CLOSE_OUT"
	   | USEprim => String "USE"
	   | FLUSH_OUTprim  => String "FLUSH_OUTp"

	   | ISNILprim {instance} => String "isnil"
	   | CARprim {instance} => String "car"
	   | CDRprim {instance} => String "cdr"
*)
	   | LENGTH1prim {instance} => String "length1"
(*	   | LENGTH2prim {instance} => String "length2" *)
		 )

	fun pp_prim2 prim = 
	  (case prim of
(*
	    ANDprim => String "AND"
	  | ORprim => String "OR"
	  | EQ_BOOLprim => String "EQ_BOOL"
	  | XORprim => String "XOR"
*)
	    EQ_REFprim _ => String "EQ_REF"
	  | SETREFprim _ => String "SETREF"
(*
	  | STRING_CONCATprim => String "STRING_CONCAT"
*)
	  | EQ_CHARprim => String "EQ_CHAR"
	  | NEQ_CHARprim => String "NEQ_CHAR"
(*
	  | EQ_STRINGprim => String "EQ_STRING"
	  | NEQ_STRINGprim => String "NEQ_STRING"
*)
	  | PLUS_FLOATprim => String "PLUS_FLOAT"
	  | MINUS_FLOATprim => String "MINUS_FLOAT"
	  | MUL_FLOATprim => String "MUL_FLOAT"
	  | DIV_FLOATprim => String "DIV_FLOATprim"
	  | LESS_FLOATprim => String "LESS_FLOAT"
	  | GREATER_FLOATprim => String "GREATER_FLOAT"
	  | LESSEQ_FLOATprim => String "LESSEQ_FLOAT"
	  | GREATEREQ_FLOATprim => String "GREATEREQ_FLOAT"
	  | EQ_FLOATprim => String "EQ_FLOAT"
	  | NEQ_FLOATprim => String "NEQ_FLOAT"
	  | PLUS_INTprim => String "PLUS_INT"
	  | MINUS_INTprim => String "MINUS_INT"
	  | MUL_INTprim => String "MUL_INT"
	  | DIV_INTprim => String "DIV_INT"
	  | MOD_INTprim => String "MOD_INT"
	  | QUOT_INTprim => String "QUOT_INT"
	  | REM_INTprim => String "REM_INT"
	  | LESS_INTprim => String "LESS_INT"
	  | GREATER_INTprim => String "GREATER_INT"
	  | LESSEQ_INTprim => String "LESSEQ_INT"
	  | GREATEREQ_INTprim => String "GREATEREQ_INT"
	  | EQ_INTprim => String "EQ_INT"
	  | NEQ_INTprim => String "NEQ_INT"
	  | LSHIFT_INTprim => String "LSHIFT_INT"
	  | RSHIFT_INTprim => String "RSHIFT_INT"
	  | AND_INTprim => String "AND_INT"
	  | OR_INTprim => String "OR_INT"
	  | PLUS_UINTprim => String "PLUS_UINT"
	  | MINUS_UINTprim => String "MINUS_UINT"
	  | MUL_UINTprim  => String "MUL_UINTp"
	  | DIV_UINTprim => String "DIV_UINT"
	  | MOD_UINTprim => String "MOD_UINT"
	  | LESS_UINTprim => String "LESS_UINT"
	  | GREATER_UINTprim => String "GREATER_UINT"
	  | LESSEQ_UINTprim => String "LESSEQ_UINT"
	  | GREATEREQ_UINTprim => String "GREATEREQ_UINT"
	  | EQ_UINTprim => String "EQ_UINT"
	  | NEQ_UINTprim => String "NEQ_UINT"
	  | LSHIFT_UINTprim => String "LSHIFT_UINT"
	  | RSHIFT_UINTprim => String "RSHIFT_UINT"
	  | AND_UINTprim => String "AND_UINT"
	  | OR_UINTprim => String "OR_UINT"
(*
	  | OUTPUTprim => String "OUTPUT"

	  | CONSprim {instance} => String "::"
*)
	  | ARRAY1prim  {instance} => String "array1"
	  | SUB1prim    {instance} => String "sub1")

	fun pp_prim3 prim = 
	  (case prim of
	     UPDATE1prim {instance} => String "update1"
(*	   | ARRAY2prim  {instance} => String "array2"
	   | SUB2prim    {instance} => String "sub2" *)
		 )
(*
	fun pp_prim4 prim = 
	  (case prim of
	     UPDATE2prim {instance} => String "update2")
*)
      in
	case prim of
	  PRIM0 p => pp_prim0 p
	| PRIM1 p => pp_prim1 p
	| PRIM2 p => pp_prim2 p
	| PRIM3 p => pp_prim3 p
(*	| PRIM4 p => pp_prim4 p *)
      end

    and pp_exp seen exp = 
      (case exp of
	 OVEREXP (c,_,exp) => (case oneshot_deref exp of
				 NONE => String "OVEREXP_NONE"
			       | (SOME e) => pp_exp seen e)
       | SCON scon => pp_scon scon
       | PRIM prim => pp_prim seen prim
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
       | SEQ elist => pp_list (pp_exp seen) elist ("(", ";",")", true)
       | LOC (con,exp) => pp_region "LOC(" ")" [pp_exp seen (!exp)]
       | RECORD [] => String "unit"
       | RECORD rbnds =>  let val (format,doer) = if (rbnds_is_tuple rbnds)
						    then (("(", ",",")", false), 
							  fn (RBND(l,e)) => (pp_exp seen e))
						  else
						    (("{", ",","}", false), 
						     fn (RBND(l,e)) => 
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
       | RAISE e =>  pp_region "RAISE(" ")" [pp_exp seen e]
       | LET (bs,e) => Vbox0 0 1 [String "LET ",
				  Vbox(separate (map (pp_bnd seen) bs) (Break0 0 0)),
				  Break,
				  String "IN  ",
				  pp_exp seen e,
				  Break,
				  String "END"]
       | NEW_STAMP con => pp_region "NEW_STAMP(" ")" [pp_con seen con]
       | EXN_INJECT (e1,e2) => pp_region "EXN_INJECT(" ")" [pp_exp seen e1, String ",", pp_exp seen e2]
       | REF (con,exp)  =>  pp_region "REF(" ")" 
			  [pp_con seen con, String ",", pp_exp seen exp]
       | GET (c,exp)  => pp_region "GET(" ")" [pp_exp seen exp]
       | SET (c,e1,e2) => pp_region "SET(" ")" [pp_exp seen e1, String ",", pp_exp seen e2]
       | ROLL (con,e) => pp_region "ROLL(" ")"
			  [pp_con seen con, pp_exp seen e]
       | UNROLL (con,e) => pp_region "UNROLL(" ")"
			  [pp_con seen con, String ",", pp_exp seen e]
       | INJ  (conlist, i, e) => pp_region "INJ(" ")"
			  [pp_list (pp_con seen) conlist ("[",", ","]",false), 
			   String ("," ^ (makestring i) ^ ","), pp_exp seen e]
       | PROJ (conlist,i,e) => pp_region "PROJ(" ")" 
			  [pp_list (pp_con seen) conlist ("[",", ","]",false),
			   String ((makestring i) ^ ","), 
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
       | SIGNAT_DATATYPE (dblist,tblist,sdecs) => 
	       HOVbox[String "SIG_DATATYPE(",
		      Break,
		      pp_list pp_db dblist ("DBs = ",", ","", true),
		      Break,
		      pp_list pp_tb tblist ("TBs = ",", ","", true),
		      Break,
		      pp_list (pp_sdec seen) sdecs ("sdecs = [",", ","]", true),
		      String ")"]
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
							       f, String " = "])::
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
    fun help' pp obj = (wrapper pp std_out obj; ())

    val pp_var' = help pp_var
    val pp_label'  = help pp_label
    val pp_con' = help (pp_con [])
    val pp_kind' = help pp_kind
    val pp_scon' = help pp_scon
    val pp_prim' = help (pp_prim [])
    val pp_mod' = help (pp_mod [])
    val pp_exp' = help (pp_exp [])
    val pp_context' = help (pp_context [])
    val pp_signat' = help (pp_signat [])

    val pp_list' = help pp_list
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
    val pp_scon = help' pp_scon
    val pp_prim = help' (pp_prim [])
    val pp_mod = help' (pp_mod [])
    val pp_exp = help' (pp_exp [])
    val pp_context = help' (pp_context [])
    val pp_signat = help' (pp_signat [])
    fun pp_list doer data = help' (pp_list' doer data)
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