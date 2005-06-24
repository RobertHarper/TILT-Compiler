(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dan Grossman                        *)
(*     June 1998, all rights reserved.                                *)
(*                                                                    *)
(* Ported to SML by Leaf Petersen                                     *)
(* November, 2003                                                     *)
(**********************************************************************)

(* talpp.ml
 *
 * Pretty printer for x86 tal language.
 *
 * TODO: 1. GAS support
 *       2. Movsx/movzx instructions
 *
 *)
structure Pptal :> PPTAL = 
  struct
    open Tal
    open Formatter 

    fun error s = Util.error "pp.sml" s
    val pp_instr_no = Stats.ff "TalNumberInstrs"

    type options = { 
		    kinds          : bool, 
		    cons           : bool, 
		    expand_abbrevs : bool,
		    expand_pr      : bool} 

    val std_options = {kinds          = true, 
		       cons           = true , 
		       expand_abbrevs = true,
		       expand_pr      = false}

      (* Utilities *)

    fun id_prn v = String(Name.label2string v)
    fun lbl_prn l = String(Name.label2string l)
      
    val pp_print_string = String 
    fun pp_print_int i = String (string_of_int i)
    fun pp_print_int32 i = String (string_of_int32 i)
    fun pp_print_break blanks indent = String (String.implode (Listops.copy (blanks,#" ")))
    fun pp_print_char c = String (String.str c)
    fun pp_open_hovbox i = HOVbox0 0 i 0
    fun pp_open_hvbox i = HVbox0 0 i 0
    fun pp_open_vbox i = Vbox0 i 0
    fun pp_open_hbox i = Hbox0 i
    fun pp_print_cut () = Newline ()
    val pp_print_newline = Newline

    fun sepi i s p l =
      let
	fun sepi i s p l = 
	  (case l 
	     of [] => []
	      | [x] => [p x]
	      | x::l =>
	       (p x :: pp_print_string s :: pp_print_break 0 i :: sepi i s p l))
      in pp_open_hovbox 0 (sepi i s p l)
      end

    fun sepb s p l =
      let
	fun sepb s p l = 
	  (case l 
	     of [] => []
	      | [x] => [p x]
	      | x::l =>
	       (p x :: pp_print_break 1 0 :: pp_print_string s :: sepb s p l))
      in pp_open_hovbox 0 (sepb s p l)
      end

    fun sep s p l = sepi 0 s p l
    fun sepc p l = sepi 0 "," p l
    fun sepci i p l = sepi i "," p l

      (* Misc *)
      
    fun print_scale s = 
      case s 
	of B1 => pp_print_char #"1"
	 | B2 => pp_print_char #"2"
	 | B4 => pp_print_char #"4"
	 | B8 => pp_print_char #"8"


    fun string_of_log l =
      case l 
	of Cadd => "++"
	 | Csub => "-"
	 | Cmuls => "*#"
	 | Cmulu => "*u"
	 | Cand => " && " (* extra space makes 'em more legible *)
	 | Cor => " | "
	 | Cimp => " =!> "
	 | Ciff => " iff "
	 | Cnot => "~"
	 | Clts => "<#"
	 | Cltu => "<"
	 | Cltes => "<=#"
	 | Clteu => "<="

    fun string_of_reg r =
      case r 
	of Eax => "a"
	 | Ebx => "b"
	 | Ecx => "c"
	 | Edx => "d"
	 | Esi => "si"
	 | Edi => "di"
	 | Ebp => "bp"
	 | Esp => "sp"
	 | Virt i => id_to_string i
	  
    fun print_reg_part opts r part =
      let
	fun Exx () = 
	  let 
	    val r = String.map Char.toUpper (string_of_reg r) 
	  in
	    (case part 
	       of RPe => String ("E"^r^"X")
		| RPx => String (r^"X")
		| RPh => String (r^"H")
		| RPl => String (r^"L"))
	  end
	fun Eother () = 
	  let 
	    val r = String.map Char.toUpper (string_of_reg r) 
	  in
	    (case part 
	       of RPe => String ("E"^r)
		| RPx => String r
		| RPh => error "Bad part reg"
		| RPl => error "Bad part reg")
	  end
      in
	case r 
	  of Eax => Exx()
	   | Ebx => Exx() 
	   | Ecx => Exx()
	   | Edx => Exx()
	   | Esi => Eother()
	   | Edi => Eother()
	   | Ebp => Eother()
	   | Esp => Eother()
	   | Virt i => error "No virtuals"
      end

    fun print_reg opts r = 
      let
	val s= 
	  case r 
	    of Eax => "EAX"
	     | Ebx => "EBX"
	     | Ecx => "ECX"
	     | Edx => "EDX"
	     | Esi => "ESI"
	     | Edi => "EDI"
	     | Ebp => "EBP"
	     | Esp => "ESP"
	     | Virt i => id_to_string i
      in pp_print_string s
      end

    (* print fpstack as part of register file. 
     * return true if comma is required before next register is printed 
     *)
    fun print_internal_fpstack opts fps need_comma =
      let 
	fun aux fps need_comma i =
	  let
	    fun pp_reg () = 
	      let
		val r = pp_print_string ("ST"^string_of_int i)
	      in 
		if need_comma then [pp_print_char #",",r] else [r]
	      end
	  in
	    if i < 8 then 
	      case fpstack_get_fpreg fps i 
		of FPfull => pp_reg () @ (aux fps true (i+1))
		 | FPempty => aux fps need_comma (i+1)
		 | FPany => pp_reg () @ pp_print_string ("?") :: aux fps true (i+1)
	    else []
	  end
      in
	if fpstack_isempty fps then (String "",need_comma)
	else (pp_open_hovbox 0 (aux fps need_comma 0), true)
      end

    (* print fpstack on its own *)
    fun print_fpstack opts fps =
      let
	val (fmt,need_comma) = 
	  print_internal_fpstack opts fps false
      in
	pp_open_hovbox 0 [
			  pp_print_char #"{",
			  fmt,
			  pp_print_char #"}"]
      end
  
      (* Kinds *)

    fun print_kind_a (k : kind) =
      case #rkind k
	of Kbyte s => pp_open_hovbox 0 [pp_print_char #"T", print_scale s]
	 | Ktype   => pp_print_char #"T"
	 | Kmemi i => String ("Tm "^(string_of_int32 i))
	 | Kmem => pp_print_string "Tm"
	 | Kstack => pp_print_string "Ts"
	 | Kint => pp_print_string "Sint"
	 | Kbool => pp_print_string "Tbool"
	 | Karrow (k1,k2) =>
	  pp_open_hovbox 0 [
		 print_kind_b true k1,
		 pp_print_string "-!>",
		 (case #rkind k2
		    of Karrow (_,_) => print_kind_a k2
		     | _ => print_kind_b false k2)
		    ]
	 | Kprod ks =>
	  pp_open_hovbox 0 [
		 pp_print_string "*[",
		 sepci 2 (print_kind_b false) ks,
		 pp_print_string "]"
		 ]
	 | Kname => pp_print_string "Tn"
	 | Kcap => pp_print_string "Tcap"
	 | Kms => pp_print_string "Tms"
	 (* LX *)
	 | Ksum ks => 
	  pp_open_hovbox 0 [
		 pp_print_string "+[",
		 sepci 2 (print_kind_b false) ks,
		 pp_print_string "]"
		 ]
	 | Kvar id => id_prn id
	 | Kmu ([(i1,k)],i) => 
	  pp_open_hovbox 0 [
		 pp_print_string "mu ",
		 id_prn i,
		 pp_print_char #".",
		 print_kind_a k
		 ]
	 | Kmu (ks,i) => 
	  pp_open_hovbox 0 [
		 pp_print_string "Kmu ",
		 id_prn i,
		 pp_print_string " with [",
		 pp_open_hovbox 0 (List.foldr (fn ((i,k),fs) => (id_prn i :: pp_print_string " " :: fs)) [] ks),
		 pp_print_string "]"
		 ]
    (* end LX *)

    and print_kind_b p k =
      case #rkind k
	of Karrow (_,_) =>
	  pp_open_hovbox 0 [
		 if p then pp_print_string "(" else String "",
		 pp_open_hovbox 0 [print_kind_a k],
		 if p then pp_print_string ")" else String ""
		   ]
	 | _ => pp_open_hovbox 0 [print_kind_a k]
    and print_kind (opts : options) k =
      if #kinds opts then print_kind_b false k else String ""
    and print_ckind (opts : options) k =
      if #kinds opts then 
	pp_open_hovbox 0 [pp_print_string ":",print_kind_b false k]
      else 
	String ""

    (* Constructors *)

    fun print_primcon pc =
      case pc 
	of PCbytes B1 => pp_print_string "B1"
	 | PCbytes B2 => pp_print_string "B2"
	 | PCbytes B4 => pp_print_string "B4"
	 | PCbytes B8 => pp_print_string "B8"
	 | PCfloat32 => pp_print_string "F4"
	 | PCfloat64 => pp_print_string "F8"
	 | PCjunk i => pp_print_string ("junk "^ (string_of_int32 i))
	 | PCjunkbytes B1  => pp_print_string "junk1"
	 | PCjunkbytes B2  => pp_print_string "junk2"
	 | PCjunkbytes B4  => pp_print_string "junk4"
	 | PCjunkbytes B8  => pp_print_string "junk8"
	 | PCint i  => pp_print_string (string_of_int32 i)
	 | PCtrue   => pp_print_string "true"
	 | PCfalse  => pp_print_string "false"


    fun print_variance v =
      case v 
	of Read => pp_print_string "^r"
	 | Write => pp_print_string "^w"
	 | ReadWrite => pp_print_string "^rw"

    fun log_prec l =
      case l 
	of Cadd => 10
	 | Csub => 10
	 | Cmuls => 11
	 | Cmulu => 11
	 | Cand => 6
	 | Cor => 7
	 | Cimp => 5
	 | Ciff => 5
	 | Cnot => 12
	 | Clts => 9
	 | Cltu => 9
	 | Cltes => 9
	 | Clteu => 9

    fun prec c =
      case c 
	of Cvar _ => 100
	 | Clam (_,_,_) => 1
	 | Capp (_,_) => 2
	 | Ctuple _ => 100
	 | Cproj (_,_) => 3
	 | Clab _ => 100
	 | Cprim _ => 100
	 | Crec _ => 100
	 | Cforall (_,_,_) => 4
	 | Cexist (_,_,_,_) => 4
	 | Ccode _ => 4
	 | Cmsjoin (_,_) => 5
	 | Cms _ => 100
	 | Chptr (_,_,_) => 4
	 | Cfield (_,_) => 7
	 | Cprod _ => 100
	 | Csum _ => 100
	 | Carray _ => 100
	 | Csing _ => 100
	 | Csptr _ => 2
	 | Cempty => 100
	 | Ccons (_,_) => 6
	 | Cappend (_,_) => 5
	 | Cjoin _ => 100
	 | Ccap _ => 100
	 | Cname _ => 100
	 | Ctagof _ => 100
	 (* LX *)
	 | Cinj(_,_,_) => 3
	 | Ccase(_,_,_) => 100
	 | Cfold (_,_) => 100
	 | Cvoid _ => 100
	 | Cpr (_,_) => 100
	 (* end LX *)

    fun print_con_a (opts : options) (inprec : int) (con : con) =
      case (!(#abbrev con), #expand_abbrevs opts) 
	of (SOME v,false) => id_prn v
	 | _ =>  
	  let 
	    val c = !(#rcon con )
	    val myprec = prec c 
	    val lp =
	      if inprec >= myprec then pp_print_string "(" else String ""
	    val rp = if inprec >= myprec then pp_print_string ")"  else String ""
	  in
	    case c 
	      of Cvar v => id_prn v
	       | Clam (v,k,c) => 
		pp_open_hovbox 0 [
		       lp,
		       pp_print_string "fn ",
		       print_con_b opts myprec v k c,
		       rp
		       ]
	       | Capp (c1,c2) =>
		pp_open_hovbox 0 [lp,
				  print_con_a opts myprec c1,
				  pp_print_break 1 2,
				  print_con_a opts (myprec+1) c2,
				  rp
				  ]
	       | Ctuple cs =>
		pp_open_hovbox 0 
		[
		 pp_print_char #"[",
		 sepci 1 (print_con_a opts 0) cs,
		 pp_print_char #"]"
		 ]
	       | Cproj (i,c) =>
		pp_open_hovbox 0 
		[lp,
		 print_con_a opts myprec c,
		 pp_print_string ("."^(string_of_int32 i)),
		 rp
		 ]
	       | Clab l => pp_open_hovbox 0 [pp_print_char #"`",lbl_prn l]
	       | Cprim pc => print_primcon pc
	       | Crec vkcs =>
		let 
		  fun f (v,k,c) =
		    pp_open_hovbox 0
		    [id_prn v,
		     print_ckind opts k,
		     pp_print_char #".",
		     pp_print_break 0 2, 
		     print_con_a opts 0 c
		     ]
		in
		  pp_open_hovbox 0 
		  [pp_print_string "rec(",
		   sepc f vkcs,
		   pp_print_string ")"
		   ]
		end
	       | Cforall (v,k,c) =>
		pp_open_hovbox 0
		[lp,
		 pp_print_string "All[",
		 print_con_c opts myprec v k c,
		 rp
		 ]
	       | Cexist (v,k,c1,c2) =>
		pp_open_hovbox 0
		[lp,
		 pp_print_string "Exist[",
		 print_con_d opts myprec v k c1 c2,
		 rp
		 ]
	       | Ccode c => 
		pp_open_hovbox 0
		[
		 lp,
		 pp_print_string "code",
		 pp_print_break 1 2,
		 print_con_a opts (myprec+1) c,
		 rp
		 ]
	       | Cms ms => print_machine_state opts ms
	       | Cmsjoin(c1,c2) => 
		pp_open_hovbox 0
		[
		 lp,
		 print_con_a opts (myprec+1) c1,
		 pp_print_char #"&",
		 pp_print_cut (),
		 print_con_a opts myprec c2,
		 rp
		 ]
	       | Chptr (is,co,tco) =>
		let 
		  fun print_tags is =
		    sepc (fn i => pp_print_string (string_of_int32 i)) is 
		in
		  pp_open_hovbox 0
		  (
		   [lp,
		   pp_print_string "^"]
		   @
		   (case tco 
		      of NONE =>
			if is<>[] then 
			  let val (opin,close) = if co=NONE then ("[","]") else ("(",")")
			  in [pp_print_string ("T"^opin),
			      print_tags is,
			      pp_print_string close]
			  end
			else []
		    | SOME (c,v) =>
			  [pp_print_string "T",
			   print_variance v,
			   pp_print_string "(",
			   pp_open_hovbox 0 (print_con_a opts 0 c
					     ::( if is<>[] then [String ",",pp_print_break 0 0,print_tags is]
						 else [])),
			   pp_print_string ")"])
		      @
		      [
		       (case co 
			  of NONE => pp_print_string ""
			   | SOME c => print_con_a opts myprec c),
			  rp
			  ])
		end
	       | Cfield (c,v) =>
		pp_open_hovbox 0 
		[lp, 
		 print_con_a  opts myprec c,
		 print_variance v, 
		 rp]
	       | Cprod cs =>
		pp_open_hovbox 0 
		[
		 pp_print_string "*[",
		 sepc (print_con_a opts 0) cs,
		 pp_print_char #"]"
		 ]
	       | Csum cs =>
		pp_open_hovbox 0
		[
		 pp_print_string "+[",
		 sepc (print_con_a opts 0) cs,
		 pp_print_char #"]"
		 ]
	       | Carray(cl,ce) =>
		pp_open_hovbox 0
		[pp_print_string "array(",
		 print_con_a opts 0 cl, pp_print_char #",", pp_print_break 0 2,
		 print_con_a opts 0 ce, pp_print_string ")"]
	       | Csing c =>
		pp_open_hovbox 0 
		[pp_print_string "S(", print_con_a opts 0 c, pp_print_string ")"]
	       | Csptr c =>
		pp_open_hovbox 0 
		[
		 lp, pp_print_string "sptr", pp_print_break 1 2,
		 print_con_a opts (myprec+1) c, rp]
	       | Cempty => pp_print_string "se"
	       | Ccons (c1, c2) =>
		pp_open_hovbox 0 
		[
		 lp,
		 print_ccons opts myprec con,
		 rp
		 ]
	       | Cappend (c1,c2) => 
		pp_open_hovbox 0 [lp,
				  print_con_a opts (myprec+1) c1, pp_print_string "#",
				  pp_print_break 0 0,
				  print_con_a opts myprec c2,
				  rp ]
	       | Cname c => 
		pp_open_hovbox 0 [
				  pp_print_string "Nm(", print_con_a opts 0 c, pp_print_string ")" ]
	       | Ctagof c =>
		pp_open_hovbox 0 
		[
		 pp_print_string "Tagof(", print_con_a opts 0 c,
		 pp_print_string ")"
		 ]
	       | Cjoin cs =>
		pp_open_hovbox 0 
		[
		 pp_print_string "&[", 
		 sepc (print_con_a opts 0) cs,
		 pp_print_char #"]"
		 ]
	       | Ccap d =>
		let 
		  val entries = Name.LabelMap.foldli (fn (x,p,es) => (x,p)::es) [] d 
		in
		  pp_open_hovbox 0 
		  [
		   pp_print_string "cap[",
		   sepc (fn (x,(ai,c)) => 
			 (pp_open_hovbox 0 
			  [id_prn x,
			   pp_print_char (case ai 
					    of MayAlias => #":"
					     |	Unique => #"!"),
			   print_con_a opts 0 c])) entries,
		   pp_print_char #"]"
		   ]
		end
	       (* LX *)
	       | Cinj(i,c,k) =>
		pp_open_hovbox 0
		[
		 pp_open_hovbox 0 [lp, pp_print_string "inj",  pp_print_break 1 2,
				   pp_print_int32 i, pp_print_break 1 2,
				   pp_open_hovbox 0 [lp, print_con_a opts myprec c,  rp], pp_print_break 0 2,
				   pp_print_char #"[",  print_kind opts k, pp_print_char #"]", 
				   rp]
		 ]
	       | Ccase(c,a,cs) => 
		pp_open_hovbox 0 
		[
		 pp_print_string "case (", print_con_a opts 0 c, 
		 pp_print_string ")", id_prn a,
		 pp_print_string "[",
		 sepc (print_con_a opts 0) cs,
		 pp_print_char #"]"
		 ]
	       | Cfold(k,c) =>  
		pp_open_hovbox 0
		[
		 pp_print_string "roll[",
		 print_kind opts k, pp_print_char #"]",
		 pp_open_hovbox 0 [lp,
				   print_con_a opts (myprec+1) c,
				   rp]
		 ]
	       | Cpr(i,l) =>
		let 
		  fun find l = 
		    case l 
		      of ((hd as (_,_,_,f,_,_))::rest) =>
			if Name.eq_label (f,i) 
			  then hd else find rest
		       | [] => error "ill-formed Cpr in talpp.ml"

		  val (j,a,k,f,k',c) = find l 
		in
		  if (not (#expand_pr opts)) then 
		    id_prn f 
		  else 
		    pp_open_hovbox 0 [lp, id_prn f,
				      pp_print_string " : ", id_prn j , 
				      pp_print_string " -!>", print_kind opts k', pp_print_string " = fn ",
				      id_prn a, pp_print_string ":", print_kind opts k ,  pp_print_string ".",
				      print_con_a opts (myprec+1) c, 
				      rp]
		end 
	  	  
	       (* just print out the id for now... maybe later we'll 
		print more information *)
	       | Cvoid k => 
		pp_open_hovbox 0 [
				  pp_print_string "void[",
				  print_kind opts k,
				  pp_print_string "]"
				  ]
	       (* end LX *)
	  end
    (* Print nary con lambda *)
    and print_con_b opts myprec v k con =
      let
	fun loop v k con acc = 
	  let
	    val acc = id_prn v :: acc
	    val acc = print_ckind opts k :: acc
	    val acc = pp_print_break 1 0 :: acc
	  in		
	    case (!(#abbrev con), #expand_abbrevs opts) 
	      of (SOME v,false) => pp_open_hovbox 0 (rev (id_prn v::acc))
	       | _ =>
		let 
		  val c = !(#rcon con )
		in
		  (case c 
		     of Clam (v,k,c) => loop v k c acc
		      | _ => 
		       pp_open_hovbox 0 [pp_open_hovbox 0 (rev acc),
					 pp_print_char #".",
					 pp_print_break 1 2,
					 print_con_a opts myprec con])
		end
	  end
      in loop v k con []
      end
    (* Print nary forall *)
    and print_con_c opts myprec v k con =
      let
	fun loop v k con acc = 
	  let
	    val acc = id_prn v :: acc
	    val acc = print_ckind opts k :: acc
	  in
	    case (!(#abbrev con), #expand_abbrevs opts) 
	      of (SOME v,false) => pp_open_hovbox 0 (rev (id_prn v::acc))
	       | _ =>
		let 
		  val c = !(#rcon con)
		in
		  case c 
		    of Cforall (v,k,c) => 
		      let
			val acc = pp_print_break 1 0 :: acc
		      in loop v k c acc
		      end
		     | _ =>
		      pp_open_hovbox 0
		      [
		       pp_open_hovbox 0 (rev acc),
		       pp_print_string "]. ",
		       print_con_a opts myprec con]
		end
	  end
      in loop v k con []
      end
    (* Print nary existential *)
    and print_con_d opts myprec v k c1 c2 =
      let 
	fun loop v k c1 c2 acc = 
	  let
	    val rc1 = !(#rcon c1)
	    val rc2 = !(#rcon c2)
	    val acc = id_prn v :: acc
	    val acc = print_ckind opts k :: acc
	  in
	    case (rc1,rc2)
	      of (Cprim PCtrue,Cexist (v,k,c1,c2)) => 
		let
		  val acc = pp_print_break 1 0:: acc
		in loop v k c1 c2 acc
		end
	       | (Cprim PCtrue,_) =>
		pp_open_hovbox 0 
		[
		 pp_open_hovbox 0 (rev acc),
		 pp_print_string "]. ",
		 print_con_a opts myprec c2
		 ]
	       | _ =>
		let
		  val acc = pp_print_string "|" :: acc
		  val acc =  pp_print_break 0 2 :: acc
		  val acc = print_con_a opts 0 c1 :: acc
		in pp_open_hovbox 0 
		  [
		   pp_open_hovbox 0 (rev acc),
		   pp_print_string "]. ",
		   print_con_a opts myprec c2
		   ]
		end
	  end
      in loop v k c1 c2 []
      end
    and print_ccons opts myprec c =
      case !(#rcon c)
	of Ccons(c1,c2) => 
	  pp_open_hovbox 0 
	  [print_con_a opts (myprec+1) c1,
	   pp_print_string "::",
	   pp_print_break 0 0,
	   print_ccons opts myprec c2
	   ]
	 | _ => print_con_a opts myprec c

    and print_machine_state opts ms =
      let
	val cap = ms_get_cap ms 

	val (fmt1,need_comma) =
	  if cap <> cempty_cap then
	    ([pp_print_string "cap: ", print_con_a opts 0 cap],
	     true)
	  else 
	    ([],false) 
	    (* Floating point *)
	val (fmt2,need_comma) = 
	  print_internal_fpstack opts (ms_get_fpstack ms) need_comma 

	val s =
	  case ms_get_cc ms 
	    of CCnoinfo => 
	      if need_comma then ","
	      else ""
	     | _ => error "Only CCnoinfo supported"

	fun f (r,c) =
	  pp_open_hovbox 0 
	  [
	   print_reg opts r, pp_print_string ": ", print_con_a opts 0 c
	   ] 
	  val rlist = ms_fold_reg (fn r => fn c => fn rl => (r,c)::rl) ms [] 
      in
	pp_open_hovbox 0
	[
	 pp_print_string "{",
	 pp_open_hovbox 0 
	 (fmt1 @ fmt2 ::
	  [if rlist <> [] then pp_print_string s else pp_print_string "",
	     sepc f rlist
	     ]),
	 pp_print_string "}"
	 ]
      end
    
    fun print_con opts c =
      if #cons opts then
	(case (!(#abbrev c), #expand_abbrevs opts) 
	   of (SOME v,false) => (id_prn v)
	    | _ => print_con_a opts 0 c)
      else pp_print_string ""
		      
    and print_ccon opts c =
      if #cons opts then 
	pp_open_hovbox 0 
	[ pp_print_string ":", print_con_a opts 0 c]
      else pp_print_string ""

    (*** Code ***)
    fun print_annotate opts a =
      if #cons opts then
	case a 
	  of Con c => print_con  opts c
	   | AReg r => print_reg  opts r
	   | StackTail (r,i) => 
	    pp_open_hovbox 0 
	    [print_reg opts r,
	     pp_print_char #" ",
	     pp_print_int i]
	   | StackSlice(r,i,j,c) => 
	    pp_open_hovbox 0 
	    [
	     print_reg opts r,
	     pp_print_char #" ", pp_print_int i,
	     pp_print_char #" ", pp_print_int j,
	     pp_print_char #" ", print_con opts c
	     ]
      else String ""

    (* 'a Coerce *)
    fun strip_tapps clist cons =
      case clist 
	of (Tapp con)::clist => strip_tapps clist (con::cons)
	 | _ => (clist,cons)

    fun print_coerce f opts (raw,clist) =
      case clist 
	of [] => f opts raw
	 | (Pack (c1,c2))::clist =>
	  (* c1 is hidden type, c2 is existential *)
	  pp_open_hovbox 0
	  [
	   pp_print_string "pack(<", print_con opts c1,
	   pp_print_string ">,", pp_print_break 0 2,
	   print_coerce f opts (raw,clist),
	   pp_print_char #",", pp_print_break 0 2,
	   pp_print_char #"<", print_con opts c2, pp_print_string ">)"
	   ]
	 | (Tapp a)::clist => (* changed by Dan for anotations *)
	  let 
	    val (clist,cons) = strip_tapps clist [a] 
	  in
	    pp_open_hovbox 0 
	    [
	     pp_print_string "tapp(",
	     print_coerce f opts (raw,clist),
	     pp_print_char #",", pp_print_break 0 2, pp_print_char #"<",
	     pp_open_hovbox 0 [ sepc (print_annotate opts) cons],
	     pp_print_string ">)"
	     ]
	  end
	 | (Roll t)::clist =>
	    pp_open_hovbox 0
	    [
	     pp_print_string "roll(<", print_con opts t,
	     pp_print_string ">,", pp_print_break 0 2,
	     print_coerce f opts (raw,clist), pp_print_string ")"
	     ]
	 | (Unroll)::clist =>
	    pp_open_hovbox 0
	    [
	     pp_print_string "unroll(", print_coerce f opts (raw,clist),
	     pp_print_string ")"
	     ]
	 | (Tosum t)::clist => 
	    pp_open_hovbox 0
	    [
	     pp_print_string "sum(", pp_print_char #"<", print_con opts t,
	     pp_print_string ">,", pp_print_break 0 2,
	     print_coerce f opts (raw,clist), pp_print_string ")"
	     ]
	 | (RollTosum t)::clist => 
	    pp_open_hovbox 0
	    [
	     pp_print_string "rollsum(", pp_print_char #"<", print_con opts t,
	     pp_print_string ">,", pp_print_break 0 2,
	     print_coerce f opts (raw,clist), pp_print_string ")"
	     ]
	 | (Fromsum)::clist =>
	    pp_open_hovbox 0
	    [
	     pp_print_string "rec(", print_coerce f opts (raw,clist),
	     pp_print_string ")"
	     ]
	 | (Toarray (off,d,c))::clist =>
	    pp_open_hvbox 0
	    [pp_print_string "array(",
	     pp_print_string (string_of_int32 off), pp_print_string ",",
	     pp_print_break 0 2,
	     pp_print_string (string_of_int d), pp_print_string ",",
	     pp_print_break 0 2,
	     pp_print_string "<",
	     print_con opts c, 
	     pp_print_string ">,",
	     pp_print_break 0 2,
	     print_coerce f opts (raw,clist),
	     pp_print_string ")"
	     ]
	 | (Slot (i,sz))::clist =>
	    pp_open_hvbox 0 
	    [
	     pp_print_string "slot(",
	     pp_print_string (string_of_int32 i), pp_print_string ",",
	     pp_print_break 0 2,
	     pp_print_string (string_of_int32 sz), pp_print_string ",",
	     pp_print_break 0 2,
	     print_coerce f opts (raw,clist), 
	     pp_print_string ")"
	     ]
	 | (Subsume t)::clist =>
	    pp_open_hovbox 0
	    [
	     pp_print_string "subsume(", pp_print_char #"<", print_con opts t,
	     pp_print_string ">,", pp_print_break 0 2,
	     print_coerce f opts (raw,clist), pp_print_string ")"
	     ]
	 | Forgetname::clist =>
	    pp_open_hovbox 0 
	    [
	     pp_print_string "forgetname(", print_coerce f opts (raw,clist),
	     pp_print_string ")"
	     ]
	 | Prove::clist =>
	    pp_open_hovbox 0 
	    [
	     pp_print_string "prove(", print_coerce f opts (raw,clist),
	     pp_print_string ")"
	     ]

    fun print_label_coerce opts lc = print_coerce (fn _ => fn l => lbl_prn l) opts lc

    fun print_reg_coerce opts rc = print_coerce print_reg opts rc

    fun print_genop opts gop =
      let
	fun print_opt opt =
	  case opt 
	    of NONE => []
	     | SOME (s,r') => 
	      pp_print_char #"+" ::
	      (if not (s = B1) then [print_scale s, pp_print_char #"*", print_reg opts r'] 
	       else [print_reg opts r'])
      in
	case gop 
	  of Immed i => pp_print_string (string_of_int32 i)
	   | Reg r => print_reg opts r
	   | Addr l => lbl_prn l
	   | Prjr (r,i,opt) =>
	    pp_open_hovbox 0 
	    ([
	     pp_print_char #"[", print_reg_coerce opts r
	     ]
	    @
	    print_opt opt
	    @
	    [
	     (if not (i = 0w0) then (pp_print_string ("+"^ (string_of_int32 i))) else pp_print_string ""),
	     pp_print_char #"]"
	     ])
	   | Prjl (l,i,opt) =>
	    pp_open_hovbox 0 
	    ([
	     pp_print_char #"[", print_label_coerce opts l
	     ]
	    @
	    print_opt opt
	    @
	    [
	     (if not (i = 0w0) then (pp_print_string ("+"^ (string_of_int32 i))) else pp_print_string ""),
	     pp_print_char #"]"
	     ])
      end
    fun print_genop_part opts gop reg_part =
      case gop 
	of Reg r => print_reg_part opts r reg_part	
	 | _ => print_genop opts gop

    fun print_genop_coerce opts cgop = print_coerce print_genop opts cgop

    (* MASM gets projections of code labels wrong and also needs size information
     * in some instances.  Thus the phrase DWORD PTR must be inserted in certain
     * circumstances.
     *
     * print_unary_op, print_unary_op_coerce, print_anop, print_anop_coerce,
     * print_binop, and print_binop2 do these insertions
     *)

    fun gop_add_dptr gop = 
      (case gop 
	 of Prjr (_,_,_) => pp_print_string "DWORD PTR "
	  | Prjl (_,_,_) => pp_print_string "DWORD PTR "
	  | _ => pp_print_string "")

    fun gopl_add_dptr gop = 
      (case gop 
	 of Prjl (_,_,_) => pp_print_string "DWORD PTR "
	  | _ => pp_print_string "")

    fun print_unary_op opts gop =
      pp_open_hovbox 0 
      [
       gop_add_dptr gop,
       print_genop opts gop
       ]

    fun print_unary_op_coerce opts (cgop as (gop,_)) =
      pp_open_hovbox 0 
      [
       gop_add_dptr gop,
       print_genop_coerce opts cgop
       ]

    fun print_anop opts gop =
      pp_open_hovbox 0 
      [
       gopl_add_dptr gop,
       print_genop opts gop
       ]

    fun print_anop_coerce opts (cgop as (gop,_)) =
      pp_open_hovbox 0 
      [
       gopl_add_dptr gop,
       print_genop_coerce opts cgop
       ]

    fun add_ptrs o1 o2 = 
      let
	fun add_ptr o1 o2 = 
	  (case o1 
	     of Prjl(_,_,_) => true
	      | Prjr(_,_,_) => (case o2 of Immed _ => true | _ => false)
	      | _ => false)
      in (add_ptr o1 o2,add_ptr o2 o1)
      end
    fun print_binop opts o1 o2 =
      let
	val (ptr1,ptr2) = add_ptrs o1 o2
      in
	pp_open_hovbox 0 
	[
	 if ptr1 then pp_print_string "DWORD PTR " else pp_print_string "",
	 print_genop opts o1, pp_print_char #",",
	 if ptr2 then pp_print_string "DWORD PTR " else pp_print_string "",
	 print_genop opts o2
	 ]
      end
    fun print_binop2 opts o1 (co2 as (o2,_)) =
      let 
	val (ptr1,ptr2) = add_ptrs o1 o2
      in
	pp_open_hovbox 0 
	[
	 if ptr1 then pp_print_string "DWORD PTR " else pp_print_string "",
	 print_genop opts o1, pp_print_char #",",
	 if ptr2 then pp_print_string "DWORD PTR " else pp_print_string "",
	 print_genop_coerce opts co2
	 ]
      end 
    fun print_binop3 opts (co1 as (o1,_)) (co2 as (o2,_)) = 
      let
	val (ptr1,ptr2) = add_ptrs o1 o2
      in
	pp_open_hovbox 0 
	[
	 if ptr1 then pp_print_string "DWORD PTR " else pp_print_string "",
	 print_genop_coerce opts co1, pp_print_char #",",
	 if ptr2 then pp_print_string "DWORD PTR " else pp_print_string "",
	 print_genop_coerce opts co2
	 ]
      end

   fun print_binop_part opts o1 part1 o2 part2 =
     let
       val (ptr1,ptr2) = add_ptrs o1 o2
       fun print_part ptr part oper = 
	 if ptr then
	   (case part
	      of RPe => [pp_print_string "DWORD PTR ", print_genop opts oper]
	       | RPl => [pp_print_string "BYTE PTR ",  print_genop opts oper]
	       | _   => error "memory half words unimplemented")
	 else []
     in
	pp_open_hovbox 0 
	(
	 (print_part ptr1 part1 o1) @
	 [ print_genop_part opts o1 part1, pp_print_char #","] @
	 (print_part ptr2 part2 o2) @
	 [print_genop_part opts o2 part2]
	 )
     end

    fun print_cc opts cc =
      let 
	val s =
	  case cc 
	    of Above => "A" | AboveEq => "AE" | Below => "B" | BelowEq => "BE"
	     | Eq => "E" | Greater => "G" | GreaterEq => "GE" | Less => "L"
	     | LessEq => "LE" | NotEq => "NE" | NotOverflow => "NO" | NotSign => "NS"
	     | Overflow => "O" | ParityEven => "PE" | ParityOdd => "PO" | Sign => "S"
      in pp_print_string s
      end

    (* Floating Point *)

    fun print_fpreg i =
      if i = 0 then pp_print_string "ST"
      else pp_print_string ("ST("^(string_of_int i)^")")

    fun print_fpregs b i =
      if b then pp_open_hovbox 0 [print_fpreg 0, pp_print_string ",", print_fpreg i]
      else pp_open_hovbox 0 [print_fpreg i, pp_print_string ",", print_fpreg 0]

    fun print_fpmem opts scale g =
      case (scale,g) 
	of (B2,Reg Eax) => pp_print_string "AX"
	 | _ =>
	  pp_open_hovbox 0 
	  [
	   (case scale 
	      of B2 => pp_print_string "WORD PTR "
	       | B4 => pp_print_string "DWORD PTR "
	       | B8 => pp_print_string "QWORD PTR "
	       | _ => error "print_fpmem: bad scale"),
           print_genop opts g
	   ]

    fun implicit_st oper =
      case oper 
	of Fcom => true
	 | Fcomp => true
	 | Fucom => true
	 | Fucomp => true
	 | _ => false

    fun print_fpargs opts implicit_st args =
      case args 
	of FPstack i => print_fpreg i
	 | FPstack2 (b,i) => 
	  if implicit_st then print_fpreg i
	  else print_fpregs b i
	 | FPgenop (s,g) => print_fpmem opts s g
	    
    fun print_fpnoargs opts oper =
      let 
	val s =
	  case oper 
	    of F2xm1 => "F2XM1"
	     | Fabs => "FABS"
	     | Fchs => "FCHS"
	     | Fclex => "FCLEX"
	     | Fnclex => "FNCLEX"
	     | Fcompp => "FCOMPP"
	     | Fucompp => "FUCOMPP"
	     | Fcos => "FCOS"
	     | Fdecstp => "FDECSTP"
	     | Fincstp => "FINCSTP"
	     | Finit => "FINIT"
	     | Fninit  => "FNINIT"
	     | Fld1 => "FLD1"
	     | Fldz => "FLDZ"
	     | Fldpi => "FLDPI"
	     | Fldl2e => "FLDL2E"
	     | Fldl2t => "FLDL2T"
	     | Fldlg2 => "FLDLG2"
	     | Fldln2 => "FLDLN2"
	     | Fnop => "FNOP"
	     | Fpatan => "FPATAN"
	     | Fprem => "FPREM"
	     | Fprem1 => "FPREM1"
	     | Fptan => "FPTAN"
	     | Frndint => "FRNDINT"
	     | Fscale => "FSCALE"
	     | Fsin => "FSIN"
	     | Fsincos => "FSINCOS"
	     | Fsqrt => "FSQRT"
	     | Ftst => "FTST"
	     | Fwait => "FWAIT"
	     | Fxam => "FXAM"
	     | Fxtract => "FXTRACT"
	     | Fyl2x => "FYL2X"
	     | Fyl2xp1 => "FYL2XP1"
      in pp_print_string s
      end

    fun print_fpsomeargs opts oper =
      let 
	val s = 
	  case oper 
	    of Fadd => "FADD"
	     | Fcom => "FCOM"
	     | Fcomp => "FCOMP"
	     | Fdiv => "FDIV"
	     | Fdivr => "FDIVR"
	     | Fmul => "FMUL"
	     | Fsub => "FSUB"
	     | Fsubr => "FSUBR"
	     | Fucom => "FUCOM"
	     | Fxch => "FXCH"
	     | Fiadd => "FIADD"
	     | Ficom => "FICOM"
	     | Ficomp => "FICOMP"
	     | Fidiv => "FIDIV"
	     | Fidivr => "FIDIVR"
	     | Fimul => "FIMUL"
	     | Fisub => "FISUB"
	     | Fisubr => "FISUBR"
	     | Faddp => "FADDP"
	     | Fdivp => "FDIVP"
	     | Fdivrp => "FDIVRP"
	     | Fmulp => "FMULP"
	     | Fsubp => "FSUBP"
	     | Fsubrp => "FSUBRP"
	     | Fucomp => "FUCOMP"
	     | Fst => "FST"
	     | Fstp => "FSTP"
	     | Fist => "FIST"
	     | Fistp => "FISTP"
	     | Fld => "FLD"
	     | Fild => "FILD"
	     | Ffree => "FFREE"
	     | Fcomi => "FCOMI"
	     | Fcomip => "FCOMIP"
	     | Fucomi => "FUCOMI"
	     | Fucomip => "FUCOMIP"
	     | Fstsw => "FSTSW"
	     | Fnstsw => "FNSTSW"
      in pp_print_string s
      end
	  
    (* Instructions *)
    fun print_arithbin opts x =
      let 
	val s = 
	  case x 
	    of Adc => "ADC" | Add => "ADD" | And => "AND" | Imul2 => "IMUL"
	     | Or => "OR" | Sbb => "SBB" | Sub => "SUB" | Xor => "XOR"
      in pp_print_string s
      end


    fun print_arithun opts x =
      let 
	val s = 
	  case x 
	    of Dec => "DEC" | Inc => "INC" | Neg => "NEG" | Not => "NOT"
      in pp_print_string s
      end

    fun print_arithmd opts x =
      let 
	val s = 
	  case x 
	    of Div => "DIV" | Idiv => "IDIV" | Imul1 => "IMUL" | Mul => "MUL"
      in pp_print_string s
      end

    fun print_arithsr opts x =
      let 
	val s = 
	  case x 
	    of Rcl => "RCL" | Rcr => "RCR" | Rol => "ROL" | Ror => "ROR" | Sal => "SAL"
	     | Sar => "SAR" | Shl => "SHL" | Shr => "SHR"
      in pp_print_string s
      end

    fun print_conv opts c =
      case c 
	of Cbw => pp_print_string "\tCBW"
	 | Cdq => pp_print_string "\tCDQ"
	 | Cwd => pp_print_string "\tCWD"
	 | Cwde => pp_print_string "\tCWDE"

    fun scale_to_reg_part i =
      if i = 0w1 then RPl
      else if i = 0w2 then RPx
      else if i = 0w4 then RPe
      else error "Talpp.scale_to_reg_part"

    fun print_array_arg opts genop =
      let 
	fun print_opt opt = 
	  case opt 
	    of NONE => pp_print_string ""
	     | SOME (s,r) => 
	      pp_open_hovbox 0 
	      [
	       pp_print_string "+", 
	       print_scale s,
	       pp_print_string "*",
	       print_reg opts r
	       ]
      in
	case genop 
	  of Prjr (rc,n,opt) =>
	    pp_open_hovbox 0 
	    [
	     print_reg_coerce opts rc, 
	     print_opt opt,
	     pp_print_string  ("+" ^ (string_of_int32 n))
	     ]
	   | Prjl (lc,n,opt) =>
	    pp_open_hovbox 0 
	    [
	     print_label_coerce opts lc, 
	     print_opt opt,
	     pp_print_string  ("+" ^ (string_of_int32 n))
	     ]
	   | _ => error "Talpp.print_array_arg"
      end
    

    fun print_mallocarg opts ma =
      let 
	fun aux ma =
	  case ma 
	    of Mprod mas => 
	      pp_open_hovbox 0 
	      [
	       pp_print_char #"[",
	       sepc aux mas,
	       pp_print_char #"]"
	       ]
	     | Mbytes s => 
	      pp_open_hovbox 0 
	      [pp_print_char #":", print_scale s]
	     | Mbytearray (scale,size) =>
	      pp_open_hovbox 0
	      [
	       pp_print_string "array(",
	       pp_print_string (string_of_int32 size),
	       pp_print_string ",B",
	       print_scale scale,
	       pp_print_string ")" 
	       ]
      in
	pp_open_hovbox 0 
	[
	 pp_print_char #"<", aux ma, pp_print_char #">"
	 ]
      end


    fun print_instruction opts i =
      let 
	val pp_string = pp_print_string
	val pp_char = pp_print_char 
	fun pp_iopt iopt = 
	  (case iopt 
	     of NONE => [pp_string "" ]
	      | SOME instructions =>
	       [
		pp_string "\tvirtual<",
		sepi 2 " " (print_instruction opts) instructions,
		pp_string ">"
		])

	val fmtlist = 
	  (case i 
	     of ArithBin (x,o1,o2) =>
	       [
		pp_char #"\t", print_arithbin opts x, pp_char #"\t",
		print_binop opts o1 o2
		]
	      | ArithUn (x,oper) =>
	       [
		pp_char #"\t", print_arithun opts x, pp_char #"\t",
		print_unary_op opts oper
		]
	      | ArithMD (x,oper) =>
	       [
		pp_char #"\t", print_arithmd opts x, pp_char #"\t",
		print_unary_op opts oper
		]
	      | ArithSR (x,oper,io) =>
	       [
		pp_char #"\t", print_arithsr opts x, pp_char #"\t",
		print_unary_op opts oper, pp_char #",",
		(case io 
		   of NONE => pp_string "CL"
		    | SOME i => pp_string (string_of_int32 i))
		]
	      | Bswap r =>   [pp_string "\tBSWAP\t", print_reg opts r]
	      | Call oper => [pp_string "\tCALL\t", print_unary_op_coerce opts oper]
	      | Clc => [pp_string "\tCLC"]
	      | Cmc => [pp_string "\tCMC"]
	      | Cmovcc (cc,r,oper) =>
	       [
		pp_string "\tCMOV", print_cc opts cc, pp_char #"\t",
		print_reg opts r, pp_char #",", print_anop_coerce opts oper
		]
	      | Cmp (o1,o2) =>  [pp_string "\tCMP\t", print_binop3 opts o1 o2]
	      | Conv c => [print_conv opts c]
	      | Imul3 (r,oper,i) =>
	       [
		pp_string "\tIMUL\t", print_reg opts r,
		pp_char #",", print_anop opts oper,
		pp_char #",", pp_print_string (string_of_int32 i)
		]
	      | Int n => [pp_string "\tINT\t", pp_string (string_of_int8 n)]
	      | Into => [pp_string "\tINTO"]
	      | Jcc (cc,l,iopt) =>
	       [
		pp_string "\tJ", print_cc opts cc, pp_char #"\t",
		print_label_coerce opts l
		] @
	       (pp_iopt iopt)
	      | Jecxz (l,iopt) => 
	        [
		 pp_string "\tJECXZ\t", 
		 print_label_coerce opts l] @
		(pp_iopt iopt)
	      | Jmp oper => [pp_string "\tJMP\t", print_anop_coerce opts oper]
	      | Lahf => [pp_string "\tLAHF"]
	      | Lea (r,oper) =>
		[
		 pp_string "\tLEA\t", print_reg opts r, pp_char #",",
		 print_anop opts oper
		 ]
	      | Loopd (l,bo) =>
		[
		 pp_string "\tLOOP",
		 (case bo 
		    of NONE => pp_string ""
		     | SOME false => pp_string "NE"
		     | SOME true => pp_char #"E"),
		 pp_string "D\t", print_label_coerce opts l
		 ]
	      | Mov (o1,o2) =>
		[
		 pp_string "\tMOV\t", 
		 print_binop2 opts o1 o2
		 ]
	      | Movpart (zx,o1,rp1,o2,rp2) => 
		if rp1 <> rp2 then 
		  [
		   (if zx then pp_string "\tMOVZX\t" else pp_string "\tMOVSX\t"),
		   print_binop_part opts o1 rp1 o2 rp2 
		   ]
		else
		  [
		   pp_string "\tMOV\t",
		   print_binop_part opts o1 rp1 o2 rp2
		   ]
	      | Nop => [pp_string "\tNOP"]
	      | Pop oper => [pp_string "\tPOP\t", print_unary_op opts oper]
	      | Popad => [pp_string "\tPOPAD"]
	      | Popfd => [pp_string "\tPOPFD"]
	      | Push oper => [pp_string "\tPUSH\t", print_unary_op_coerce opts oper]
	      | Pushad => [pp_string "\tPUSHAD"]
	      | Pushfd => [pp_string "\tPUSHFD"]
	      | Rdtsc => [pp_string "\tRDTSC"]
	      | Retn no =>
		  [
		  pp_string "\tRETN",
		   (case no 
		      of NONE => pp_string ""
		       | SOME n => pp_string  ("\t"^ (string_of_int32 n)))
		      ]
	      | Sahf => [pp_string "\tSAHF"]
	      | Setcc (cc,oper) =>
		  [
		   pp_string "\tSET", print_cc opts cc, pp_char #"\t",
		   (case oper 
		      of Reg r => print_reg_part opts r RPl
		       | _ => error "bad args to SETcc")
		  ]
	      | Shld (oper,r,io) => 
		  [
		   pp_string "\tSHLD\t", print_anop opts oper,
		   pp_char #",", print_reg opts r, pp_char #",",
		   (case io 
		      of NONE => pp_string "CL"
		       | SOME c => pp_string (string_of_int32 c))
		      ]
	      | Shrd (oper,r,io) => 
		  [
		   pp_string "\tSHRD\t", print_anop opts oper,
		   pp_char #",", print_reg opts r, pp_char #",",
		   (case io 
		      of NONE => pp_string "CL"
		       | SOME c => pp_string (string_of_int32 c))
		      ]
	      | Stc => [pp_string "\tSTC"]
	      | Test (o1,o2) => [pp_string "\tTEST\t", print_binop opts o1 o2]
	      | Xchg (oper,r) =>
		  [
		   pp_string "\tXCHG\t", print_anop opts oper,
		   pp_char #",", print_reg opts r
		   ]
	      (* Abstract ops *)
	      | Coerce gc =>
		  [
		  pp_string "\tCOERCE\t", print_genop_coerce opts gc
		   ]
	      | CoerceName nc =>
		  let 
		    fun print_name opts name =
		      pp_open_hovbox 0 [
					pp_string "name(",
					id_prn name,
					pp_char #")" 
					]
		  in
		    [
		     pp_string "\tCOERCE\t", 
		     print_coerce print_name opts nc
		     ]
		  end
	      | Comment s =>
		  [pp_string "; ", pp_string s]
	      | Fallthru cons =>
		   pp_string "\tFALLTHRU"
		   :: (if cons<>[] 
			 then 
			   [
			    pp_string "\t<",
			    sepc (print_con opts) cons,
			    pp_char #">"
			    ]
		       else [])
	      | Malloc (x,i,margopt) =>
		[
		 pp_string "\tMALLOC\t", 
		 pp_string (id_to_string x), pp_char #",",
		 pp_string (string_of_int32 i)
		 ] @
		 (case margopt 
		    of NONE => []
		     | SOME marg => [pp_char #",", print_mallocarg opts marg])
	      | Unpack (v,r,oper) =>
		[
		 pp_string "\tUNPACK\t", id_prn v, pp_char #",",
		 print_reg opts r, pp_char #",", print_anop_coerce opts oper
		 ]
	      | Sunpack (v,g) =>
		[
		 pp_string "\tSUNPACK\t", id_prn v, pp_char #",",
		 print_genop opts g
		 ]
	      | Nameobj (x,g) =>
		[
		 pp_string "\tNAMEOBJ\t", id_prn x, pp_char #",", 
		 print_genop opts g
		 ]
	      | ForgetUnique x =>
		[
		 pp_string "\tFORGETUNIQUE\t", id_prn x
		 ]
	      | RemoveName x =>
		[pp_string "\tREMOVENAME\t", id_prn x]
	      (* Floating Point *)
	      | FPnoargs oper => [pp_char #"\t", print_fpnoargs opts oper]
	      | FPsomeargs (oper,args) => 
		[
		 pp_char #"\t",
		 print_fpsomeargs opts oper,
		 pp_char #"\t",
		 print_fpargs opts (implicit_st oper) args
		 ]
	      (* LX *)
	      | Letprod (is,c) =>  
		[
		 pp_print_string "\tLETPROD\t", pp_print_string "["
		 ] @
		(case is 
		   of (hd::rest) => 
		     (id_prn hd ::
		      List.foldr (fn (i,s) => (pp_print_char #"," :: id_prn i :: s)) [] rest)
		    | [] => []) @
		   [ 
		    pp_print_string "],",
		    print_con opts c
		    ]
	      | Letroll (i,c) =>
		[
		 pp_print_string "\tLETROLL\t",
		 id_prn i, pp_print_char #",", print_con opts c
		 ]
	      | Vcase (int,c,id,gc) =>
		[
		 pp_print_string "\tVCASE\t",
		 pp_print_string (string_of_int32 int), pp_print_char #",", 
		 id_prn id, pp_print_char #",",
		 print_con opts c, pp_print_char #",", print_genop_coerce opts gc
		 ]
	     (* end LX *)
		)
      in pp_open_hovbox 0 fmtlist
      end

    fun print_code_block opts ((l,lc,is) : code_block) =
      let
	fun pp_instructions is = 
	  let
	    fun realp i fmt = pp_print_newline () :: print_instruction opts i :: fmt
	    fun linenop count fmt = 
	      if !pp_instr_no then realp (Comment ("Instr no: "^(Int.toString count))) fmt
	      else fmt
	    fun folder (i,(fmt,count)) = 
	      (case i 
		 of Comment _ => (realp i fmt,count)
		  | _ => 
		   let
		     val fmt = linenop count fmt
		     val fmt = realp i fmt
		   in (fmt,count+1)
		   end)
	    val (revfmt,_) = Vector.foldl folder ([],0) is
	  in List.rev revfmt
	  end
      in
      pp_open_hovbox 0 
      (
       [
	lbl_prn l,
	pp_print_char #":",
	pp_print_newline ()]
       @
       (case lc 
	  of SOME lc => 
	    if #cons opts then 
	      [
	       pp_print_string "LABELTYPE <",
	       print_con opts lc,
	       pp_print_char #">", 
	       pp_print_newline ()
	       ]
	   else []
	   | NONE => [])
       @ pp_instructions is
       )
      end
      (*** Data ***)

    fun fixupFloat float_string =
      let
	fun fixSigns [] = []
	  | fixSigns (#"~" :: rest) = #"-" :: fixSigns rest
	  | fixSigns (d :: rest) = d :: (fixSigns rest)
	fun explodeToSci f_s =
	  (explode f_s) @
	  (if ((Char.contains f_s #"e") orelse 
	       (Char.contains f_s #"E") orelse 
	       (Char.contains f_s #"n") orelse   (* Catch [~,+,-]nan and  [~,+,-]inf. *)
	       (Char.contains f_s #"i"))
	     then []
	   else [#"e", #"0"])
      in
	(implode o fixSigns o explodeToSci) float_string
      end

    fun print_data_item opts di =
      let
	val fmtlist = 
	  case di 
	    of Dlabel cl =>
	      [ pp_print_string "\tDD\t", print_label_coerce opts cl]
	     | Dbytes (s0) =>
	      let 
		fun valid_str_elt c = 
		  let  
		    val cd = Char.ord c 
		    val max = Char.ord #"~" 
		  in (cd>= 032 andalso cd<=max andalso cd<>034) 
		  end
                val s0_len = String.size s0 
		val s0_valid = 
		  let 
		    fun loop i = 
		      if(i<0) then true
		      else (valid_str_elt (String.sub (s0,i)) andalso loop (i-1))
		  in
		    loop (s0_len - 1)
		  end
	      in
		 pp_print_string "\tDB\t"
		 ::
		 (if s0_valid then
		    [pp_print_string "\034",
		     pp_print_string s0,
		     pp_print_string "\034"
		     ]
		  else 
		    let
		      fun loop (i,acc) = 
			let
			  val c = String.sub(s0,i)
			in
			  if i = 0 then pp_print_int (Char.ord c) :: acc
			  else loop (i -1,pp_print_char #"," ::  pp_print_int (Char.ord c) :: acc)
			end
		    in loop (s0_len -1,[])
		    end)
	      end
	     | D2bytes i => [pp_print_string ("\tDW\t" ^ (string_of_int16 i))]
	     | D4bytes ci =>
	      let 
		fun aux opts i = pp_print_string (string_of_int32 i) 
	      in
		[
		 pp_print_string "\tDD\t", print_coerce aux opts ci
		 ]
	      end
	     | Dfloat32 f => error "No 32 bit float constants yet" (*[pp_print_string ("\tREAL4\t0x" ^ (f32_to_hex f))]*)
	     | Dfloat64 f => [pp_print_string ("\tREAL8\t" ^ (fixupFloat f))]
	     | Djunk => [pp_print_string "\tDD\t?"]
	     | Dup => [pp_print_string "\tTAL_STRUCT"]
	     | Ddown => [pp_print_string "\tTAL_ENDS"]
      in pp_open_hovbox 0 fmtlist
      end

    fun print_data_block opts ((l,align,lc,(dis,clist)) : data_block) =
      let
	val fmt1 = 
	  [
	   lbl_prn l,
	   pp_print_char #":",
	   pp_print_newline (),
	   if align <> 0w4 then pp_open_hovbox 0
	     [
	      pp_print_string ("ALIGN\t" ^ (string_of_int32 align)),
	      pp_print_newline ()
	      ]
	   else pp_print_string "",
	   (case (#cons opts,lc)
	      of (true,SOME c) => pp_open_hovbox 0
		[
		 pp_print_string "LABELTYPE <",
		 print_con opts c,
		 pp_print_char #">", 
		 pp_print_newline ()
		 ]
	       | _ => pp_print_string ""),
	   if clist<>[] then pp_open_hovbox 0
	     [
	      pp_print_string "COERCE\t",
	      print_coerce (fn _ => fn _ => pp_print_string "?") opts ((),clist),
		pp_print_newline ()
		]
	   else pp_print_string ""
	     ]
	val fmt2 = 
	  let 
	    fun aux (di,s) = print_data_item opts di :: pp_print_newline () :: s
 	  in  List.foldr aux [] dis
	  end
      in pp_open_hovbox 0 (fmt1 @ fmt2)
      end

    (*** Compilation Units ***)

    fun print_vector add_cut vector printer = 
      let
	val term = if (Vector.length vector)>0 andalso add_cut then [pp_print_newline ()] else []
      in Vector.foldr printer term vector
      end


    fun print_con_abbrevs' opts con_abbrevs =
      let
	fun printer ((l,c),acc) = 
	  pp_print_string "\tTYPE\t"
	  ::pp_print_char #"<"
	  :: pp_open_hovbox 0
	  [
	   id_prn l,
	   pp_print_string " = ",
	   print_con opts c
	   ]
	  ::pp_print_char #">"
	  :: pp_print_cut ()
	  ::acc
      in print_vector true con_abbrevs printer
      end

    fun print_con_abbrevs opts con_abbrevs = pp_open_vbox 0 (print_con_abbrevs' opts con_abbrevs)

    fun print_kind_abbrevs' opts kind_abbrevs = 
      let
	fun printer ((l,k),acc) = 
	  pp_print_string "\tKIND\t"
	  ::pp_print_char #"<"
	  :: pp_open_hovbox 0
	  [
	   id_prn l,
	   pp_print_break 1 2,
	   pp_print_string "= ",
	   print_kind opts k
	   ]
	  ::pp_print_char #">"
	  :: pp_print_cut ()
	  ::acc
      in print_vector true kind_abbrevs printer
      end
      
    fun print_kind_abbrevs opts kind_abbrevs =  pp_open_vbox 0 (print_kind_abbrevs' opts kind_abbrevs)

    fun print_int_header' opts name = 
      let
	val fmt1 = 
	  [
	   pp_print_string ("; TAL INTERFACE "^name), pp_print_newline (),
	   pp_print_string "; This file was generated by TILT ", pp_print_newline ()
	   ]
      in fmt1
      end

    fun print_int_header opts name = pp_open_vbox 0 (print_int_header' opts name)

    fun print_tal_int' (opts : options) (modname : string) (m : tal_int) =
      let
	val fmt1 = print_int_header' opts modname
	val fmt2 = print_con_abbrevs' opts (#int_abbrevs m) 
	val fmt3 = print_kind_abbrevs' opts (#int_kindabbrevs m) 
	val fmt4 = 
	  let
	    fun folder ((l,k,cd),acc) = 
	      pp_print_string "\tTYPE\t"
	      :: pp_print_char #"<"
	      :: pp_open_hovbox 0 ([
				    lbl_prn l
				    ,pp_print_break 1 2
				    ,pp_print_char #":"
				    ,print_kind opts k
				    ]
				    @ (case cd 
					 of AbsCon => []
					  | BoundCon c => 
					   [pp_print_break 1 2, pp_print_string "<= ", print_con opts c]
					  | ConcCon c =>
					   [pp_print_break 1 2, pp_print_string "= ", print_con opts c])
				   )
	      :: pp_print_char #">"
	      :: pp_print_newline ()
	      :: acc

	  in print_vector true (#int_cons m) folder
	  end

       val fmt5 = 
	 let
	   fun folder ((l,c),acc) = 
	     pp_print_string "\tVAL\t"
	     ::lbl_prn l
	     ::pp_print_string ",<"
	     ::print_con opts c
	     ::pp_print_char #">"
	     ::pp_print_newline ()
	     ::acc
	 in print_vector false (#int_vals m) folder
	 end
      in (fmt1@fmt2@fmt3@fmt4@fmt5)
      end

    fun print_tal_int opts modname tal_int = pp_open_vbox 0 (print_tal_int' opts modname tal_int)

    fun print_tal_int_type (opts : options) ({it_cons=cons, it_vals=vals} : tal_int_type) =
      let 
	fun prn_lkco (l,k,cd) =
	  pp_open_hovbox 0 
	  ([ lbl_prn l, pp_print_break 1 2,
	    print_ckind opts k
	    ] @
	   (if #cons opts then
	      case cd 
		of AbsCon => []
		 | BoundCon c =>
		  [pp_print_break 1 2, pp_print_string "<=", print_con opts c]
		 | ConcCon c =>
		  [pp_print_break 1 2, pp_print_char #"=", print_con opts c]
	    else []))
	fun prn_lc (l,c) =
	  pp_open_hovbox 0
	  [lbl_prn l, pp_print_break 1 2,
	   print_ccon opts c
	   ]
      in
	pp_open_hovbox 0
	[
	 pp_print_char #"{", 
	 pp_open_hovbox 0 
	 [
	  sepc prn_lkco cons, 
	  pp_print_char #",", pp_print_break 1 0, 
	  sepc prn_lc vals
	  ],
	 pp_print_char #"}"
	 ]
      end

    fun print_tal_imp_body' (opts : options) (m : tal_imp) =
      let 
	val fmt1 = print_con_abbrevs' opts (#imp_abbrevs m) 

	val fmt2 = 
	  let
	    fun folder ((a,k),acc) = 
	      pp_print_string "\tKIND\t" :: 
	      pp_print_char #"<":: 
	      (pp_open_hovbox 0 
	       [ id_prn a,
		pp_print_break 1 2,
		pp_print_string "= ",
		print_kind opts k
		])::
	      pp_print_char #">" ::
	      pp_print_cut () ::
	      acc
	  in print_vector true (#imp_kindabbrevs m) folder
	  end

	val fmt3 = 
	  let
	    fun folder ((l,k,c),acc) =
	      pp_print_string "\tTYPE\t" ::
	      pp_print_char #"<" ::
	      pp_open_hovbox 0 [ lbl_prn l,
				pp_print_break 1 2, pp_print_char #":", print_kind opts k,
				pp_print_break 1 2, pp_print_string "= ",
				print_con opts c
				] ::
	      pp_print_char #">" ::
	      pp_print_cut () ::
	      acc
	  in print_vector true (#con_blocks m) folder
	  end

	val fmt4 = 
	  [pp_print_string "\tCODE", pp_print_cut (), pp_print_cut ()]

	val fmt5 = 
	  let
	    fun folder (block,acc) = 
	      print_code_block opts block :: acc
	  in print_vector true (#code_blocks m) folder
	  end
	
	val fmt6 = 
	  [pp_print_string "\tDATA", pp_print_cut (), pp_print_cut ()]

	val fmt7 = 
	  let
	    fun folder (dblock,acc) = print_data_block opts dblock :: acc
	  in print_vector true (#data_blocks m) folder
	  end

	val fmt8 = 
	  [
	   pp_print_string "\t_end_TAL", pp_print_cut (),
	   pp_print_string "\tEND", pp_print_cut ()
	   ]
      in
	(fmt1 @ fmt2 @ fmt3 @ fmt4 @ fmt5 @ fmt6 @ fmt7 @ fmt8
	 )
      end

    fun print_tal_imp_body opts m = pp_open_vbox 0 (print_tal_imp_body' opts m)

    fun print_imp_header' (opts : options) name = 
      let
	val fmt1 = 
	  [
	   pp_print_string ("; TAL IMPLEMENTATION "^name), pp_print_cut (),
	   pp_print_string "; This file was generated by TILT", pp_print_cut (),
	   pp_print_string "\tINCLUDE\tTAL.INC", pp_print_cut (),
	   pp_print_string "\t_begin_TAL", pp_print_cut (), pp_print_cut (),
	   pp_print_cut ()
	   ]
      in fmt1 
      end

    fun print_imp_header opts name = pp_open_vbox 0 (print_imp_header' opts name)

    fun print_tal_import_refs' opts import_refs = 
      let
	fun folder (iref,acc) = 
	  let
	    val name = 
	      (case iref
		 of Int_filename s => s
		  | Int_data (s,_) => s )
	  in
	    pp_print_string ("\tTAL_IMPORT\t"^name) 
	    ::pp_print_cut ()
	    ::acc
	  end
      in print_vector true import_refs folder
      end

    fun print_tal_import_refs opts import_refs = pp_open_vbox 0 (print_tal_import_refs' opts import_refs)

    fun print_tal_export_refs' (opts : options) (export_refs : Tal.int_ref vector) = 
      let
	fun folder (eref,acc) = 
	  let 
	    val name =
	      case eref
		of Int_filename s => s
		 |Int_data (s,_) => s 
	  in
	    pp_print_string ("\tTAL_EXPORT\t"^name)
	    :: pp_print_cut ()
	    :: acc
	  end
      in print_vector true export_refs folder
      end

    fun print_tal_export_refs opts export_refs = pp_open_vbox 0 (print_tal_export_refs' opts export_refs)

    fun print_tal_pre_mod (opts : options) name (modl : tal_pre_mod) =
      let 
	val m = #pre_imp modl 

	val fmt1 = print_imp_header' opts name

	val fmt2 = print_tal_import_refs' opts (#import_refs modl)

	val fmt3 = print_tal_export_refs' opts (#export_refs modl)

	val fmt4 = print_tal_imp_body' opts m

      in pp_open_vbox 0
	(fmt1 @ fmt2 @ fmt3 @ fmt4 
	 )
      end


    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp std_options obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  ())
      end

    fun help pp obj = pp std_options obj
    fun help' pp obj = wrapper pp TextIO.stdOut obj

    val pp_genop' = help print_genop
    val pp_genop = help' print_genop




    fun write_vector write add_cut vector printer = 
      let
	val () = Vector.app ((app write) o printer) vector
	val () = if (Vector.length vector)>0 andalso add_cut then write (pp_print_newline ()) else ()
      in ()
      end

    fun write_tal_imp_body' (opts : options) (write : Formatter.format -> unit) (m : tal_imp) =
      let 
	val writel = app write

	val () = 
	  let
	    fun printer (l,c) = 
	      [pp_print_string "\tTYPE\t"
		     ,pp_print_char #"<"
		     , pp_open_hovbox 0
		     [
		      id_prn l,
		      pp_print_string " = ",
		      print_con opts c
		      ]
		     ,pp_print_char #">"
		     , pp_print_cut ()
		     ]
	  in  write_vector write true (#imp_abbrevs m) printer
	  end

	val () = 
	  let
	    fun printer (a,k) = 
	      [pp_print_string "\tKIND\t" , 
	       pp_print_char #"<", 
	       (pp_open_hovbox 0 
		[ id_prn a,
		 pp_print_break 1 2,
		 pp_print_string "= ",
		 print_kind opts k
		 ]),
	       pp_print_char #">" ,
	       pp_print_cut () 
	       ]
	  in write_vector write true (#imp_kindabbrevs m) printer
	  end

	val () = 
	  let
	    fun printer ((l,k,c)) =
	      [
	      pp_print_string "\tTYPE\t" ,
	      pp_print_char #"<" ,
	      pp_open_hovbox 0 [ lbl_prn l,
				pp_print_break 1 2, pp_print_char #":", print_kind opts k,
				pp_print_break 1 2, pp_print_string "= ",
				print_con opts c
				] ,
	      pp_print_char #">" ,
	      pp_print_cut () 
	       ]
	  in write_vector write true (#con_blocks m) printer
	  end

	val () = 
	  (write(pp_print_string "\tCODE");
	   write(pp_print_cut ());
	   write(pp_print_cut ())
	   )

	val () = 
	  let
	    fun printer block = 
	      [print_code_block opts block]
	  in write_vector write true (#code_blocks m) printer
	  end
	
	val () = 
	  writel[pp_print_string "\tDATA", pp_print_cut (), pp_print_cut ()]

	val () = 
	  let
	    fun printer (dblock) = [print_data_block opts dblock]
	  in write_vector write true (#data_blocks m) printer
	  end

	val () = 
	  writel[
		 pp_print_string "\t_end_TAL", pp_print_cut (),
		 pp_print_string "\tEND", pp_print_cut ()
		 ]
      in
	()
      end

    fun write_tal_imp_body opts write m = write_tal_imp_body' opts write m

  end  (* EOF: x86talpp.ml *)


