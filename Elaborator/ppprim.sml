(* Primitives pretty-printer. *)
functor Ppprim(structure Prim : PRIM)
	: PPPRIM =
  struct

    structure Prim = Prim

    open Prim Formatter Util

    val error = fn s => error "ppprim.sml" s

    fun pp_is W8 = String "8"
      | pp_is W16 = String "16"
      | pp_is W32 = String "32"
      | pp_is W64 = String "64"

    fun pp_fs F32 = String "32"
      | pp_fs F64 = String "64"

    fun pp_tt int_tt = String "INT_TT"
      | pp_tt real_tt = String "REAL_TT"
      | pp_tt both_tt = String "BOTH_TT"

    fun pp_ilprim ilprim = 
	  (case ilprim of
	    eq_uint is => Hbox[String "EQ_UINT", pp_is is]
	  | neq_uint is => Hbox[String "NEQ_UINT", pp_is is]
	  | not_uint is => Hbox[String "NOT_UINT", pp_is is]
	  | and_uint is => Hbox[String "AND_UINT", pp_is is]
	  | or_uint is => Hbox[String "OR_UINT", pp_is is]
	  | lshift_uint is => Hbox[String "LSHIFT_UINT", pp_is is])

    fun pp_prim prim = 
	  (case prim of
(*	   | NIL  {instance} => String "nil" *)
	     soft_vtrap tt => String "SOFT_VTRAP"
	   | soft_ztrap tt => String "SOFT_ZTRAP"
	   | hard_vtrap tt => String "HARD_VTRAP"
	   | hard_ztrap tt => String "HARD_ZTRAP"

(*	   | NOT => String "NOT" *)
	   | mk_ref  => String "MK_REF"
	   | deref  => String "DEREF"
(*
	   | SIZE => String "SIZE"
	   | CHR => String "CHR"
	   | ORD => String "ORD"
	   | EXPLODE => String "EXPLODE"
	   | IMPLODE => String "IMPLODE"
*)
	   | neg_float fs => Hbox[String "NEG_FLOAT", pp_fs fs]
	   | abs_float fs => Hbox[String "ABS_FLOAT", pp_fs fs]
(*
	   | SQRT => String "SQRT"
	   | SIN => String "SIN"
	   | COS => String "COS"
	   | ARCTAN => String "ARCTAN"
	   | EXP => String "EXP"
	   | LN => String "LN"
*)
	   | not_int is => Hbox[String "NOT_INT", pp_is is]
	   | neg_int is => Hbox[String "NEG_INT", pp_is is]
	   | abs_int is => Hbox[String "ABS_INT", pp_is is]
	   | float2int => String "FLOAT2INT"
	   | int2float => String "INT2FLOAT"
	   | int2uint => String "INT2UINT"
	   | uint2int => String "UINT2INT"
(*
	   | OPEN_IN => String "OPEN_IN"
	   | OPEN_OUT => String "OPEN_OUT"
	   | INPUT => String "INPUT"
	   | LOOKAHEAD => String "LOOKAHEAD"
	   | CLOSE_IN => String "CLOSE_IN"
	   | END_OF_STREAM => String "END_OF_STREAM"
	   | CLOSE_OUT => String "CLOSE_OUT"
	   | USE => String "USE"
	   | FLUSH_OUT  => String "FLUSH_OUTp"

	   | ISNIL => String "isnil"
	   | CAR => String "car"
	   | CDR  => String "cdr"
*)
	   | length1 true => String "length1"
	   | length1 false => String "vlength1"
(*	   | LENGTH2 => String "length2" *)


(*
	    AND => String "AND"
	  | OR => String "OR"
	  | EQ_BOOL => String "EQ_BOOL"
	  | XOR => String "XOR"
*)
	  | eq_ref => String "EQ_REF"
	  | setref => String "SETREF"
(*
	  | STRING_CONCAT => String "STRING_CONCAT"
*)
(*
	  | EQ_CHAR => String "EQ_CHAR"
	  | NEQ_CHAR => String "NEQ_CHAR"
	  | EQ_STRING => String "EQ_STRING"
	  | NEQ_STRING => String "NEQ_STRING"
*)
	  | plus_float fs => Hbox[String "PLUS_FLOAT", pp_fs fs]
	  | minus_float fs => Hbox[String "MINUS_FLOAT", pp_fs fs]
	  | mul_float fs => Hbox[String "MUL_FLOAT", pp_fs fs]
	  | div_float fs => Hbox[String "DIV_FLOAT", pp_fs fs]
	  | less_float fs => Hbox[String "LESS_FLOAT", pp_fs fs]
	  | greater_float fs => Hbox[String "GREATER_FLOAT", pp_fs fs]
	  | lesseq_float fs => Hbox[String "LESSEQ_FLOAT", pp_fs fs]
	  | greatereq_float fs => Hbox[String "GREATEREQ_FLOAT", pp_fs fs]
	  | eq_float fs => Hbox[String "EQ_FLOAT", pp_fs fs]
	  | neq_float fs => Hbox[String "NEQ_FLOAT", pp_fs fs]
	  | plus_int is => Hbox[String "PLUS_INT", pp_is is]
	  | minus_int is => Hbox[String "MINUS_INT", pp_is is]
	  | mul_int is => Hbox[String "MUL_INT", pp_is is]
	  | div_int is => Hbox[String "DIV_INT", pp_is is]
	  | mod_int is => Hbox[String "MOD_INT", pp_is is]
	  | quot_int is => Hbox[String "QUOT_INT", pp_is is]
	  | rem_int is => Hbox[String "REM_INT", pp_is is]
	  | plus_uint is => Hbox[String "PLUS_UINT", pp_is is]
	  | minus_uint is => Hbox[String "MINUS_UINT", pp_is is]
	  | mul_uint is => Hbox[String "MUL_UINTp", pp_is is]
	  | div_uint is => Hbox[String "DIV_UINT", pp_is is]
	  | mod_uint is => Hbox[String "MOD_UINT", pp_is is]
	  | less_int is => Hbox[String "LESS_INT", pp_is is]
	  | greater_int is => Hbox[String "GREATER_INT", pp_is is]
	  | lesseq_int is => Hbox[String "LESSEQ_INT", pp_is is]
	  | greatereq_int is => Hbox[String "GREATEREQ_INT", pp_is is]
	  | eq_int is => Hbox[String "EQ_INT", pp_is is]
	  | neq_int is => Hbox[String "NEQ_INT", pp_is is]
	  | lshift_int is => Hbox[String "LSHIFT_INT", pp_is is]
	  | rshift_int is => Hbox[String "RSHIFT_INT", pp_is is]
	  | and_int is => Hbox[String "AND_INT", pp_is is]
	  | or_int is => Hbox[String "OR_INT", pp_is is]
	  | less_uint is => Hbox[String "LESS_UINT", pp_is is]
	  | greater_uint is => Hbox[String "GREATER_UINT", pp_is is]
	  | lesseq_uint is => Hbox[String "LESSEQ_UINT", pp_is is]
	  | greatereq_uint is => Hbox[String "GREATEREQ_UINT", pp_is is]
	  | rshift_uint is => Hbox[String "RSHIFT_UINT", pp_is is]

	  | output => String "output"
	  | input => String "input"
(*
	  | OUTPUT => String "OUTPUT"

	  | CONS => String "::"
*)
	  | array1 true => String "array1"
	  | sub1   true  => String "sub1"
	  | update1 => String "update1"
	  | array_eq true => String "array_eq"
	  | array1 false => String "vector1"
	  | sub1   false  => String "vsub1"
	  | array_eq false => String "vector_eq"

	  | intsub1 true => String "intsub1"
	  | floatsub1 true => String "floatsub1"
	  | ptrsub1 true => String "ptrsub1"
	  | intsub1 false => String "vintsub1"
	  | floatsub1 false => String "vfloatsub1"
	  | ptrsub1 false => String "vptrsub1"
	  | intupdate1 => String "intudpate"
	  | floatupdate1 => String "floatudpate"
	  | ptrupdate1 => String "ptrudpate"

(*	   | ARRAY2  {instance} => String "array2"
	   | SUB2    {instance} => String "sub2" *)
		 )
(*
	fun pp_prim4 prim = 
	  (case prim of
	     UPDATE2 {instance} => String "update2")
*)


    fun pp_tag n = String(Name.tag2string n)
    fun pp_value pp_exp pp_con scon =
	let 
	    fun doint ((W64 | W32 | W16 | W8),w) = TilWord64.toDecimalString w
	in case scon of
	    int (is,i) => String (doint (is,i))
	  | uint (is,i) => String (doint (is,i))
	  | float (_,s) => String s
	  | array (c,a) => String "ArrayValue"
	  | vector (c,a) => String "VectorValue"
	  | refcell r => String "RefCellValue"
	  | tag (name,c) => HOVbox[String "tag(",pp_tag name, String ", ", 
				   pp_con c, String ")"]
	end

    fun wrapper pp out obj = 
      let 
	val fmtstream = open_fmt out
	val fmt = pp obj
      in (output_fmt (fmtstream,fmt); 
	  close_fmt fmtstream;
	  fmt)
      end

    val pp_prim' = pp_prim
    val pp_ilprim' = pp_ilprim
    val pp_is' = pp_is
    val pp_fs' = pp_fs
    val pp_value' = pp_value
    fun pp_prim obj = (wrapper pp_prim' TextIO.stdOut obj; ())
    fun pp_ilprim obj = (wrapper pp_ilprim' TextIO.stdOut obj; ())
    fun pp_is obj = (wrapper pp_is' TextIO.stdOut obj; ())
    fun pp_fs obj = (wrapper pp_fs' TextIO.stdOut obj; ())
    fun pp_value pp_exp pp_con obj = (wrapper (pp_value' pp_exp pp_con) TextIO.stdOut obj; ())

  end
