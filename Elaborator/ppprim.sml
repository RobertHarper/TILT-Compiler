(*$import PPPRIM PRIM Util Formatter Prim Stats *)
(* Primitives pretty-printer. *)
structure Ppprim :> PPPRIM =
  struct

    open Prim Formatter Util

    val error = fn s => error "ppprim.sml" s
    val elide = Stats.ff("HilElide")

    fun pp_is_real W8 = String "8"
      | pp_is_real W16 = String "16"
      | pp_is_real W32 = String "32"
      | pp_is_real W64 = String "64"

    fun pp_fs_real F32 = String "32"
      | pp_fs_real F64 = String "64"

    fun pp_is arg = if (!elide) then String "" else pp_is_real arg
    fun pp_fs arg = if (!elide) then String "" else pp_fs_real arg

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
	  | xor_uint is => Hbox[String "XOR_UINT", pp_is is]
	  | lshift_uint is => Hbox[String "LSHIFT_UINT", pp_is is]
          | mk_ref  => String "MK_REF"
          | deref  => String "DEREF"
	  | eq_ref => String "EQ_REF"
	  | setref => String "SETREF")

    fun pp_aggregate str t = 
	let val str2 = (case t of
			    (OtherArray true) => "_knownArray"
			  | (OtherArray false) => "_unknownArray"
			  | (OtherVector true) => "_knownVector"
			  | (OtherVector false) => "_unknownVector"
			  | (IntArray is) => "_intArray"
			  | (IntVector is) => "_intVector"
			  | (FloatArray is) => "_floatArray"
			  | (FloatVector is) => "_floatVector")
	in  String (str ^ str2)
	end

    fun pp_prim prim = 
	  (case prim of
(*	   | NIL  {instance} => String "nil" *)
	     soft_vtrap tt => String "SOFT_VTRAP"
	   | soft_ztrap tt => String "SOFT_ZTRAP"
	   | hard_vtrap tt => String "HARD_VTRAP"
	   | hard_ztrap tt => String "HARD_ZTRAP"

(*	   | NOT => String "NOT" *)

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
	   | int2int(is1,is2) => Hbox[String "INT2INT(", pp_is is1, String "->", pp_is is2, String ")"]
	   | uint2uint(is1,is2) => Hbox[String "UINT2UINT(", pp_is is1, String "->", pp_is is2, String ")"]
	   | int2uint(is1,is2) => Hbox[String "INT2UINT(", pp_is is1, String "->", pp_is is2, String ")"]
	   | uint2int(is1,is2) => Hbox[String "UINT2INT(", pp_is is1, String "->", pp_is is2, String ")"]
	   | uinta2uinta(is1,is2) => Hbox[String "UINTA2UINTA(", pp_is is1, String "->", pp_is is2, String ")"]
	   | uintv2uintv(is1,is2) => Hbox[String "UINTV2UINTV(", pp_is is1, String "->", pp_is is2, String ")"]

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

(*	   | LENGTH2 => String "length2" *)


(*
	    AND => String "AND"
	  | OR => String "OR"
	  | EQ_BOOL => String "EQ_BOOL"
	  | XOR => String "XOR"
*)

(*
	  | STRING_CONCAT => String "STRING_CONCAT"
*)
(*
	  | EQ_CHAR => String "EQ_CHAR""
	  | NEQ_CHAR => String "NEQ_CHAR"
	  | EQ_STRING => String "EQ_STRING"
	  | NEQ_STRING => String "NEQ_STRING"
*)
	  | plus_float fs => Hbox[String "plusF", pp_fs fs]
	  | minus_float fs => Hbox[String "minusF", pp_fs fs]
	  | mul_float fs => Hbox[String "mulF", pp_fs fs]
	  | div_float fs => Hbox[String "divF", pp_fs fs]
	  | less_float fs => Hbox[String "lessF", pp_fs fs]
	  | greater_float fs => Hbox[String "greaterF", pp_fs fs]
	  | lesseq_float fs => Hbox[String "lesseqF", pp_fs fs]
	  | greatereq_float fs => Hbox[String "greaterF", pp_fs fs]
	  | eq_float fs => Hbox[String "eqF", pp_fs fs]
	  | neq_float fs => Hbox[String "neqF", pp_fs fs]
	  | plus_int is => Hbox[String "plusI", pp_is is]
	  | minus_int is => Hbox[String "minusI", pp_is is]
	  | mul_int is => Hbox[String "mulI", pp_is is]
	  | div_int is => Hbox[String "divI", pp_is is]
	  | mod_int is => Hbox[String "modI", pp_is is]
	  | quot_int is => Hbox[String "quotI", pp_is is]
	  | rem_int is => Hbox[String "remI", pp_is is]
	  | plus_uint is => Hbox[String "plusUI", pp_is is]
	  | minus_uint is => Hbox[String "minusUI", pp_is is]
	  | mul_uint is => Hbox[String "mulUI", pp_is is]
	  | div_uint is => Hbox[String "divUI", pp_is is]
	  | mod_uint is => Hbox[String "modUI", pp_is is]
	  | less_int is => Hbox[String "lessI", pp_is is]
	  | greater_int is => Hbox[String "greaterI", pp_is is]
	  | lesseq_int is => Hbox[String "lesseqI", pp_is is]
	  | greatereq_int is => Hbox[String "greatereqI", pp_is is]
	  | eq_int is => Hbox[String "eqI", pp_is is]
	  | neq_int is => Hbox[String "neqI", pp_is is]
	  | lshift_int is => Hbox[String "lshiftI", pp_is is]
	  | rshift_int is => Hbox[String "rshiftI", pp_is is]
	  | and_int is => Hbox[String "andI", pp_is is]
	  | or_int is => Hbox[String "orI", pp_is is]
	  | xor_int is => Hbox[String "xorI", pp_is is]
	  | less_uint is => Hbox[String "lessUI", pp_is is]
	  | greater_uint is => Hbox[String "greaterUI", pp_is is]
	  | lesseq_uint is => Hbox[String "lesseqUI", pp_is is]
	  | greatereq_uint is => Hbox[String "greatereqUI", pp_is is]
	  | rshift_uint is => Hbox[String "rshiftUI", pp_is is]

	  | open_in => String "open_in"
	  | input => String "input"
	  | input1 => String "input1"
	  | lookahead => String "lookahead"
	  | open_out => String "open_out"
	  | close_in => String "close_in"
	  | close_out => String "close_out"
	  | flush_out => String "flush_out"
	  | output => String "output"
	  | end_of_stream => String "end_of_stream"



	  | array2vector t => pp_aggregate "array2vector" t
	  | vector2array t => pp_aggregate "vector2array" t
	  | create_table t => pp_aggregate "create" t
	  | create_empty_table t => pp_aggregate "createempty" t
	  | sub t => pp_aggregate "sub" t
	  | update t => pp_aggregate "update" t
	  | length_table t => pp_aggregate "length" t
	  | equal_table t => pp_aggregate "equal" t

(*	   | ARRAY2  {instance} => String "array2"
	   | SUB2    {instance} => String "sub2" *)
		 )
(*
	fun pp_prim4 prim = 
	  (case prim of
	     UPDATE2 {instance} => String "update2")
*)


    fun pp_tag n = String(Name.tag2string n)
    fun pp_value exp2value pp_exp pp_con scon =
	let 
	    fun doint (intsize,w) = TilWord64.toDecimalString w
	in case scon of
	    int (is,i) => String (doint (is,i))
	  | uint (is,i) => String (doint (is,i))
	  | float (_,s) => String s
	  | array (c,a) => String "ArrayValue"
	  | vector (_,a) => 
		if ((Array.length a) > 0)
		    then (case (exp2value(Array.sub(a,0))) of
			      SOME(uint(W8,_)) => 
				  let fun folder(e,acc) = 
				      (case (exp2value e) of
					   SOME(uint(W8,c)) => 
					       let val c = chr(TilWord64.toInt c)
					       in  if (c = #"\n") then (#"\\")::(#"n")::acc else c::acc
					       end
					 | _ => error "bad vector value: corrupt string")
				  in  String(implode(#"\"" :: (Array.foldr folder [#"\""] a)))
				  end
			       | _ => String "VectorValue")
		else String "EmptyVectorValue"
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
    val pp_is' = pp_is_real
    val pp_fs' = pp_fs_real
    val pp_value' = pp_value
    fun pp_prim obj = (wrapper pp_prim' TextIO.stdOut obj; ())
    fun pp_ilprim obj = (wrapper pp_ilprim' TextIO.stdOut obj; ())
    fun pp_is obj = (wrapper pp_is' TextIO.stdOut obj; ())
    fun pp_fs obj = (wrapper pp_fs' TextIO.stdOut obj; ())
    fun pp_value exp2value pp_exp pp_con obj = (wrapper (pp_value' exp2value pp_exp pp_con) 
						TextIO.stdOut obj; ())

  end
