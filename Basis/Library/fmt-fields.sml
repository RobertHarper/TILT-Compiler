(*$import Word8 Word Real Substring Atom *)

(* fmt-fields.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This module defines types and routines that are common to both
 * the Format and Scan structures.
 *)

structure FmtFields : sig

  (* precompiled format specifiers *)
    datatype sign
      = DfltSign	(* default: put a sign on negative numbers *)
      | AlwaysSign	(* "+"      always has sign (+ or -) *)
      | BlankSign	(* " "      put a blank in the sign field for positive numbers *)
    datatype neg_sign
      = MinusSign	(* default: use "-" for negative numbers *)
      | TildeSign	(* "~"      use "~" for negative numbers *)
    type field_flags = {
	sign : sign,
	neg_char : neg_sign,
	zero_pad : bool,
	base : bool,
	ljust : bool,
	large : bool
      }

    datatype field_wid = NoPad | Wid of int

    datatype real_format
      = F_Format		(* "%f" *)
      | E_Format of bool	(* "%e" or "%E" *)
      | G_Format of bool	(* "%g" or "%G" *)

    datatype field_type
      = OctalField
      | IntField
      | HexField
      | CapHexField
      | CharField
      | BoolField
      | StrField
      | RealField of {prec : int, format : real_format}

    datatype fmt_spec
      = Raw of substring
      | CharSet of char -> bool
      | Field of (field_flags * field_wid * field_type)

    datatype fmt_item
      = ATOM of Atom.atom
      | LINT of LargeInt.int
      | INT of Int.int
      | LWORD of LargeWord.word
      | WORD of Word.word
      | WORD8 of Word8.word
      | BOOL of bool
      | CHR of char
      | STR of string
      | REAL of Real.real
      | LREAL of LargeReal.real
      | LEFT of (int * fmt_item)	(* left justify in field of given width *)
      | RIGHT of (int * fmt_item)	(* right justify in field of given width *)

    exception BadFormat			(* bad format string *)

    val scanFieldSpec : substring -> (fmt_spec * substring)
    val scanField     : substring -> (fmt_spec * substring)

  end = struct

    structure SS = Substring
    structure SC = StringCvt

  (* precompiled format specifiers *)
    datatype sign
      = DfltSign	(* default: put a sign on negative numbers *)
      | AlwaysSign	(* "+"      always has sign (+ or -) *)
      | BlankSign	(* " "      put a blank in the sign field for positive numbers *)
    datatype neg_sign
      = MinusSign	(* default: use "-" for negative numbers *)
      | TildeSign	(* "~"      use "~" for negative numbers *)
    type field_flags = {
	sign : sign,
	neg_char : neg_sign,
	zero_pad : bool,
	base : bool,
	ljust : bool,
	large : bool
      }

    datatype field_wid = NoPad | Wid of int

    datatype real_format
      = F_Format		(* "%f" *)
      | E_Format of bool	(* "%e" or "%E" *)
      | G_Format of bool	(* "%g" or "%G" *)

    datatype field_type
      = OctalField
      | IntField
      | HexField
      | CapHexField
      | CharField
      | BoolField
      | StrField
      | RealField of {prec : int, format : real_format}

    datatype fmt_spec
      = Raw of substring
      | CharSet of char -> bool
      | Field of (field_flags * field_wid * field_type)

    datatype fmt_item
      = ATOM of Atom.atom
      | LINT of LargeInt.int
      | INT of Int.int
      | LWORD of LargeWord.word
      | WORD of Word.word
      | WORD8 of Word8.word
      | BOOL of bool
      | CHR of char
      | STR of string
      | REAL of Real.real
      | LREAL of LargeReal.real
      | LEFT of (int * fmt_item)	(* left justify in field of given width *)
      | RIGHT of (int * fmt_item)	(* right justify in field of given width *)

    exception BadFormat			(* bad format string *)

  (* string to int conversions *)
    val decToInt : (char, substring) SC.reader -> (Int.int, substring) SC.reader
	  = Int.scan SC.DEC

  (* scan a field specification.  Assume that the previous character in the
   * base string was "%" and that the first character in the substring fmtStr
   * is not "%".
   *)
    fun scanFieldSpec fmtStr = let
	  val (fmtStr, flags) = let
		fun doFlags (ss, flags : field_flags) = (
		      case (SS.getc ss, flags)
		       of (SOME(#" ", ss'), {sign=AlwaysSign, ...}) =>
			    raise BadFormat
			| (SOME(#" ", ss'), _) =>
			    doFlags (ss', {
				sign = BlankSign, neg_char = #neg_char flags,
				zero_pad = #zero_pad flags, base = #base flags,
				ljust = #ljust flags, large = #large flags
			      })
			| (SOME(#"+", ss'), {sign=BlankSign, ...}) =>
			    raise BadFormat
			| (SOME(#"+", ss'), _) =>
			    doFlags (ss', {
				sign = AlwaysSign, neg_char = #neg_char flags,
				zero_pad = #zero_pad flags, base = #base flags,
				ljust = #ljust flags, large = #large flags
			      })
			| (SOME(#"~", ss'), _) =>
			    doFlags (ss', {
				sign = #sign flags, neg_char = TildeSign,
				zero_pad = #zero_pad flags, base = #base flags,
				ljust = #ljust flags, large = #large flags
			      })
			| (SOME(#"-", ss'), _) => 
			    doFlags (ss', {
				sign = #sign flags, neg_char = MinusSign,
				zero_pad = #zero_pad flags, base = #base flags,
				ljust = #ljust flags, large = #large flags
			      })
			| (SOME(#"#", ss'), _) =>
			    doFlags (ss', {
				sign = #sign flags, neg_char = #neg_char flags,
				zero_pad = #zero_pad flags, base = true,
				ljust = #ljust flags, large = #large flags
			      })
			| (SOME(#"0", ss'), _) =>
			    (ss', {
				sign = #sign flags, neg_char = #neg_char flags,
				zero_pad = true, base = #base flags,
				ljust = #ljust flags, large = #large flags
			      })
			| _ => (fmtStr, flags)
		      (* end case *))
		in
		  doFlags (fmtStr, {
		      sign = DfltSign, neg_char = MinusSign,
		      zero_pad = false, base = false, ljust = false,
		      large = false
		    })
		end
	  val (wid, fmtStr) = if (Char.isDigit(valOf(SS.first fmtStr)))
		then let
		  val (n, fmtStr) = valOf (decToInt SS.getc fmtStr)
		  in (Wid n, fmtStr) end
		else (NoPad, fmtStr)
	  val (ty, fmtStr) = (case SS.getc fmtStr
		 of (SOME(#"d", ss)) => (IntField, ss)
		  | (SOME(#"X", ss)) => (CapHexField, ss)
		  | (SOME(#"x", ss)) => (HexField, ss)
		  | (SOME(#"o", ss)) => (OctalField, ss)
		  | (SOME(#"c", ss)) => (CharField, ss)
		  | (SOME(#"s", ss)) => (StrField, ss)
		  | (SOME(#"b", ss)) => (BoolField, ss)
		  | (SOME(#".", ss)) => let
(* NOTE: "." ought to be allowed for d,X,x,o and s formats as it is in ANSI C *)
		      val (n, ss) = valOf(decToInt SS.getc ss)
		      val (format, ss) = (case SS.getc ss
			     of (SOME(#"E" , ss))=> (E_Format true, ss)
			      | (SOME(#"e" , ss))=> (E_Format false, ss)
			      | (SOME(#"f" , ss))=> (F_Format, ss)
			      | (SOME(#"G" , ss))=> (G_Format true, ss)
			      | (SOME(#"g", ss)) => (G_Format false, ss)
			      | _ => raise BadFormat
			    (* end case *))
		      in
			(RealField{prec = n, format = format}, ss)
		      end
		  | (SOME(#"E", ss)) => (RealField{prec=6, format=E_Format true}, ss)
		  | (SOME(#"e", ss)) => (RealField{prec=6, format=E_Format false}, ss)
		  | (SOME(#"f", ss)) => (RealField{prec=6, format=F_Format}, ss)
		  | (SOME(#"G", ss)) => (RealField{prec=6, format=G_Format true}, ss)
		  | (SOME(#"g", ss)) => (RealField{prec=6, format=G_Format false}, ss)
		  | _ => raise BadFormat
		(* end case *))
	  in
	    (Field(flags, wid, ty), fmtStr)
	  end (* scanFieldSpec *)

    fun scanField fmtStr = (case SS.getc fmtStr
	   of (SOME(#"%", fmtStr')) => (Raw(SS.slice(fmtStr, 0, 1)), fmtStr')
	    | _ => scanFieldSpec fmtStr
	  (* end case *))

  end
