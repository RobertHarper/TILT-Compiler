
structure AsmUtil : sig

    val itoa : int -> string

    val printableString : string -> string

  end = struct

    val hexDigits = "0123456789abcdef"

    val >> = Word.~>>
    val & = Word.andb

    infix >> &

    val wtoi = Word.toIntX
    val itow = Word.fromInt

    local
      fun f (0w0, l) = l
	| f (n, l) = f ((n >> 0w4), String.sub(hexDigits, wtoi (n & 0w15)) :: l)
    in
    fun itoa 0 = "0x0"
      | itoa i = if (i < 0)
	  then implode(#"-" :: #"0" :: #"x" :: f(itow(~i), []))
	  else implode(#"0" :: #"x" :: f(itow i, []))
    end

  (* generates a printable string, including the surrounding quotes and
   * the null termination.
   *)
    fun printableString s = let
	  fun isPrint c = ((#" " <= c) andalso (c <= #"~"))
	  fun octal i = String.sub(hexDigits, wtoi (i & 0wx7))
	  fun mkLegal #"\"" = "\\\""
	    | mkLegal #"\\" = "\\\\"
	    | mkLegal c = if ((#" " <= c) andalso (c <= #"~"))
		then String.str c
		else let val c' = itow (Char.ord c)
		  in
		    implode[#"\\", octal(c' >> 0w6), octal(c' >> 0w3), octal c']
		  end
	  val term = (case (itow(size s) & 0wx3)  (* natural word size *)
		  of 0w0 => "\000\000\000\000\""
		   | 0w1 => "\000\000\000\""
		   | 0w2 => "\000\000\""
		   | _ => "\000\""
		(* end case *))
	  in
	    concat("\"" :: (map mkLegal (explode s)) @ [term])
	  end

  end;

