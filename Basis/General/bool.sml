(*$import Prelude StringCvt BOOL *)
(* bool.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure Bool : BOOL =
  struct

(*    datatype bool = datatype bool *)

    val not = not

  (* NOTE: this should probably accept a wider range of arguments, but the
   * spec hasn't been written yet.
   *)
    fun scan (getc : (char, 'a) StringCvt.reader) cs = (
	  case (getc (StringCvt.skipWS getc cs))
	   of (SOME(#"t", cs')) => (case (StringCvt.getNChars getc (cs', 3))
		 of (SOME([#"r", #"u", #"e"], cs'')) => SOME(true, cs'')
		  | _ => NONE
		(* end case *))
	    | (SOME(#"f", cs')) => (case (StringCvt.getNChars getc (cs', 4))
		 of (SOME([#"a", #"l", #"s", #"e"], cs'')) => SOME(false, cs'')
		  | _ => NONE
		(* end case *))
	    | _ => NONE
	  (* end case *))

    fun toString true = "true"
      | toString false = "false"
    val fromString = StringCvt.scanString scan

  end

