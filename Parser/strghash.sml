(*$import Prelude Char TopLevel Int String *)

(* stringhash.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *)

structure StrgHash =
  struct

    val prime = 8388593 (* largest prime less than 2^23 *)
    val base = 128

    fun hashString (str: string) : int = let
	  val l = size str
	  fun ordof i = Char.ord(String.sub(str, i))
          in
	    case l
	     of 0 => 0
	      | 1 => ordof 0
	      | 2 => ordof 0 + base * ordof 1
	      | 3 => ordof 0 + base * (ordof 1 + base * ordof 2)
	      | _ => let
		  fun loop (0,n) = n
		    | loop (i,n) = let
			val i = i-1
			val n' = ordof i + (base * n) 
			in
			  loop (i, (n' - prime * Int.quot(n', prime)))
			end
		  in
		    loop (l,0)
		  end
	    (* end case *)
	  end

  end (* structure StrgHash *)




(*
 * $Log$
# Revision 1.6  2001/12/13  16:32:49  swasey
# *** empty log message ***
# 
# Revision 1.5  2000/09/12  18:57:14  swasey
# Changes for cutoff compilation
# 
 * Revision 1.4  1999/09/22 15:46:09  pscheng
 * *** empty log message ***
 *
# Revision 1.3  1998/04/24  22:51:59  pscheng
# fixed imports
#
# Revision 1.2  1998/01/21  20:40:51  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  14:12:37  pscheng
# added copy of SMLNJ parser files
# 
 *)
