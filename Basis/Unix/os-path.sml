(*$import Prelude String Substring OS_PATH OS_PathFn *)
(* os-path.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the UNIX implementation of the generic OS.Path structure.
 *
 *)

structure OS_Path = OS_PathFn (
  struct
    val unsafe_vsub = TiltPrim.unsafe_vsub
    val op ^ = String.^
					 
    exception Path
    exception InvalidArc

    datatype arc_kind = Null | Parent | Current | Arc of string

    fun classify "" = Null
      | classify "." = Current
      | classify ".." = Parent
      | classify a = Arc a

    val parentArc = ".."

    val currentArc = "."

    fun validVolume (_, vol)= Substring.isEmpty vol

    val volSS = Substring.all ""

    (* Note: we are guaranteed that this is never called with "" *)
    fun splitVolPath s = if (unsafe_vsub(s, 0w0) = #"/")
	  then (true, volSS, Substring.triml 1 (Substring.all s))
	  else (false, volSS, Substring.all s)

    fun joinVolPath (true, "", "") = "/"
      | joinVolPath (true, "", s) = "/" ^ s
      | joinVolPath (false, "", s) = s
      | joinVolPath _ = raise Path (* invalid volume *)

    val arcSepChar = #"/"

    fun toUnixPath (s : string) = s
    fun fromUnixPath (s : string) = s
  end);

(*
 * $Log$
# Revision 1.5  2001/12/13  16:31:33  swasey
# *** empty log message ***
# 
# Revision 1.4  2000/11/27  22:36:43  swasey
# *** empty log message ***
# 
 * Revision 1.3  2000/09/12 18:55:02  swasey
 * Changes for cutoff compilation
 *
 * Revision 1.2  2000/08/22 18:28:24  swasey
 * Brought up to date
 *
# Revision 1.1  98/03/09  19:54:19  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
