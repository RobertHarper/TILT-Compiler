(*$import Firstlude TiltPrim Prelude String Substring OS_PATH OS_PathFn *)
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

