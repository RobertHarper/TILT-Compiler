(*$import Prelude *)

(* pathnames.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature PATHNAMES =
  sig
    val explodePath :string -> string list
    val implodePath :string list -> string
    val trim :string -> string
  end

(*
 * $Log$
# Revision 1.3  2001/12/13  16:32:46  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/09/12  18:57:05  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/01/21  20:40:39  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  18:16:04  pscheng
# added the sig file
# 
 *)
