(*$import TopLevel *)

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
# Revision 1.1  98/01/21  20:40:39  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  18:16:04  pscheng
# added the sig file
# 
 *)
