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

