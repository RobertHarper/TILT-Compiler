(*$import Prelude *)
(* os-process-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The generic process control interface.
 *
 *)

signature OS_PROCESS =
  sig

    eqtype status

    val success   : status
    val failure   : status

    val system    : string -> status

    val atExit    : (unit -> unit) -> unit

    val exit      : status -> 'a
    val terminate : status -> 'a

    val getEnv : string -> string option

  end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:24  swasey
# *** empty log message ***
# 
# Revision 1.1  98/03/09  19:53:07  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:21  george
 *   Version 109.24
 *
 *)
