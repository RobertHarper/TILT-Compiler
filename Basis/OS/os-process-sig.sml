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

