(*$import Prelude SYMBOL ENV *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* printutil.sig *)

signature PRINTUTIL =
sig
  structure Symbol : SYMBOL
  val newline : unit -> unit
  val tab : int -> unit
  val printSequence : string -> ('a -> unit) -> 'a list -> unit
  val printClosedSequence : (string*string*string) -> ('a -> unit) ->
					 'a list -> unit
  val printSym : Symbol.symbol -> unit
  val formatQid : Symbol.symbol list -> string
  val mlstr : string -> string
  val pr_mlstr : string -> string
  val nlindent : int -> unit
  val printvseq : int -> string -> ('a -> unit) -> 'a list -> unit
  val prIntPath : int list -> unit
  val prSymPath : Symbol.symbol list -> unit

end (* signature PRINTUTIL *)


(*
 * $Log$
# Revision 1.3  2000/09/12  18:57:09  swasey
# Changes for cutoff compilation
# 
# Revision 1.2  98/02/15  22:44:05  pscheng
# bootstrapping changes
# 
# Revision 1.1  1998/01/21  20:40:42  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  18:16:05  pscheng
# added the sig file
# 
 *)
