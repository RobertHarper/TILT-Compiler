(*$import SYMBOL TopLevel *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* env.sig *)

signature FASTSYMBOL = 
sig
      type raw_symbol
      type symbol
      val rawSymbol: int * string -> raw_symbol
      val sameSpaceSymbol : symbol -> raw_symbol -> symbol
      val varSymbol: raw_symbol -> symbol
      val tycSymbol: raw_symbol -> symbol
      val sigSymbol: raw_symbol -> symbol
      val strSymbol: raw_symbol -> symbol
      val fctSymbol: raw_symbol -> symbol
      val fixSymbol: raw_symbol -> symbol
      val labSymbol: raw_symbol -> symbol
      val tyvSymbol: raw_symbol -> symbol
      val fsigSymbol: raw_symbol -> symbol
      val var'n'fix : raw_symbol -> symbol * symbol
end

signature INTSTRMAPV = 
sig 
  type 'a intstrmap
  val new : (int * string * '_a) list -> '_a intstrmap

  (* in case of duplicates, the element towards the head of the 
   * list is discarded,and the one towards the tail is kept.
   *)
  val elems : 'a intstrmap -> int
  val map : 'a intstrmap -> int * string -> 'a
  val app : (int * string * 'a -> unit) -> 'a intstrmap -> unit
  val transform : ('a -> 'b) -> 'a intstrmap -> 'b intstrmap
  val fold : ((int*string*'a)*'b->'b)->'b->'a intstrmap->'b

end (* signature INTSTRMAP *)

signature ENV =
sig
  structure Symbol : SYMBOL
  structure FastSymbol : FASTSYMBOL

  type 'b env

  exception Unbound  
  exception SpecialEnv

  val empty: 'b env
  val look: 'b env * Symbol.symbol -> 'b
  val bind: Symbol.symbol * 'b * 'b env -> 'b env

  val special: (Symbol.symbol -> '_b) * (unit -> Symbol.symbol list) -> '_b env
      (* Note: special(f,NONE) means Don't Memoize! *)

  val atop: 'b env * 'b env -> 'b env
      (* atop(e1,e2): place e1 on top of e2 *)

  val consolidate: '1b env -> '1b env
  val app: (Symbol.symbol * 'b -> unit) -> 'b env -> unit
  val map: ('1b -> '1b) -> '1b env -> '1b env
  val fold: ((Symbol.symbol * 'b) * 'a -> 'a) -> 'a -> 'b env -> 'a

  val symbols : 'b env -> Symbol.symbol list 
                                (* may contain duplicate symbols *)

end (* signature ENV *)

(*
 * $Log$
# Revision 1.3  98/02/15  22:43:22  pscheng
# bootstrapping changes
# 
# Revision 1.2  1998/02/01  01:27:57  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
#
# Revision 1.1  1998/01/21  20:40:12  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  18:16:03  pscheng
# added the sig file
# 
 *)
