(* see http://www.dina.kvl.dk/%7Esestoft/sml/top-level-chapter.html *)

(* The interface provided by TopLevel should not be assumed by users
   of the basis library.

   Users who want the standard top-level environment should import
   Prelude and TopLevel.
*)

(* overloads - complement's Prelude *)
overload div : 'a as PreInt.idiv and TiltPrim.bdiv and TiltPrim.udiv
overload mod : 'a as PreInt.imod and TiltPrim.bmod and TiltPrim.umod
overload abs : 'a as PreInt.iabs and TiltPrim.fabs
overload <  : 'a as String.<
overload >  : 'a as String.>
overload <= : 'a as String.<=
overload >= : 'a as String.>=

exception Div = Div
exception Overflow = Overflow

(* types *)
type substring = PreString.substring

(* values *)
(* !, := primitive, ref -- primitive *)
(* before, ignore, o -- Prelude *)
val exnName = General.exnName
val exnMessage = General.exnMessage

val getOpt = Option.getOpt
val isSome = Option.isSome
val valOf = Option.valOf

(* not -- Prelude *)

(* real -- primitive *)
val trunc = Real.trunc
val floor = Real.floor
val ceil = Real.ceil
val round = Real.round

(* ord -- primitive *)
val chr = Char.chr

val size = String.size
val str = String.str
val concat = String.concat
val implode = String.implode
val explode = String.explode
val substring = String.substring
val op^ = String.^

val null = List.null
val hd = List.hd
val tl = List.tl
val length = List.length
(* rev -- Prelude *)
val op@ = List.@
val app = List.app
val map = List.map
val foldr = List.foldr
val foldl = List.foldl

val print = TextIO.print

val vector = Vector.fromList

fun use (_ : string) : unit = raise TiltExn.LibFail "use unimplemented"
