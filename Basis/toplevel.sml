(*$import Prelude PreString PreInt Real Char String Option List General TextIO Vector Word31 *)
(* see http://www.dina.kvl.dk/%7Esestoft/sml/top-level-chapter.html *)

(* The interface provided by TopLevel should not be assumed by users
   of the basis library.

   Users who want the standard top-level environment should import
   Prelude and TopLevel.
*)

(* overloads *)
overload + : 'a as Word31.+
overload - : 'a as Word31.-
overload * : 'a as Word31.*
overload div : 'a as PreInt.idiv and TiltPrim.bdiv and TiltPrim.udiv and Word31.div
overload mod : 'a as PreInt.imod and TiltPrim.bmod and TiltPrim.umod and Word31.mod
overload abs : 'a as PreInt.iabs and TiltPrim.fabs
overload <  : 'a as String.<  and Word31.<
overload >  : 'a as String.>  and Word31.>
overload <= : 'a as String.<= and Word31.<=
overload >= : 'a as String.>= and Word31.>=

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
    
(* not -- primitive *)

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
