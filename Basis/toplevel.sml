(*$import String Option Substring List General TopLevelHelp *)
(* http://www.dina.kvl.dk/%7Esestoft/sml/top-level-chapter.html *)

val concat = String.concat

(*
type substring = Substring.substring
*)

val valOf = Option.valOf
val app = List.app
val map = List.map
val hd = List.hd
val tl = List.tl
val (op @) = List.@
val foldl = List.foldl
val foldr = List.foldr
val null = List.null

overload + : 'a as iplus and bplus and uplus and fplus
overload - : 'a as iminus and bminus and uminus and fminus
overload * : 'a as imult and bmult and umult and fmult
overload div : 'a as idiv and bdiv and udiv
overload mod : 'a as imod and bmod and umod

overload <  : 'a as ilt and blt and ult and flt and String.<
overload <= : 'a as ilte and blte and ulte and flte and String.<=
overload >  : 'a as igt and bgt and ugt and fgt and String.>
overload >= : 'a as igte and bgte and ugte and fgte and String.>=

fun raw_print(s : string) : unit = output(1,s)
val print = textio_print
val (op o) = General.o
