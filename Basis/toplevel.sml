(*$import Prelude TopLevelHelp *)
(* http://www.dina.kvl.dk/%7Esestoft/sml/top-level-chapter.html *)

extern ml_output : (int, string, unit) -->
fun raw_print(s : string) : unit = Ccall(ml_output,1,s)

val valOf = option_valOf
val isSome = option_isSome

val app = list_app
val map = list_map
val hd = list_hd
val tl = list_tl
val (op @) = list_append
val foldl = list_foldl
val foldr = list_foldr
val null = list_null

val concat = string_concat
val print = textio_print

overload + : 'a as iplus and bplus and uplus and fplus
overload - : 'a as iminus and bminus and uminus and fminus
overload * : 'a as imult and bmult and umult and fmult
overload div : 'a as idiv and bdiv and udiv
overload mod : 'a as imod and bmod and umod

overload <  : 'a as ilt and blt and ult and flt and string_lt
overload <= : 'a as ilte and blte and ulte and flte and string_le
overload >  : 'a as igt and bgt and ugt and fgt and string_gt
overload >= : 'a as igte and bgte and ugte and fgte and string_ge

