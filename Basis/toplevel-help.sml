(*$import String Option Substring List General TextIO *)
(* http://www.dina.kvl.dk/%7Esestoft/sml/top-level-chapter.html *)

val option_valOf = Option.valOf
val option_isSome = Option.isSome

val list_app = List.app
val list_map = List.map
val list_hd = List.hd
val list_tl = List.tl
val list_append = List.@
val list_foldl = List.foldl
val list_foldr = List.foldr
val list_null = List.null

val string_concat = String.concat
val string_lt = String.<
val string_le = String.<=
val string_gt = String.>
val string_ge = String.>=


val textio_print = TextIO.print
