(*$import Rtl Nil  *)

signature TORTL =
sig
   val diag : bool ref
   val debug : bool ref
   val debug_full_when : int ref
   val debug_full_env : bool ref
   val debug_simp : bool ref
   val debug_bound : bool ref

   (* The translation to RTL performs some name mangling.  Translate
      looks up each imported label's compilation unit in a unitmap. *)

   type unitmap
   val empty : unitmap
   val extend : unitmap * Nil.label * string -> unitmap	(* unit name *)
   val restrict : unitmap * Nil.label list -> unitmap

   val translate : string * unitmap * Nil.module -> Rtl.module (* unit name *)
   val entryTables : string list -> Rtl.module (* unit names *)

end
