signature TORTL =
sig
   val diag : bool ref
   val debug : bool ref
   val debug_full_when : int ref
   val debug_full_env : bool ref
   val debug_simp : bool ref
   val debug_bound : bool ref

   val translate : string * Nil.module -> Rtl.module (* unit name *)
   val entryTables : string list -> Rtl.module (* unit names *)

end
