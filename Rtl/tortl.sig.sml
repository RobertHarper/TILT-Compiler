(*$import RTL NIL *)
signature TORTL =
sig
   structure Rtl : RTL
   structure Nil : NIL

   val diag : bool ref
   val debug : bool ref
   val debug_full_when : int ref
   val debug_full_env : bool ref
   val debug_simp : bool ref
   val debug_bound : bool ref

   val do_constant_records : bool ref
   val do_gcmerge : bool ref
   type translate_params = { HeapProfile : int option, do_write_list : bool, 
                             codeAlign : Rtl.align, FullConditionalBranch : bool, 
                             elim_tail_call : bool, recognize_constants : bool }

   val translate : string -> translate_params -> Nil.module -> Rtl.module
                   (* unit name *)

end
