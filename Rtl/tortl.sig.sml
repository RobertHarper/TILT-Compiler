signature TORTL =
sig
   structure Rtl : RTL
   structure Nil : NIL

   val debug : bool ref
   type translate_params = { HeapProfile : int option, do_write_list : bool, 
                             codeAlign : Rtl.align, FullConditionalBranch : bool, 
                             elim_tail_call : bool, recognize_constants : bool }

   val translate : translate_params -> Nil.module -> Rtl.module

end
