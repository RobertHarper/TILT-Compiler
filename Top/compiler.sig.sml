(*$import Prelude *)
(* Abstract view of the compiler - from HIL and down; to be used by
 * the separate compilation system. *)

signature COMPILER =
 sig

   val use_mlrisc : bool ref

   type sbnd and context_entry and context

   val debug_asm : bool ref (* use the -g flag in call to assembler *)
   val keep_asm : bool ref  (* don't erase the asm *)
   val uptoElaborate : bool ref
   val uptoNil : bool ref
   val uptoRtl : bool ref

   val compile : context * string * (sbnd option * context_entry) list * context -> unit

   (* compile(ctxt, unitName, sbnds, ctxt') compiles sbnds into an
    * object file `unitName.o'. ctxt is the context in which the sbnds
    * were produced, and ctxt' contains the new bindings. unitName is
    * the name of the unit being compiled and can be used for
    * generating unique identifiers. Also, `unitName.o' must contain a
    * label for `initialization' with name `unitName_doit'. 
    *)

 end
