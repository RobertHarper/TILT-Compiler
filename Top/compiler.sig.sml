(*$import Prelude *)
(* Abstract view of the compiler - from HIL and down; to be used by
 * the separate compilation system. *)

signature COMPILER =
 sig

   datatype platform = TIL_ALPHA | TIL_SPARC | MLRISC_ALPHA | MLRISC_SPARC
   val platform : platform ref

   type sbnd and context_entry and context

   val debug_asm : bool ref (* use the -g flag in call to assembler *)
   val keep_asm : bool ref  (* don't erase the asm *)
   val uptoElaborate : bool ref
   val uptoPhasesplit : bool ref
   val uptoRtl : bool ref

   val compile : context * string * (sbnd option * context_entry) list * context -> string

   (* compile(ctxt, unitName, sbnds, ctxt') compiles sbnds to an obj file `unitName.platform.o'. 
    * ctxt is the context in which the sbnds
    * were produced, and ctxt' contains the new bindings. unitName is
    * the name of the unit being compiled and can be used for
    * generating unique identifiers. Also, `unitName.o' must contain a
    * label for `initialization' with name `unitName_doit'. 
    *)

   val base2s : string -> string  (* Given a base, create name of .o file for current platform *)
   val base2o : string -> string  (* Given a base, create name of .o file for current platform *)
   val base2uo : string -> string  (* Given a base, create name of .o file for current platform *)

 end
