(* Abstract view of the compiler - from HIL and down; to be used by
 * the separate compilation system. *)

signature COMPILER =
 sig
   type sbnds and context

   val compile : context * string * sbnds * context * string -> unit

   (* compile(ctxt, unitName, sbnds, ctxt', oFile)  compiles sbnds into
    * an object file oFile. ctxt is the context in which the sbnds
    * were produced, and ctxt' contains the new bindings. unitName is
    * the name of the unit being compiled and can be used for
    * generating unique identifiers. Also, oFile must contain a label
    * for `initialization' with name `unitName_do'. *)
 end
