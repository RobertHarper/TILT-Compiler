(*$import Prelude Il *)
(* Abstract view of the compiler - from HIL and down; to be used by
 * the separate compilation system. *)

signature COMPILER =
 sig

   datatype platform = TIL_ALPHA | TIL_SPARC | MLRISC_ALPHA | MLRISC_SPARC
   val setTargetPlatform : platform -> unit  (* also sets endian-ness *)
   val getTargetPlatform : unit -> platform 

   type sbnd and context_entry and context

   val debug_asm : bool ref (* use the -g flag in call to assembler *)
   val keep_asm : bool ref  (* don't erase the asm *)
   val uptoElaborate : bool ref
   val uptoPhasesplit : bool ref
   val uptoRtl : bool ref

   (* compile(ctxt, unitName, sbnds, ctxt') 
      compiles sbnds to an asm file `unitName.platform.s'. 
    * ctxt is the context in which the sbnds
    * were produced, and ctxt' contains the new bindings. unitName is
    * the name of the unit being compiled and can be used for
    * generating unique identifiers. Also, `unitName.o' must contain a
    * label for `initialization' with name `unitName_doit'. 
    *)
   val native : unit -> bool                              (* can we invoke assembler *)
   val checkNative : unit -> unit
   val il_to_asm : string * string * Il.module -> string  (* unitName, fileBase -> .s *)
   val assemble  : string -> string                       (* fileBase -> .o, if platform matches *)
   val assemble_start : string -> string                  (* fileBase -> .o, if platform matches *)
   val assemble_done  : string -> bool                    (* fileBase -> done?, if platform matches *)

   val ui2base : string -> string  (* Convert a platform-dependent .ui filename to a filebase *)
   val base2ui : string -> string  (* Convert a filebase to a platform-dependent .ui filename *)
   val base2s  : string -> string  (* Convert a filebase to a platform-dependent .s filename *)
   val base2o  : string -> string  (* Convert a filebase to a platform-dependent .o filename *)
   val base2uo : string -> string  (* Convert a filebase to a platform-dependent .uo filename *)

 end
