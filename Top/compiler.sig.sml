(*$import Prelude FILECACHE Paths *)

signature COMPILER =
sig

    val showWrittenContext : bool ref	(* Print contexts as they are written to disk. *)
    val writeUnselfContext : bool ref	(* Write out unselfified context too. *)
    val showImports : bool ref		(* Print imports when building contexts. *)

    (* Contains only ilFiles *)
    structure IlCache : FILECACHE

    type unit_paths = Paths.unit_paths
    type il_module
    type nil_module
    type rtl_module

    datatype kind =
	DIRECT				(* direct import; labels available *)
      | INDIRECT			(* indirect import; labels hidden *)

    datatype import =
	FILE of unit_paths * kind
      | PRIM of kind

    (* In all compiler phases, unit names may be used to generate
     * unique identifiers.  We assume that unit names are globally
     * unique.  *)
	
    (* Elaborate source file (against interface file if constrained)
     * in the context of imports.  May write a new targetIlFile and
     * modify Cache.
     *)
    val elaborate : {unit : string, smlFile : string, intFile : string option,
		     targetIlFile : string, imports : import list}
	-> il_module * bool					(* true if ilFile written to disk  *)
	
    val il_to_nil : string * il_module -> nil_module		(* unit name *)
	
    val nil_to_rtl : string * nil_module -> rtl_module		(* unit name *)
	
    val rtl_to_asm : string * rtl_module -> unit		(* assembler target *)

    (* Create initialization code for the given units. *)
    val link : string * string list -> unit			(* assembler target, units *)
			 
end
