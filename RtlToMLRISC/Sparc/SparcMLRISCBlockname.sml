(*$import TopLevel MLRISC_BLOCKNAME *)


(* =========================================================================
 * SparcMLRISCRegion.sml
 * ========================================================================= *)

structure SparcMLRISCBlockname
	    :> MLRISC_BLOCKNAME
	    = struct

  type name = string
   
  val default = "defBlock"

  fun toString (name : name) : string = name

end

