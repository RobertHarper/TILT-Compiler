(*$import TopLevel MLRISC_BLOCKNAME *)


(* =========================================================================
 * AlphaMLRISCRegion.sml
 * ========================================================================= *)

structure AlphaMLRISCBlockname
	    :> MLRISC_BLOCKNAME
	    = struct

  type name = string
   
  val default = "defBlock"

  fun toString (name : name) : string = name

end

