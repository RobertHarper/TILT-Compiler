(*$import TopLevel MLRISC_CONSTANT *)


(* =========================================================================
 * SparcMLRISCConstant.sml
 * ========================================================================= *)

structure SparcMLRISCConstant
	    :> MLRISC_CONSTANT
	    = struct

  (* -- types -------------------------------------------------------------- *)

  datatype const =
    Int of int
  | DeferInt of unit -> int

  (* -- functions ---------------------------------------------------------- *)

  fun valueOf(Int int)	    = int
    | valueOf(DeferInt int) = int()

  fun toString const = Int.toString(valueOf const)

end

