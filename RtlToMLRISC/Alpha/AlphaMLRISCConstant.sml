(*$import MLRISC_CONSTANT *)


(* =========================================================================
 * AlphaMLRISCConstant.sml
 * ========================================================================= *)

structure AlphaMLRISCConstant
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

