(* =========================================================================
 * AlphaFloatConvention.sml
 * ========================================================================= *)

structure AlphaFloatConvention
	    :> FLOAT_CONVENTION
		 where type id	 = int
		   and type fexp = AlphaMLTreeExtra.MLTree.fexp
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTreeExtra = AlphaMLTreeExtra

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type fexp = MLTree.fexp

  (* -- values ------------------------------------------------------------- *)

  val zero	 = SOME 31
  val temporary2 = 29
  val temporary1 = 30

  val arguments	   = [16, 17, 18, 19, 20, 21]
  val results	   = [0, 1]
  val callerSaves1 = [10, 11, 12, 13, 14, 15]
  val callerSaves2 = [22, 23, 24, 25, 26, 27, 28, 29]
  val calleeSaves  = [2, 3, 4, 5, 6, 7, 8, 9]

  val available = results@calleeSaves@callerSaves1@arguments@callerSaves2
  val dedicated = [30, 31]
  val preserve	= calleeSaves
  val define	= results@callerSaves1@arguments@callerSaves2
  val use	= arguments
  val escape	= results@calleeSaves

  (* -- functions ---------------------------------------------------------- *)

  val expression = MLTree.FREG

end

