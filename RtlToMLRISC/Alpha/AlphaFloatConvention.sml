
(* =========================================================================
 * AlphaFloatConvention.sml
 * ========================================================================= *)

structure AlphaFloatConvention
	    :> FLOAT_CONVENTION
		 where type id = int
	    = struct

  (* -- types -------------------------------------------------------------- *)

  type id = int

  (* -- values ------------------------------------------------------------- *)

  val temporary2 = 29
  val temporary1 = 30

  val arguments	   = [16, 17, 18, 19, 20, 21]
  val results	   = [0]
  val callerSaves1 = [1, 2, 3, 4, 5, 6, 7, 8]
  val callerSaves2 = [22, 23, 24, 25, 26, 27, 28]
  val calleeSaves  = [9, 10, 11, 12, 13, 14, 15]

  val available = results@callerSaves1@calleeSaves@arguments@callerSaves2@
		  [temporary2]
  val dedicated = [30, 31]
  val preserve	= calleeSaves
  val define	= results@callerSaves1@arguments@callerSaves2
  val use	= arguments
  val escape	= results@calleeSaves

end

