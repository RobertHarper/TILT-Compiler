(*$import TopLevel FLOAT_CONVENTION SparcMLTreeExtra *)

(* =========================================================================
 * SparcFloatConvention.sml
 * ========================================================================= *)

structure SparcFloatConvention
	    :> FLOAT_CONVENTION
		 where type id	 = int
		   and type fexp = SparcMLTreeExtra.MLTree.fexp
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTreeExtra = SparcMLTreeExtra

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type fexp = MLTree.fexp

  (* -- values ------------------------------------------------------------- *)

  val zero	 = NONE
  val temporary1 = 62
  val temporary2 = 60
(*
  val arguments	   = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  val results	   = [0, 1, 2, 3]
  val callerSaves  = [16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]
*)
  val arguments	   = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30] 
  val results	   = [0, 2, 4, 6]
  val callerSaves  = [32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62] 
  val calleeSaves  = []

  val sort = Listops.insertion_sort Int.compare 
  val available = sort (results @ calleeSaves @ callerSaves@ arguments)
  val dedicated = [temporary1, temporary2]
  val preserve	= calleeSaves
  val define	= sort (results @ callerSaves @ arguments)
  val use	= arguments
  val escape	= results@calleeSaves

  (* -- functions ---------------------------------------------------------- *)

  val expression = MLTree.FREG

end

