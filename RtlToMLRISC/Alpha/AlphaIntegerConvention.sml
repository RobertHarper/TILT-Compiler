(*$import TopLevel INTEGER_CONVENTION AlphaMLTreeExtra *)


(* =========================================================================
 * AlphaIntegerConvention.sml
 * ========================================================================= *)

structure AlphaIntegerConvention
	    :> INTEGER_CONVENTION
		 where type id	 = int
		   and type rexp = AlphaMLTreeExtra.MLTree.rexp
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTreeExtra = AlphaMLTreeExtra

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type rexp = MLTree.rexp

  (* -- values ------------------------------------------------------------- *)

  val zero		= 31
  val callPointer	= 27
  val returnPointer	= 26
  val globalPointer	= 29
  val stackPointer	= 30
  val threadPointer     = 12
  val heapPointer	= 13
  val heapLimit		= 14
  val exceptionPointer	= 15
  val exceptionArgument = 26
  val temporary1	= 28
  val temporary2	= 25

  val arguments	   = [16, 17, 18, 19, 20, 21]
  val results	   = [0]
  val callerSaves1 = [1, 2, 3, 4, 5, 6, 7, 8]
  val callerSaves2 = [22, 23, 24, 25]
  val calleeSaves  = [ 9, 10, 11 (* , 12, 13, 14, 15 *)]

  val available = results@callerSaves1@calleeSaves@arguments@callerSaves2@
		  [callPointer]
  val dedicated = [12, 13, 14, 15, 26, 28, 29, 30, 31]
  val preserve	= calleeSaves
  val define	= results@callerSaves1@arguments@callerSaves2@[callPointer]
  val use	= arguments@[callPointer]
  val escape	= results@calleeSaves

  (* -- functions ---------------------------------------------------------- *)

  val expression = MLTree.REG

end

