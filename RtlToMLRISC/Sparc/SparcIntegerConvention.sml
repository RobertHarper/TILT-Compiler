(*$import TopLevel INTEGER_CONVENTION SparcMLTreeExtra *)

(* =========================================================================
 * SparcIntegerConvention.sml
 * ========================================================================= *)

structure SparcIntegerConvention
	    :> INTEGER_CONVENTION
		 where type id	 = int
		   and type rexp = SparcMLTreeExtra.MLTree.rexp
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTreeExtra = SparcMLTreeExtra

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type rexp = MLTree.rexp

  (* -- values ------------------------------------------------------------- *)

  val zero		= 0  (* required by hardware *)
  val returnPointer	= 15 (* required by hardware; contains call address not return address *)
  val stackPointer	= 14 (* by convention *)
  val threadPointer     = 1
  val heapPointer	= 2
  val heapLimit		= 3
  val exceptionPointer	= 4
  val exceptionArgument = returnPointer
  val temporary1	= 16
  val temporary2	= 17

  val arguments	  = [8, 9, 10, 11, 12, 13]
  val results	  = [8]
  val callerSaves = [5, 6, 7] @ [16, 17, 18, 19, 20, 21, 22, 23]
  val calleeSaves = []

  val sort = Listops.insertion_sort Int.compare 
  val available = sort (results @ callerSaves @ calleeSaves @ arguments)
  (* We exclude temporary2 from dedicated *)
  val dedicated = [zero, returnPointer, stackPointer, threadPointer,
		   heapPointer, heapLimit, exceptionPointer,
		   temporary1]
  val preserve	= calleeSaves
  val define	= sort (results @ callerSaves @ arguments)
  val use	= arguments
  val escape	= sort (results @ calleeSaves)

  (* -- functions ---------------------------------------------------------- *)

  val expression = MLTree.REG

end

