(*$import TopLevel INTEGER_CONVENTION AlphaMLTreeExtra *)


(* =========================================================================
 * AlphaIntegerConvention.sml
 * ========================================================================= *)

structure AlphaIntegerConvention
	    :> sig
                 include INTEGER_CONVENTION
		 val callPointer: id (* address for a procedure call, needed to fix globalPointer *)
		 val globalPointer: id (* base address of current global table *)
               end
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
  val handlerPointer    = 27
  val threadPointer     = 12
  val heapPointer	= 13
  val heapLimit		= 14
  val exceptionPointer	= 15
  val exceptionArgument = 26
  val temporary1	= 28
  val temporary2	= 25

  val sort = Listops.insertion_sort Int.compare 

  val arguments	   = [16, 17, 18, 19, 20, 21]
  val results	   = [0]
  val callerSaves = [1, 2, 3, 4, 5, 6, 7, 8, 22, 23, 24, 25]
  val calleeSaves  = [ 9, 10, 11 (* , 12, 13, 14, 15 *)]

  val available = sort (results @ callerSaves @ calleeSaves @ arguments @ [callPointer])
      
  (* We exclude temporary2, handlerPointer, and callPointer from dedicated *)
  val dedicated = [zero, returnPointer, stackPointer, threadPointer,
		   heapPointer, heapLimit, exceptionPointer,
		   temporary1]
  val preserve	= calleeSaves
  val define	= sort (results @ callerSaves @ arguments @ [callPointer])
  val use	= sort (arguments @ [callPointer])
  val escape	= sort (results @ calleeSaves)

  (* -- functions ---------------------------------------------------------- *)

  val expression = MLTree.REG

end

