(*$import Nil Name *)

(* Function usage analysis *)

signature ANALYZE = 
    sig
	type funinfo = {definition : Nil.con * Nil.function,
			size : int,
			occurs : (bool * int) list}
	    (* definition = function definition
	     * size = size of function body, based on number of Nil datatype nodes used to represent it
	     * occurs = list of pairs corresponding to occurences of a function symbol, the first indicating whether
	     *          the occurrence is inside the function's own body, and the second giving the level to which the function
	     *          is applied. Here are three examples of showing the occurrence levels of the function variable f:
	     *
	     * val g = (f, 5)      level = 0
	     * val g = f 5         level = 1
	     * val g = f 5 6       level = 2
	     *)

	val analyze : Nil.module -> funinfo Name.VarMap.map
        (* Produce statistics on occurrences of function symbols in a module *)
    end

