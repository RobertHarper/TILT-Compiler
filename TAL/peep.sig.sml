signature TALPEEP = 
  sig
    (* Incremental peephole optimization: add instruction to instruction list, 
     * performing small peephole optimizations in the process*)
    val peep_incr : Tal.instruction * Tal.instruction list -> Tal.instruction list
  end