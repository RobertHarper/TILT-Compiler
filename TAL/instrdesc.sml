(* Description of instructions *)
structure InstrDesc :> INSTRDESC = 
  struct

    fun warn s = (print "WARNING: ";print s;print "\n")

    (* generate_functional_updates (NONE) (SOME "instrdesc") ["preblocks","blocks","instrs"]; *)
    type instrdesc = 
      {preblocks : Tal.code_block list,
       blocks : Tal.code_block list,
       instrs : Tal.instruction list}  

    fun empty_instrdesc () = 
      {preblocks = [],
       blocks = [],
       instrs = []}

    fun set_preblocks ({ preblocks = _  , blocks , instrs } :instrdesc) preblocks =
      {
       preblocks = preblocks,
       blocks = blocks,
       instrs = instrs
       }
    fun set_blocks ({ preblocks , blocks = _  , instrs } :instrdesc) blocks =
      {
       preblocks = preblocks,
       blocks = blocks,
       instrs = instrs
       }
    fun set_instrs ({ preblocks , blocks , instrs = _  } :instrdesc) instrs =
      {
       preblocks = preblocks,
       blocks = blocks,
       instrs = instrs
       }

    fun emit (id : instrdesc) (instr : Tal.instruction) = 
      set_instrs id (TalPeep.peep_incr(instr,#instrs id))

    fun emit_block (id : instrdesc) l copt = 
      let
	val instrs = #instrs id
	val block = (l,copt,Vector.fromList instrs)
	val blocks = block::(#blocks id)
	val id = set_blocks id blocks
	val id = set_instrs id []
      in id
      end

    fun get_blocks (id : instrdesc) = 
      let
	val () = (case #instrs id
		    of [] => ()
		     | _ => warn "Unclosed block1")
      in #preblocks id @ #blocks id
      end

    fun flush_blocks (id : instrdesc) = 
      let
	val () = (case #instrs id
		    of [] => ()
		     | _ => warn "Unclosed block1 flushed")
	val id = set_blocks id []
	val id = set_preblocks id []
	val id = set_instrs id []
      in id
      end

    fun flush_instrs (id : instrdesc) = 
      let
	val id = set_instrs id []
      in id
      end

    fun add_blocks (id : instrdesc) (blocks : Tal.code_block list): instrdesc = 
      set_blocks id ((#blocks id) @ blocks)

    fun add_blocks_before (id : instrdesc) (blocks : Tal.code_block list): instrdesc = 
      set_preblocks id (blocks @ (#preblocks id))


  end (* structure ID *)
