(* printFlowgraph.sml -- print flowgraph of target machine instructions. 
 *
 * Copyright (c) 1997 Bell Laboratories.
 *)
signature PRINT_FLOW_GRAPH = sig
   structure F : FLOWGRAPH
   structure E : EMITTER_NEW
      sharing E.I = F.I
      sharing E.P = F.P
   type regmaps = int Array.array ref list

   val printBlock : TextIO.outstream -> int Intmap.intmap -> F.block -> unit
   val printCluster : TextIO.outstream -> string -> F.cluster -> unit
end


functor PrintFlowGraphFn 
   (structure FlowGraph : FLOWGRAPH
    structure Emitter   : EMITTER_NEW where P=FlowGraph.P and I=FlowGraph.I
   ) : PRINT_FLOW_GRAPH =
struct
   structure E = Emitter
   structure F = FlowGraph
   structure C = FlowGraph.C

   type regmaps = int Array.array ref list

   fun useStream s f x =
   let val oldS = ! AsmStream.asmOutStream
       val _    = AsmStream.asmOutStream := s
       val r    = f x
       val _    = AsmStream.asmOutStream := oldS
   in  r
   end

   fun printInts stream ints = let
     fun pr str = TextIO.output(stream, str)
     fun iter [] = ()
       | iter [i] = pr (Int.toString i)
       | iter (h::t) = (pr (Int.toString h ^ ", "); iter t)
   in iter ints
   end

   fun blockNum(F.BBLOCK{blknum, ...}) = blknum
     | blockNum(F.ENTRY{blknum, ...}) = blknum
     | blockNum(F.EXIT{blknum, ...}) = blknum

   fun printBlock stream regmap b = let
     fun display(F.PSEUDO pOp) = E.pseudoOp pOp
       | display(F.LABEL l)    = E.defineLabel l
       | display(F.ORDERED blks) = app display blks
       | display(F.BBLOCK{blknum, succ, pred, liveOut, liveIn, insns, name, ...}) = 
	 let 
	     fun pr str = TextIO.output(stream, str)
	     val prInts = printInts stream
	 in
	     (pr ("BLOCK " ^ Int.toString blknum ^ "(" ^F.B.toString name ^ ")\n");
	      pr ("\tlive in:  " ^ C.cellset2string (!liveIn) ^ "\n");
	      pr ("\tlive out: " ^ C.cellset2string (!liveOut) ^ "\n");
	      pr ("\tsucc:     "); prInts (map blockNum (!succ)); pr "\n";
	      pr ("\tpred:     "); prInts (map blockNum (!pred)); pr "\n";
	      app (fn i => E.emitInstr(i,regmap)) (rev (!insns)))
	 end
   in 
       useStream stream display b 
   end

   fun printCluster s msg (F.CLUSTER {blocks, regmap, entry, exit, ...}) = let
     fun pr str = TextIO.output(s, str)
     fun printEntry(F.ENTRY{blknum, succ}) = 
       (pr ("ENTRY " ^ Int.toString blknum ^ "\n");
        pr "\tsucc:     "; printInts s (map blockNum (!succ));	pr "\n")
	
     fun printExit(F.EXIT{blknum, pred}) = 
       (pr ("EXIT " ^ Int.toString blknum ^ "\n");
        pr "\tpred      "; printInts s (map blockNum (!pred));  pr "\n")
   in
     TextIO.output(s,"[ "^ msg ^" ]\n");
     printEntry entry;
     app (printBlock s regmap) blocks;
     printExit exit
   end
end

