(* liveness.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** liveness.sml - computes live variables **)

signature LIVENESS = sig

  structure F : FLOWGRAPH

  val liveness : F.block list * (int -> int) -> F.block list
end


functor Liveness
    (structure Flowgraph : FLOWGRAPH
     structure Instruction : INSTRUCTIONS
     val defUse : Instruction.instruction -> int list * int list
     val cellset : Instruction.C.cellset * int list -> Instruction.C.cellset
     val regSet  : Instruction.C.cellset -> int list
     sharing Flowgraph.I = Instruction) : LIVENESS = 
struct

  structure F  = Flowgraph
  structure SL = SortedList

  fun error msg = MLRiscErrorMsg.impossible ("Liveness."^msg)

  fun prList(l,msg:string) = let
      fun pr([]) = print "\n"
	| pr(x::xs) = (print(Int.toString x ^ " "); pr xs)
    in print msg; pr l
    end

  fun liveness(blocks,regmap) = let
      val _ = print "Perry: ra/liveness.sml liveness start\n"
      fun codeBlocks [] = []
	| codeBlocks((blk as F.BBLOCK _)::blks) = blk::codeBlocks blks
	| codeBlocks(_::blks) = codeBlocks blks

      fun dataflow blkArr = let
          val _ = print "Perry: ra/liveness.sml liveness:dataflow start\n"
          val M      			    = Array.length blkArr
	  val useArr : int list Array.array = Array.array(M,[])
	  val defArr : int list Array.array = Array.array(M,[])

          val _ = (print "Perry: ra/liveness.sml liveness:dataflow M = ";
		   print (Int.toString M); print "\n")

	  fun listNeq([],[]) = false
	    | listNeq((x:int)::xs,y::ys) = x<>y orelse listNeq(xs,ys)
	    | listNeq _ = true

	  fun uniqMap([],sl) = sl
	    | uniqMap(x::xs,sl) = uniqMap(xs,SL.enter(regmap(x),sl))
	    
	  fun init ~1 = ()
            | init n  = let
	        val _ = print "Perry: ra/liveness.sml liveness:dataflow:init start\n"
		val temp = Array.sub(blkArr,n)
	        val _ = print "Perry: ra/liveness.sml liveness:dataflow:init 0.5\n"
		val F.BBLOCK temp2 = temp
	        val _ = print "Perry: ra/liveness.sml liveness:dataflow:init 0.6\n"
		val {blknum,insns,liveIn,...} = temp2
	        val _ = print "Perry: ra/liveness.sml liveness:dataflow:init 1\n"
		fun defuse(insn::insns,def,use) = let
		      val _ = print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 1.start\n"
		      val (d,u) = defUse insn
		      val _ = print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 1.1\n"
		      val u' = SL.difference(uniqMap(u,[]),def)
		      val _ = print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 1.2\n"
		      val use' = SL.merge(u', use)
		      val _ = print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 1.3\n"
		      val d' = SL.difference(uniqMap(d,[]),use')
		      val _ = print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 1.4\n"
		    in
		      defuse(insns, SL.merge(d',def), use')
		    end
		  | defuse([],def,use) = 
		      (print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 2.1\n";
		       Array.update(useArr,blknum,use);
		       print "Perry: ra/liveness.sml liveness:dataflow:init:defuse 2.2\n";
		       Array.update(defArr,blknum,def))
	      in
		  defuse(rev(!insns),[],[]);
		  print "Perry: ra/liveness.sml liveness:dataflow:init 2\n";
		  liveIn:=cellset(!liveIn,[]);
		  print "Perry: ra/liveness.sml liveness:dataflow:init 3\n";
		  init(n-1)
	      end

	  fun outB(F.BBLOCK{succ=ref [], ...}) = false
	    | outB(F.BBLOCK{succ=ref [F.EXIT _], ...}) = false
	    | outB(F.BBLOCK{succ, liveOut,...}) = let
		fun inSuccs([], acc) = acc
		  | inSuccs(F.EXIT _::sl, acc) = inSuccs(sl, acc)
		  | inSuccs(F.BBLOCK{blknum,liveIn,...}::sl, acc) = 
		      inSuccs(sl, SL.merge(regSet(!liveIn), acc))
		val liveout = inSuccs(!succ, [])
		val change = listNeq(regSet(!liveOut),liveout)
	      in liveOut:=cellset(!liveOut,liveout); change
	      end
	    | outB _ = error "liveness.dataflow.outB"

	  fun inB(F.BBLOCK{blknum,liveIn,liveOut,...}) = let
	      val use    = Array.sub(useArr,blknum)
	      val def    = Array.sub(defArr,blknum)
	      val livein = SL.merge(use,SL.difference(regSet(!liveOut),def))
	      val change = listNeq(regSet(!liveIn),livein)
	    in
	      liveIn := cellset(!liveIn,livein); change
	    end
	    | inB _ = error "liveness.dataflow.inB"

	  fun bottomup() = let
	      val visited = Array.array(M,false)
	      fun visit(n, changed) = let
		  fun visitSucc([],changed') = changed'
		    | visitSucc(F.EXIT _::ns, changed') =
		       visitSucc(ns, changed')
		    | visitSucc(F.BBLOCK{blknum=n, ...}::ns,changed') =
		       if Array.sub(visited,n) then visitSucc(ns,changed')
		       else visitSucc(ns,visit(n,changed'))

		  val block as(F.BBLOCK{succ,...}) = Array.sub(blkArr, n)
		  val _ = Array.update(visited,n,true)

		  val changed' = visitSucc(!succ,changed);
		  val change1 = outB block
		  val change2 = inB block
		in
		  changed' orelse change1 orelse change2
		end
	      fun visitAll(n,last,changed) = 
		  if n=last then changed
		  else if Array.sub(visited,n) then visitAll(n+1,last,changed)
		       else visitAll(n+1,last,visit(n,changed))
            in
	      visitAll(0,M,false)
	    end

	  fun repeat n = if bottomup() then repeat(n+1) else (n+1)
	in
	    print "Perry: ra/liveness.sml liveness:dataflow 1\n";
	    init (M-1); 
	    print "Perry: ra/liveness.sml liveness:dataflow 2\n";
	    repeat 0
	end  
      val _ = print "Perry: ra/liveness.sml liveness 2\n"

      val temp = codeBlocks blocks

      val _ = (print "Perry: ra/liveness.sml liveness length temp = ";
	       print (Int.toString (length temp)); print "\n")
    in 
	dataflow (Array.fromList temp);
	print "Perry: ra/liveness.sml liveness 3\n";
	blocks
    end
end


(*
 * $Log$
# Revision 1.1  99/02/17  21:17:31  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:31  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
