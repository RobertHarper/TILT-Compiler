(* flowgen.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)


signature FLOWGRAPH_GEN = sig
  
   structure F : FLOWGRAPH
   structure C : CELLS
   structure I : INSTRUCTIONS
   structure P : INSN_PROPERTIES
   structure T : MLTREE
   structure B : BLOCK_NAMES
   structure Pu : PSEUDO_OPS

   sharing I.C = C
   sharing F.I = P.I = I
   sharing T.Constant = I.Constant
   sharing T.PseudoOp = Pu

   val defineLabel   : Label.label -> unit
      (** define a label in the flowgraph **)

   val entryLabel    : Label.label -> unit
      (** define argument as being a label and entry point **)

   val pseudoOp : Pu.pseudo_op -> unit
      (** create a pseudo op in the flowgraph **)

   val emitInstr     : I.instruction -> unit
      (** emitInstr - either creates a new BBLOCK or adds the instruction
       **	to a BBLOCK that is being built locally.
       **       If the instruction is a branch, then the successor labels
       **	are noted in a hash table.
       ** Uses: I.branchTargets, I.instrKind
       **)

   val exitBlock  : T.mlrisc list -> unit
      (** exitBlock - associates the list of live registers with the last
       **	code block. The last instruction is usually a branch
       **	with no targets. If not it is assumed to be a label
       **	that will be linked in at some later time. The call
       **	to exitBlock had better reflect the correct list of live 
       **   registers that terminate the branch.
       **)

   val endCluster : int Intmap.intmap -> unit
       (** endCluster
	**	cleans up all local references and tables.
	**      creates the final flowgraph and calls the continuation.
	**)

   val ordered : T.mltree list -> unit
      (** ordered 
       **	creates an ordered list of pseudo-ops and labels, that
       **	must be kept together always.
       **)

   val blockName : B.name -> unit

   val beginCluster : unit -> unit
end

functor FlowGraphGen
  (structure Flowgraph : FLOWGRAPH
   structure InsnProps : INSN_PROPERTIES
   structure MLTree : MLTREE

   val optimize : (Flowgraph.cluster -> Flowgraph.cluster) option ref
   val output : Flowgraph.cluster -> unit
     sharing Flowgraph.I = InsnProps.I
     sharing MLTree.Constant = InsnProps.I.Constant
     sharing MLTree.PseudoOp = Flowgraph.P
     sharing Flowgraph.B = MLTree.BNames) : FLOWGRAPH_GEN = 
struct

  structure F = Flowgraph
  structure P = InsnProps
  structure I = Flowgraph.I
  structure C = I.C
  structure T = MLTree
  structure B = MLTree.BNames
  structure Pu = T.PseudoOp
  
  type label = Label.label

  fun error msg = MLRiscErrorMsg.impossible ("FlowGraph." ^ msg)

  val bblkCnt = ref 0 
  val entryLabels = ref ([] : Label.label list)
  val blkName  = ref B.default 
  val currBlock : F.block option ref = ref NONE
  val blockList : F.block list ref = ref []

  fun nextBlkNum () = !bblkCnt before bblkCnt := !bblkCnt + 1
  fun blockName name = 
    (case !currBlock
     of NONE => ()
      | SOME blk => 
         (currBlock := NONE; blockList := blk:: !blockList)
     (*esac*);
     blkName := name)

  (** Note - currBlock will always be a reference to a F.BLOCK{..} **)
  fun newBasicBlk init = 
      F.BBLOCK{blknum=nextBlkNum(),
	       name= !blkName,
	       liveIn=ref C.empty,
	       liveOut=ref C.empty,
	       succ=ref [],
	       pred=ref [],
	       insns=ref init}
  local
    fun blockListAdd b = let
      val blocks = !blockList
    in
      case !currBlock 
       of NONE => blockList := b::blocks
        | SOME blk => (blockList:=b::blk::blocks;  currBlock:=NONE)
    end
  in
    fun pseudoOp pOp  = blockListAdd (F.PSEUDO pOp)
    fun defineLabel lab = blockListAdd(F.LABEL lab)
    fun entryLabel lab = 
      (entryLabels := lab::(!entryLabels);  blockListAdd(F.LABEL lab))
    fun ordered(mlts)        = 
      blockListAdd
        (F.ORDERED(map (fn T.PSEUDO_OP pOp => F.PSEUDO pOp
	                 | T.DEFINELABEL lab => F.LABEL lab
			 | T.ENTRYLABEL lab => 
			     (entryLabels := lab :: !entryLabels;
			      F.LABEL lab)
		         | _ => error "ordered ")
                   mlts))
  end (*local*)

  (** emitInstr - instructions are always added to currBlock. **)
  fun emitInstr instr = let
    fun addInstr (NONE) = currBlock:=SOME(newBasicBlk [instr])
      | addInstr (SOME(F.BBLOCK{insns, ...})) = insns := instr::(!insns)
  in
    addInstr(!currBlock);
    case P.instrKind instr
     of P.IK_JUMP => 
         (blockList:= Option.valOf(!currBlock) :: (!blockList);
	  currBlock := NONE)
      | _ => ()
    (*esac*)
  end      


  fun exitBlock liveRegs  = let
    val addReg   = C.addCell C.GP
    val addFreg  = C.addCell C.FP
    val addCCreg = C.addCell C.CC
    (* we don't care about memory locations that may be live. *)
    fun live(T.GPR(T.REG r)::rest, acc) = live(rest, addReg(r, acc))
      | live(T.FPR(T.FREG f)::rest, acc) = live(rest, addFreg(f, acc))
      | live(T.CCR(T.CC c)::rest, acc) = live(rest, addCCreg(c, acc))
      | live(_::rest, acc) = live(rest, acc)
      | live([], acc) = acc

    val lout = live(liveRegs, C.empty)

    fun findCodeBlock(F.BBLOCK{liveOut,...}::_)  = liveOut
      | findCodeBlock(F.LABEL _::blks) = findCodeBlock blks
      | findCodeBlock _                = error "exitBlock.codeBlock"

  in
    case !currBlock
     of NONE => let 
	  val outRef = findCodeBlock (!blockList)
        in outRef := lout
	end
      | SOME(F.BBLOCK{liveOut, ...}) =>
	(liveOut := lout;
	 blockList := Option.valOf(!currBlock) :: (!blockList);
	 currBlock := NONE)
      | _ => error "exitBlock"
   (*esac*)
  end

  fun endCluster(regmap) = let
      val codegen = 
	(case !optimize
	  of NONE => output
	   | SOME optimizer => output o optimizer
         (*esac*))
      exception LabTbl
      val labTbl : F.block Intmap.intmap = Intmap.new(16, LabTbl)
      val addLabTbl = Intmap.add labTbl
      val lookupLabTbl = Intmap.map labTbl

      (* find next code block *)
      exception NextCodeBlock
      fun nextCodeBlock((blk as F.BBLOCK _)::_) = blk
	| nextCodeBlock(_::rest) = nextCodeBlock rest
	| nextCodeBlock [] = raise NextCodeBlock

      (* mapping of labels to code blocks *)
      fun fillLabTbl(F.LABEL lab::blks) = 
	    (addLabTbl(Label.id lab, nextCodeBlock blks) 
					handle NextCodeBlock => ();
	     fillLabTbl blks)
	| fillLabTbl(F.ORDERED labs::blks) = fillLabTbl(labs@blks)
	| fillLabTbl(_::blks) = fillLabTbl(blks)
	| fillLabTbl [] = ()

      val exitBlk = F.EXIT{blknum=nextBlkNum(), pred=ref []}

      (** update successor and predecessor information **)
      fun graphEdges((blk as F.BBLOCK{blknum,insns,succ,...})::blks) = let
	    fun updtPred(F.BBLOCK{pred, ...}) = pred := blk :: (!pred)
	      | updtPred(F.EXIT{pred, ...}) = pred := blk :: (!pred)

	    fun succBlks([], acc) = acc
	      | succBlks(P.FALLTHROUGH::labs, acc) =
	          ((succBlks(labs, nextCodeBlock blks::acc))
		    handle NextCodeBlock => error  "graphEdges.succBlks")
	      | succBlks(P.LABELLED lab::labs, acc) =
		  ((succBlks(labs, lookupLabTbl(Label.id lab)::acc))
		    handle LabTbl => 
		      succBlks(labs, exitBlk::acc))
	      | succBlks(P.ESCAPES::labs,acc) = 
		   succBlks(labs, exitBlk::acc)

	    val lastInstr = ((hd (!insns))
		     handle _ => error "endCluster.graphEdges.lastInstr")

	    fun lastCodeBlock(F.BBLOCK _ :: _) = false
	      | lastCodeBlock(_::rest) = lastCodeBlock rest
	      | lastCodeBlock [] = true
	  in
	    case P.instrKind lastInstr
	     of P.IK_JUMP => succ:=succBlks (P.branchTargets lastInstr,[])
	      | _  => 
	        if lastCodeBlock blks then
		  succ := [exitBlk] 	(* control must escape via trap *)
		else succ := [nextCodeBlock blks] 
	    (*esac*);
	    app updtPred (!succ);
	    graphEdges(blks)
	  end
 	| graphEdges(_::blks) = graphEdges(blks)
	| graphEdges [] = ()

      fun mkEntryBlock () = let
	val blocks = map (lookupLabTbl o Label.id) (!entryLabels)
	val entryBlk = F.ENTRY{blknum=nextBlkNum(), succ=ref blocks}
      in
	app (fn (F.BBLOCK{pred, ...}) => pred := entryBlk::(!pred)) blocks;
	entryBlk
      end

      val _ = case !currBlock
	of NONE => ()
         | SOME blk => blockList := blk :: !blockList

      val blocks = rev(!blockList) before blockList := []
      val _ = fillLabTbl(blocks)
      val _ = graphEdges(blocks)
      val res = codegen (F.CLUSTER{blocks=blocks, entry=mkEntryBlock(), exit=exitBlk,
				   blkCounter=ref(!bblkCnt), regmap=regmap}) 
    in res
    end

  fun beginCluster _ = 
    (entryLabels := [];
     bblkCnt := 0;
     blkName := B.default;
     currBlock := NONE)
end

(*
 * $Log$
# Revision 1.2  99/02/17  22:32:22  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  21:15:25  pscheng
# *** empty log message ***
#
# Revision 1.1  1999/02/17  20:06:57  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/11/16 21:47:21  george
 *   Version 110.10
 *
 * Revision 1.4  1998/07/25 03:08:17  george
 *   added to support block names in MLRISC
 *
 * Revision 1.3  1998/05/25 15:11:03  george
 *   Fixed RCS keywords
 *
 *)
