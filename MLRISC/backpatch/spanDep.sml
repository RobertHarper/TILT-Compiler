(* 
 * This version of the span dependency resolution also fill delay slots
 * using a few simple strategies.
 * 
 * Allen
 *)

functor SpanDependencyResolution
    (structure Flowgraph : FLOWGRAPH
     structure Jumps : SDI_JUMPS
     structure Emitter : EMITTER_NEW
     structure DelaySlot : DELAY_SLOT_PROPERTIES
     structure Props : INSN_PROPERTIES
       sharing Emitter.P = Flowgraph.P
       sharing Flowgraph.I = Jumps.I = Emitter.I = DelaySlot.I = Props.I)
         : BBSCHED =
struct

  structure F = Flowgraph
  structure I = F.I
  structure C = I.C
  structure E = Emitter
  structure J = Jumps
  structure P = Flowgraph.P
  structure D = DelaySlot
  structure A = Array

  fun error msg = MLRiscErrorMsg.impossible ("SpanDependencyResolution."^msg)

  datatype code =
      SDI of {size : int ref,		(* variable sized *)
	      insn : I.instruction}
    | FIXED of {size: int,		(* size of fixed instructions *)
		insns: I.instruction list}
    | BRANCH of {insn : code list,      (* instruction with delay slot*)
                 branchSize : int,
                 fillSlot : bool ref} 
    | DELAYSLOT of {insn : code list,    (* instruction in delay slot *)
                    fillSlot : bool ref}
    | CANDIDATE of (* two alternatives *)
      { oldInsns  : code list, (* without delay slot filling *)
        newInsns  : code list, (* when delay slot is filled *)
        fillSlot  : bool ref   (* should we fill the delay slot? *)
      }
   
  datatype compressed = 
      PSEUDO of P.pseudo_op
    | LABEL  of Label.label
    | CODE of Label.label * code list
    | CLUSTER of {comp : compressed list, regmap : int Intmap.intmap}

  val clusterList : compressed list ref = ref []
  fun cleanUp() = clusterList := []

  fun bbsched(cluster as F.CLUSTER{blocks, regmap, blkCounter, ...}) = 
  let fun lookup r = Intmap.map regmap r handle _ => r

      fun blknumOf(F.BBLOCK{blknum,...}) = blknum
        | blknumOf(F.EXIT{blknum,...}) = blknum
        | blknumOf _ = error "blknumOf"

      fun noPseudo [] = true
        | noPseudo (F.BBLOCK _::_) = true
        | noPseudo (F.LABEL _::rest) = noPseudo rest
        | noPseudo (F.ORDERED l::rest) = noPseudo(l@rest)
        | noPseudo (F.PSEUDO _::_) = false

      (* Maps blknum -> label at the position of the second instruction *)
      val dummy = Label.newLabel ""
      val labelMap = A.array(!blkCounter,dummy)

      (* enter labels into the labelMap *)
      fun enterLabels([]) = ()
        | enterLabels(F.PSEUDO _::rest) = enterLabels rest
        | enterLabels(F.LABEL _::rest) = enterLabels rest
        | enterLabels(F.ORDERED blks::rest) = enterLabels(blks@rest)
        | enterLabels(F.BBLOCK{blknum,...}::rest) = 
             (A.update(labelMap,blknum,Label.newLabel ""); enterLabels rest)
        | enterLabels _ = error "enterLabels"

      (* 
       * Find the branch target of block blknum, return the first instruction
       * in the target block and its associated label. 
       *)
      fun findTarget(blknum,[F.BBLOCK{blknum=x,insns=insns1,...},
                             F.BBLOCK{blknum=y,insns=insns2,...}]) =
          let fun extract(blknum,[]) = NONE
                | extract(blknum,[_]) = NONE
                | extract(blknum,[_,_]) = NONE
                | extract(blknum,insns) = 
                     SOME(List.last insns,A.sub(labelMap,blknum))
          in  if x = blknum + 1 then extract(y,!insns2)
              else if y = blknum + 1 then extract(x,!insns1)
              else NONE 
          end
        | findTarget _ = NONE


      (* Convert a cluster into compressed form *)
      fun compress(F.PSEUDO pOp::rest) = PSEUDO pOp::compress rest
        | compress(F.LABEL lab::rest) = LABEL lab:: compress rest
        | compress(F.ORDERED blks::rest) = compress(blks@rest)
        | compress(F.BBLOCK{blknum,insns,succ,...}::rest) = 
	let 
            val backward = List.exists (fn b => blknumOf b <= blknum) (!succ) 

            (* build the code list *)
            fun scan([],nonSdiInstrs,nonSdiSize,code) = 
                   group(nonSdiSize,nonSdiInstrs,code)
              | scan(instr::instrs,nonSdiInstrs,nonSdiSize,code) =
                let val {n,nOn,nOff,nop} = D.delaySlot{instr=instr,backward=backward}
                in  case (nOff,instrs) of
                        (D.D_ALWAYS,delaySlot::rest) => 
                        if D.delaySlotCandidate{jmp=instr,
                                                delaySlot=delaySlot} andalso
                           not(D.conflict{regmap=lookup,src=delaySlot,dst=instr}) 
                        then scan(rest,[],0,
                                  mkCandidate1(instr,delaySlot)::
                                  group(nonSdiSize,nonSdiInstrs,code))
                        else scanSdi(instr,instrs,nonSdiInstrs,nonSdiSize,code)
                    |  _ =>  scanSdi(instr,instrs,nonSdiInstrs,nonSdiSize,code)
                end
            and scanSdi(instr,instrs,nonSdiInstrs,nonSdiSize,code) =
                let val s = J.minSize instr
                in  if J.isSdi instr then
                         scan(instrs,[],0,SDI{size=ref s,insn=instr}::
                              group(nonSdiSize,nonSdiInstrs,code))
                    else scan(instrs,instr::nonSdiInstrs,nonSdiSize+s,code)
                end
            and group(0,[],code) = code
	      | group(size,insns,code) = FIXED{size=size,insns=insns}::code

            and buildList instrs = scan'(instrs,[],0,[])

            and scan'([],nonSdiInstrs,nonSdiSize,code) = 
                   group(nonSdiSize,nonSdiInstrs,code)
              | scan'(instr::instrs,nonSdiInstrs,nonSdiSize,code) =
                let val s = J.minSize instr
                in  if J.isSdi instr then
                         scan'(instrs,[],0,SDI{size=ref s,insn=instr}::
                               group(nonSdiSize,nonSdiInstrs,code))
                    else scan'(instrs,instr::nonSdiInstrs,nonSdiSize+s,code)
                end

            (* 
             * Create a branch delay slot candidate sequence.
             * jmp is the normal jump instruction; jmp' is the
             * jump instruction when the delay slot is active.
             *)
            and mkCandidate1(jmp,delaySlot) = 
                let val fillSlot = ref true
                    val jmp' = D.enableDelaySlot{n=false,nop=false,instr=jmp}
                in  CANDIDATE{newInsns= 
                                [BRANCH{branchSize=J.minSize jmp',
                                        insn=buildList [jmp'],
                                        fillSlot=fillSlot},
                                 DELAYSLOT{insn=buildList [delaySlot],
                                           fillSlot=fillSlot}],
                              oldInsns=buildList [jmp,delaySlot],
                              fillSlot=fillSlot}
                end 

            (* 
             * Create a branch delay slot candidate sequence.
             * jmp is the normal jump instruction; jmp' is the
             * jump instruction when the delay slot is active.
             *)
            and mkCandidate2(jmp,delaySlot,label) = 
                let val fillSlot = ref true
                    val jmp' = D.setTarget(
                                D.enableDelaySlot{n=true,nop=false,instr=jmp},
                                label)
                in  CANDIDATE{newInsns= 
                                [BRANCH{branchSize=J.minSize jmp',
                                        insn=buildList [jmp'],
                                        fillSlot=fillSlot},
                                 DELAYSLOT{insn=buildList [delaySlot],
                                           fillSlot=fillSlot}],
                              oldInsns=buildList [jmp],
                              fillSlot=fillSlot}
                end 

            (*
             * Try different strategies for delay slot filling
             *)
            and fitDelaySlot(jmp,body,instrs) =
            let val {n,nOn,nOff,nop} = D.delaySlot{instr=jmp,backward=backward}
                (* 
                 * Use the previous instruction to fill the delay slot 
                 *)
                fun strategy1() =
                    case (nOff,body) of
                       (D.D_ALWAYS,delaySlot::body) => 
                        if not(D.delaySlotCandidate{jmp=jmp,
                                                   delaySlot=delaySlot}) orelse
                           D.conflict{regmap=lookup,src=delaySlot,dst=jmp} 
                        then strategy2()
                        else scan(body,[],0,[mkCandidate1(jmp,delaySlot)])
                    | _ => strategy2()
                (* 
                 * Use the first instruction in the target block to fill
                 * the delay slot 
                 *)
                and strategy2() =
                    case (nOn,findTarget(blknum,!succ)) of
                      (D.D_TAKEN,SOME(delaySlot,label)) => 
                        if not(D.delaySlotCandidate{jmp=jmp,
                                                   delaySlot=delaySlot}) orelse
                           D.conflict{regmap=lookup,src=delaySlot,dst=jmp}
                        then strategy3()
                        else scan(body,[],0,[mkCandidate2(jmp,delaySlot,label)])
                    | _ => strategy3()
                (*
                 * No delay slot filling
                 *)
                and strategy3() = scan(instrs,[],0,[])
            in  strategy1()
            end

            (*
             * Try to remove the branch if it is unnecessary 
             *)
            and processBranch(jmp,body,instrs) =
                  case (!succ, noPseudo rest) of
                     ([F.BBLOCK{blknum=id,...}],true) =>
                        if id = blknum + 1 then scan(body,[],0,[])
                        else fitDelaySlot(jmp,body,instrs)
                  |  _ => fitDelaySlot(jmp,body,instrs)

            and process([],others) = others
              | process(instrs as jmp::body,others) =
                CODE(A.sub(labelMap,blknum),
                     case Props.instrKind jmp of
                       Props.IK_JUMP => processBranch(jmp,body,instrs)
                     | _ => scan(instrs,[],0,[])
                    )::others
	in 
	    process(!insns,compress rest)
	end
      | compress [] = []
  in  enterLabels blocks;
      clusterList:=CLUSTER{comp = compress blocks, regmap=regmap}:: 
		    (!clusterList)
  end

  fun finish() = let
    fun labels(PSEUDO pOp::rest, pos) = 
          (P.adjustLabels(pOp, pos); labels(rest, pos+P.sizeOf(pOp,pos)))
      | labels(LABEL lab::rest, pos) = 
	 (Label.setAddr(lab,pos); labels(rest, pos))
      | labels(CODE(lab,code)::rest, pos) = let
	  fun size(FIXED{size, ...}) = size
	    | size(SDI{size, ...}) = !size
            | size(BRANCH{insn,...}) = sizeList(insn,0)
            | size(DELAYSLOT{insn,...}) = sizeList(insn,0)
            | size(CANDIDATE{oldInsns,newInsns,fillSlot,...}) =
                sizeList(if !fillSlot then newInsns else oldInsns,0)
          and sizeList([],n) = n
            | sizeList(code::rest,n) = sizeList(rest,size code + n)
	in  Label.setAddr(lab,pos+4);
            labels(rest, sizeList(code,pos))
	end
      | labels(CLUSTER{comp, ...}::rest, pos) = labels(rest, labels(comp,pos))
      | labels([], pos) = pos

    val delaySlotSize = D.delaySlotSize

    fun adjust(CLUSTER{comp, regmap}::cluster, pos, changed) = 
    let fun scan(PSEUDO pOp::rest, pos, changed) = 
              scan(rest, pos+P.sizeOf(pOp,pos), changed)
	  | scan(LABEL _::rest, pos, changed) = scan(rest, pos, changed)
	  | scan(CODE(_,code)::rest, pos, changed) = 
              let val (pos,changed) = doCode(code,pos,changed)
              in  scan(rest,pos,changed) end
	  | scan([], pos, changed) = adjust(cluster, pos, changed)
        and doCode([],pos,changed) = (pos,changed)
          | doCode(code::rest,pos,changed) =
            case code of
              FIXED{size,...} => doCode(rest,pos+size,changed)
            | SDI{size, insn} =>
              let val newSize = J.sdiSize(insn, regmap, Label.addrOf, pos)
 	      in  if newSize <= !size then 
                     doCode(rest,!size + pos,changed)
		  else (size:=newSize; doCode(rest, newSize+pos, true))
              end
            | DELAYSLOT{insn,fillSlot,...} => 
                let val (newPos,changed) = doCode(insn,pos,changed)
                in  doCode(rest, newPos,
                           if newPos - pos <> delaySlotSize then 
                           (fillSlot := false; true) else changed)
                end
            | BRANCH{insn,branchSize,fillSlot,...} => 
                let val (newPos,changed) = doCode(insn,pos,changed)
                in  doCode(rest, newPos,
                           if newPos - pos <> branchSize then
                           (fillSlot := false; true) else changed)
                end
            | CANDIDATE{oldInsns,newInsns,fillSlot,...} =>
                doCode((if !fillSlot then newInsns else oldInsns) @ rest,
                       pos,changed)
    in  scan(comp, pos, changed)
    end
      | adjust(_::_, _, _) = error "adjust"
      | adjust([], _, changed) = changed

    fun fixpoint zl = let 
      val size = labels(zl, 0)
    in if adjust(zl, 0, false) then fixpoint zl else size
    end

    fun emitCluster(CLUSTER{comp, regmap}) = let
      fun emit(PSEUDO pOp) = E.pseudoOp pOp
	| emit(LABEL lab) = E.defineLabel lab
	| emit(CODE(_,code)) = let
	    fun emitInstrs insns = app (fn i => E.emitInstr(i, regmap)) insns
	    fun e(FIXED{insns, ...}) = emitInstrs insns
	      | e(SDI{size, insn}) = emitInstrs(J.expand(insn, !size))
              | e(BRANCH{insn,...}) = app e insn
              | e(DELAYSLOT{insn,...}) = app e insn
              | e(CANDIDATE{newInsns,oldInsns,fillSlot,...}) =
                  app e (if !fillSlot then newInsns else oldInsns)
	  in app e code
	  end
    in app emit comp
    end

    val compressed = (rev (!clusterList)) before cleanUp()
  in
    E.init(fixpoint compressed);
    app emitCluster compressed
  end (*finish*)

end (* spanDep.sml *)

