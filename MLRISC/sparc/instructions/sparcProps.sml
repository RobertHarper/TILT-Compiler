functor SparcProps 
  (structure SparcInstr : SPARCINSTR
   structure Shuffle : SPARCSHUFFLE
      sharing Shuffle.I = SparcInstr) : INSN_PROPERTIES =
struct
  structure I = SparcInstr
  structure C = I.C
(*
  structure A = I.A
*)

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.impossible ("sparcProps."^msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  (*========================================================================
   *  Instruction Kinds
   *========================================================================*)
  fun instrKind(I.Bicc _)  = IK_JUMP
    | instrKind(I.FBfcc _) = IK_JUMP
    | instrKind(I.JMP _)   = IK_JUMP
(*
    | instrKind(I.ANNOTATION(i,_)) = instrKind i
*)
    | instrKind _          = IK_INSTR

  fun branchTargets(I.Bicc{b=I.BA,label,...}) = [LABELLED label]
    | branchTargets(I.Bicc{label,...}) = [LABELLED label, FALLTHROUGH] 
    | branchTargets(I.FBfcc{b=I.FBA,label,...}) = [LABELLED label]
    | branchTargets(I.FBfcc{label,...}) = [LABELLED label, FALLTHROUGH]
    | branchTargets(I.JMP{labs=[],...}) = [ESCAPES]
    | branchTargets(I.JMP{labs,...})    = map LABELLED labs
(*
    | branchTargets(I.ANNOTATION(i,_)) = branchTargets i
*)
    | branchTargets _ = error "branchTargets"

  fun setTargets(I.Bicc{b=I.BA,a,nop,...},[L]) = 
          I.Bicc{b=I.BA,a=a,label=L,nop=nop}
    | setTargets(I.Bicc{b,a,nop,...},[F,T]) = 
          I.Bicc{b=b,a=a,label=T,nop=nop}
    | setTargets(I.FBfcc{b,a,nop,...},[F,T]) = 
          I.FBfcc{b=b,a=a,label=T,nop=nop}
    | setTargets(I.JMP{r,i,nop,...},labels) = 
          I.JMP{r=r,i=i,labs=labels,nop=nop}
(*
    | setTargets(I.ANNOTATION(i,a),labs) = I.ANNOTATION(setTargets(i,labs),a)
*)
    | setTargets(i,_) = i

  fun negateConditional(I.Bicc{b,a,label,nop}) =
         I.Bicc{b=I.revCond b,a=a,label=label,nop=nop}
    | negateConditional(I.FBfcc{b,a,label,nop}) =
         I.FBfcc{b=I.revFcond b,a=a,label=label,nop=nop} 
(*
    | negateConditional(I.ANNOTATION(i,a)) = 
         I.ANNOTATION(negateConditional i,a)
*)
    | negateConditional _ = raise NegateConditional

  fun jump label = I.Bicc{b=I.BA,a=true,label=label,nop=true}

  fun moveInstr(I.COPY _)  = true
    | moveInstr(I.FCOPY _) = true
(*
    | moveInstr(I.ANNOTATION(i,_)) = moveInstr i
*)
    | moveInstr _          = false

  fun nop() = I.SETHI{d=0, i=0}

  (*========================================================================
   *  Parallel Move
   *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r),...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f),...}) = SOME f
(*
    | moveTmpR(I.ANNOTATION(i,_)) = moveTmpR i
*)
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{dst,src,...}) = (dst,src)
    | moveDstSrc(I.FCOPY{dst,src,...}) = (dst,src)
(*
    | moveDstSrc(I.ANNOTATION(i,_)) = moveDstSrc i
*)
    | moveDstSrc _ = error "moveDstSrc"

  fun copy{src,dst} =
     I.COPY{src=src,dst=dst,impl=ref NONE,
            tmp=case src of [_] => NONE | _ => SOME(I.Direct(C.newReg()))}

  fun fcopy{dst,src} = let
  fun trans r = if r >= 32 andalso r < 64 then r-32 else r
      val src = map trans src
      val dst = map trans dst
  in
      I.FCOPY{dst=dst,src=src,impl=ref NONE,
              tmp=case src of [_] => NONE | _   => SOME(I.FDirect(C.newFreg()))}
  end

  fun splitCopies{regmap, insns} = let
    val shuffle = Shuffle.shuffle
    val shufflefp = Shuffle.shufflefp
    fun scan([],is') = rev is'
      | scan(I.COPY{dst, src, tmp,...}::is,is') =
          scan(is, shuffle{regMap=regmap,temp=tmp,dst=dst,src=src}@is')
      | scan(I.FCOPY{dst, src, tmp,...}::is,is') =
          scan(is, shufflefp{regMap=regmap,temp=tmp,dst=dst,src=src}@is')
      | scan(i::is, is') = scan(is, i::is')
  in scan(insns,[])
  end


  fun defUseR instr =
    let
       fun oper (I.REG r,def,use) = (def,r::use)
         | oper (_,def,use)       = (def,use)
    in
	case instr of
	  (* load/store instructions *)
          I.LOAD {r,d,i,...} => oper(i,[d],[r])
        | I.STORE {r,d,i,...} => oper(i,[],[r,d])
        | I.FLOAD {r,d,i,...} => oper(i,[],[r])
        | I.FSTORE {r,d,i,...} => oper(i,[],[r])
        | I.SETHI {d,...} => ([d],[])
        | I.ARITH {r,i,d,...} => oper(i,[d],[r])
        | I.SHIFT {r,i,d,...} => oper(i,[d],[r])
        | I.JMPL{defs,uses,d,r,i,...} => oper(i,d:: #1 defs,r:: #1 uses)
        | I.CALL{defs,uses,...} => (15 :: #1 defs, #1 uses)
        | I.JMP{r,i,...} => oper(i,[],[r])
        | I.RET{leaf=false,...} => ([],[31])
        | I.RET{leaf=true,...} => ([],[15])
        | I.COPY{src,dst,tmp=SOME(I.Direct r),...} => (r::dst,src)
        | I.COPY{src,dst,...} => (dst,src)
        | I.SAVE{r,i,d} => oper(i,[d],[r])
        | I.RESTORE{r,i,d} => oper(i,[d],[r])
        | I.Ticc{r,i,...} => oper(i,[],[r]) 
        | I.RDY{d,...} => ([d],[]) 
        | I.WRY{r,i,...} => oper(i,[],[r]) 
(*
        | I.ANNOTATION(i,_) => defUseR i
*)
        | _ => ([],[])  
    end

  (* Use of FP registers *)
  fun defUseF instr =
      case instr of
        I.FLOAD{r,d,i,...} => ([d],[])
      | I.FSTORE{r,d,i,...} => ([],[d])
      | I.FPop1{r,d,...} => ([d],[r])
      | I.FPop2{r1,r2,d,...} => ([d],[r1,r2])
      | I.FCMP{r1,r2,...} => ([],[r1,r2])
      | I.JMPL{defs,uses,...} => (#2 defs,#2 uses)
      | I.CALL{defs,uses,...} => (#2 defs,#2 uses)
      | I.FCOPY{src,dst,tmp=SOME(I.FDirect r),...} => (r::dst,src)
      | I.FCOPY{src,dst,...} => (dst,src)
(*
      | I.ANNOTATION(i,_) => defUseF i
*)
      | _ => ([],[])

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse _    = error "defUse"

(*
  fun annotate(i,a) = I.ANNOTATION(i,a)
  fun annotations i =
      let fun f(I.ANNOTATION(i,a),l) = f(i,a::l)
            | f(i,l) = (i,l)
      in  f(i,[]) end
*)

end



