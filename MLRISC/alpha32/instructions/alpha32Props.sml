(* alpha32Props.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor Alpha32Props
  (structure Alpha32Instr:ALPHA32INSTR
   structure Shuffle : ALPHA32SHUFFLE
     sharing Shuffle.I = Alpha32Instr):INSN_PROPERTIES =
struct
    structure I = Alpha32Instr
    structure C = I.C
    structure LE = LabelExp

    exception NegateConditional

    fun error msg = MLRiscErrorMsg.impossible ("alpha32Props."^msg)

    val zeroR = 31

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR
    datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
    fun instrKind(I.BRANCH _)  = IK_JUMP
      | instrKind(I.FBRANCH _) = IK_JUMP
      | instrKind(I.JMPL _)    = IK_JUMP
      | instrKind _            = IK_INSTR

    fun moveInstr(I.COPY _)  = true
      | moveInstr(I.FCOPY _) = true
      | moveInstr _	     = false

    val nop = 
      fn () => I.OPERATE{oper=I.BIS, ra=zeroR, rb=I.REGop zeroR, rc=zeroR}

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
    fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
      | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
      | moveTmpR _ = NONE

    fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.FCOPY{dst, src, ...}) = (dst, src)
      | moveDstSrc _ = error "moveDstSrc"

    fun copy{src, dst} = 
      I.COPY{src=src, dst=dst, impl=ref NONE, 
	     tmp=case src of [_] => NONE | _ => SOME(I.Direct(C.newReg()))}

    fun fcopy{src,dst} = let
      fun trans r = if r >= 32 andalso r < 64 then r-32 else r
      val src = map trans src 
      val dst = map trans dst
    in
      I.FCOPY{src=src,dst=dst,impl=ref NONE,
	      tmp=case src of [_] => NONE | _   => SOME(I.FDirect(C.newFreg()))}
    end

    fun splitCopies{regmap,insns} = let
      val shuffle = Shuffle.shuffle
      val shufflefp = Shuffle.shufflefp
      fun scan([],is') = rev is'
	| scan(I.COPY{dst,src,tmp,...}::is,is') = 
	    scan(is,shuffle{regMap=regmap,src=src,dst=dst,temp=tmp}@is')
	| scan(I.FCOPY{dst,src,tmp,...}::is,is') = 
	    scan(is,shufflefp{regMap=regmap,src=src,dst=dst,temp=tmp}@is')
	| scan(i::is,is') = scan(is,i::is')
    in scan(insns,[]) 
    end

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
    fun branchTargets(I.BRANCH(I.BR, _, lab)) = [LABELLED lab]
      | branchTargets(I.BRANCH(_, _, lab))  = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.FBRANCH(_, _, lab)) = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.JMPL(_,[]))       = [ESCAPES]
      | branchTargets(I.JMPL(_,labs))     = map LABELLED labs
      | branchTargets _ = error "branchTargets"

    fun jump label = I.BRANCH(I.BR,31,label)

    fun setTargets(I.BRANCH(I.BR,0,_),[L]) = I.BRANCH(I.BR,0,L)
      | setTargets(I.BRANCH(b,r,_),[F,T])  = I.BRANCH(b,r,T)
      | setTargets(I.FBRANCH(b,r,_),[F,T]) = I.FBRANCH(b,r,T)
      | setTargets(I.JMPL(x,_),labs)       = I.JMPL(x,labs)
      | setTargets(i,_) = i

    fun negateConditional br = let
      fun revBranch I.BEQ  = I.BNE 
	| revBranch I.BGE  = I.BLT 
	| revBranch I.BGT  = I.BLE 
	| revBranch I.BLE  = I.BGT 
	| revBranch I.BLT  = I.BGE 
	| revBranch I.BLBC = I.BLBS 
	| revBranch I.BLBS = I.BLBC 
	| revBranch _ = raise NegateConditional
    in
      case br
      of I.BRANCH(br,r,label) => I.BRANCH(revBranch br,r,label)
       | I.FBRANCH(br,r,label) => I.FBRANCH(revBranch br,r,label)
       | _ => raise NegateConditional
    end

   (*========================================================================
    *  Definition and use (for register allocation mainly)
    *========================================================================*)
    fun defUseR instr =
      let
	fun Oper {oper, ra, rb=I.REGop rb, rc} = ([rc], [ra, rb])
	  | Oper {oper, ra, rb, rc} = ([rc], [ra])
	fun FMem (freg, (rd, _)) = ([], [rd])
	fun trap (def,use) =(def, use)
      in
	case instr of
	  (* load/store instructions *)
	   I.LDA{r, b, ...} => ([r], [b])
	 | I.LDAH{r, b, ...} => ([r], [b])
	 | I.LOAD{r, b, ...} => ([r], [b])
         | I.STORE{r, b, ...} => ([], [r,b])
	 | I.FLOAD{b, ...} => ([], [b])
	 | I.FSTORE{b, ...} => ([], [b])
	 (* branch instructions *)
	 | I.JMPL ({r, b, ...},_) => ([r], [b])
	 | I.JSR({r, b, ...}, def, use) => (r:: #1 def, b:: #1 use)
	 | I.BRANCH(I.BR, reg, _) => ([reg], [])
	 | I.BRANCH(_, reg, _) => ([], [reg])
	 (* operate *)
	 | I.OPERATE arg => Oper arg
	 | I.PSEUDOARITH {oper, ra, rb=I.REGop rb, rc, tmps} => 
	     (rc:: #1 tmps, [ra, rb])
	 | I.PSEUDOARITH {oper, ra, rb, rc, tmps} => (rc:: #1 tmps, [ra])
	 | I.OPERATEV arg => trap(Oper arg)
	 (* copy *)
	 | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
	 | I.COPY{dst, src, ...} => (dst, src)
	 (* floating operate *)
	 | I.FOPERATEV _ => trap([], [])
	 | I.TRAPB 	=> trap([],[])
	 (* macro *)
	 | I.CALL_PAL{def,use, ...} => (def, use)

	 | _  		=> ([],[])
      end

    (* Use of FP registers *)
    fun defUseF instr =
      case instr of
	I.DEFFREG freg				=> ([freg], [])
      | I.FBRANCH(_, freg, lab)			=>  ([],[freg])
      | I.FLOAD{r, ...}				=> ([r], [])
      | I.FSTORE{r, ...}			=> ([], [r])
      | I.FOPERATE{fa, fb, fc, ...}		=> ([fc], [fa, fb])
      | I.PSEUDOARITH{tmps, ...}		=> (#2 tmps, [])
      | I.FOPERATEV{oper=I.CVTTQ, fa, fb, fc}   => ([fc], [fa, fb])
      | I.FOPERATEV{fa, fb, fc, ...}		=> ([fc], [fa, fb, fc])
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
      | I.FCOPY{dst, src, ...}			=> (dst, src) 
      | I.JSR(_,def,use) 	     => (#2 def,#2 use)
      | _ => ([],[])

    fun defUse C.GP = defUseR
      | defUse C.FP = defUseF
      | defUse _ = error "defUse"
end


(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:51  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:16  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:47  pscheng
# *** empty log message ***
#
 * Revision 1.3  1998/05/25 15:10:48  george
 *   Fixed RCS keywords
 *
 *)
