(* alpha32.sml 
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * generates machine code from the mltree.
 *
 *)

(** NOTE: f29 and f30 are used as temporaries.
 **       r31 is always zero.
 **)
functor Alpha32
  (structure Alpha32Instr : ALPHA32INSTR
   structure Alpha32MLTree : MLTREE where Region = Alpha32Instr.Region
                                    and Constant = Alpha32Instr.Constant
   structure Flowgen : FLOWGRAPH_GEN where I = Alpha32Instr
                                     and T = Alpha32MLTree
                                     and B = Alpha32MLTree.BNames
   structure PseudoInstrs : ALPHA32_PSEUDO_INSTR where I = Alpha32Instr
(* DBM: sharing/defn conflict:
     sharing Alpha32Instr.Region = Alpha32MLTree.Region
     sharing Flowgen.I = PseudoInstrs.I =  Alpha32Instr
     sharing Flowgen.T=Alpha32MLTree 
     sharing Alpha32MLTree.Constant = Alpha32Instr.Constant
     sharing Alpha32MLTree.BNames = Flowgen.B
*)
  ) : MLTREECOMP = 
struct
  structure F = Flowgen
  structure T = Alpha32MLTree
  structure R = Alpha32MLTree.Region
  structure I = Alpha32Instr
  structure C = Alpha32Instr.C
  structure LE = LabelExp
  structure W32 = Word32

 (*********************************************************

       Trap Shadows, Floating Exceptions, and Denormalized
	Numbers on the DEC Alpha

		Andrew W. Appel and Lal George
		  Nov 28, 1995

  See section 4.7.5.1 of the Alpha Architecture Reference Manual.

  The Alpha has imprecise exceptions, meaning that if a floating
  point instruction raises an IEEE exception, the exception may
  not interrupt the processor until several successive instructions have
  completed.  ML, on the other hand, may want a "precise" model
  of floating point exceptions.

  Furthermore, the Alpha hardware does not support denormalized numbers
  (for "gradual underflow").  Instead, underflow always rounds to zero.
  However, each floating operation (add, mult, etc.) has a trapping
  variant that will raise an exception (imprecisely, of course) on
  underflow; in that case, the instruction will produce a zero result
  AND an exception will occur.  In fact, there are several variants
  of each instruction; three variants of MULT are:

  MULT  s1,s2,d       truncate denormalized result to zero; no exception
  MULT/U  s1,s2,d     truncate denormalized result to zero; raise UNDERFLOW
  MULT/SU  s1,s2,d    software completion, producing denormalized result

  The hardware treats the MULT/U and MULT/SU instructions identically,
  truncating a denormalized result to zero and raising the UNDERFLOW
  exception.  But the operating system, on an UNDERFLOW exception,
  examines the faulting instruction to see if it's an /SU form, and if so,
  recalculates s1*s2, puts the right answer in d, and continues,
  all without invoking the user's signal handler.

  Because most machines compute with denormalized numbers in hardware,
  to maximize portability of SML programs, we use the MULT/SU form.
  (and ADD/SU, SUB/SU, etc.)  But to use this form successfully,
  certain rules have to be followed.  Basically, d cannot be the same
  register as s1 or s2, because the opsys needs to be able to 
  recalculate the operation using the original contents of s1 and s2,
  and the MULT/SU instruction will overwrite d even if it traps.

  More generally, we may want to have a sequence of floating-point
  instructions.  The rules for such a sequence are:

  1. The sequence should end with a TRAPB (trap barrier) instruction.
     (This could be relaxed somewhat, but certainly a TRAPB would
      be a good idea sometime before the next branch instruction or
      update of an ML reference variable, or any other ML side effect.)
  2. No instruction in the sequence should destroy any operand of itself
     or of any previous instruction in the sequence.
  3. No two instructions in the sequence should write the same destination
     register.

  We can achieve these conditions by the following trick in the
  Alpha code generator.  Each instruction in the sequence will write
  to a different temporary; this is guaranteed by the translation from
  ML-RISC.  At the beginning of the sequence, we will put a special
  pseudo-instruction (we call it DEFFREG) that "defines" the destination
  register of the arithmetic instruction.  If there are K arithmetic
  instructions in the sequence, then we'll insert K DEFFREG instructions
  all at the beginning of the sequence.
  Then, each arithop will not only "define" its destination temporary
  but will "use" it as well.  When all these instructions are fed to
  the liveness analyzer, the resulting interference graph will then
  have inteference edges satisfying conditions 2 and 3 above.

  Of course, DEFFREG doesn't actually generate any code.  In our model
  of the Alpha, every instruction generates exactly 4 bytes of code
  except the "span-dependent" ones.  Therefore, we'll specify DEFFREG
  as a span-dependent instruction whose minimum and maximum sizes are zero.

  At the moment, we do not group arithmetic operations into sequences;
  that is, each arithop will be preceded by a single DEFFREG and
  followed by a TRAPB.  To avoid the cost of all those TRAPB's, we
  should improve this when we have time.  Warning:  Don't put more 
  than 31 instructions in the sequence, because they're all required
  to write to different destination registers!  

  What about multiple traps?  For example, suppose a sequence of
  instructions produces an Overflow and  a Divide-by-Zero exception?
  ML would like to know only about the earliest trap, but the hardware
  will report BOTH traps to the operating system.  However, as long
  as the rules above are followed (and the software-completion versions
  of the arithmetic instructions are used), the operating system will
  have enough information to know which instruction produced the
  trap.  It is very probable that the operating system will report ONLY
  the earlier trap to the user process, but I'm not sure.

  For a hint about what the operating system is doing in its own
  trap-handler (with software completion), see section 6.3.2 of
  "OpenVMS Alpha Software" (Part II of the Alpha Architecture
  Manual).  This stuff should apply to Unix (OSF1) as well as VMS.

  ****************************************************************)

  fun error msg = MLRiscErrorMsg.impossible ("Alpha32." ^ msg)

  val itow = Word.fromInt
  val wtoi = Word.toIntX

  val emitInstr = F.emitInstr
  val emit = F.emitInstr

  val zeroR = 31
  val zeroOp = I.REGop zeroR
  val zeroEA = I.Direct zeroR
  val argReg0 = 16

  val mem = R.memory

  fun cond T.LT  = I.CC_LT    | cond T.LTU = I.CC_LTU
    | cond T.LE  = I.CC_LE    | cond T.LEU = I.CC_LEU
    | cond T.EQ  = I.CC_EQ    | cond T.NEQ = I.CC_NEQ
    | cond T.GE  = I.CC_GE    | cond T.GEU = I.CC_GEU
    | cond T.GT  = I.CC_GT    | cond T.GTU = I.CC_GTU

  fun swapcc I.CC_LT  = I.CC_GT    | swapcc I.CC_LTU = I.CC_GTU
    | swapcc I.CC_LE  = I.CC_GE    | swapcc I.CC_LEU = I.CC_GEU
    | swapcc I.CC_EQ  = I.CC_EQ    | swapcc I.CC_NEQ = I.CC_NEQ   
    | swapcc I.CC_GT  = I.CC_LT    | swapcc I.CC_GEU = I.CC_LEU
    | swapcc I.CC_GE  = I.CC_LE    | swapcc _ = error "swapcc"	

  (* NOTE: stack allocation must be multiples of 16 *)
  fun stackAllocate(n) = 
    I.OPERATE{oper=I.SUBL, ra=C.stackptrR, rb=I.IMMop n, rc=C.stackptrR}

  fun stackDeallocate(n) =
    I.OPERATE{oper=I.ADDL, ra=C.stackptrR, rb=I.IMMop n, rc=C.stackptrR}

  fun loadImmed(n, base, rd) = 
    if ~32768 <= n andalso n < 32768 then
      emit(I.LDA{r=rd, b=base, d=I.IMMop n})
    else let
        val w = itow n
	val hi = Word.~>>(w, 0w16)
	val lo = Word.andb(w, 0w65535)
	val (hi', lo') =  
	  if lo < 0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
      in
	emit(I.LDA{r=rd, b=base, d=I.IMMop(wtoi lo')});
	emit(I.LDAH{r=rd, b=rd, d=I.IMMop(wtoi hi')})
      end

  (* loadImmed32 is used to load int32 and word32 constants.
   * In either case we sign extend the 32-bit value. This is compatible 
   * with LDL which sign extends a 32-bit valued memory location.
   *)
  fun loadImmed32(0w0, base, rd) = 
       emit(I.OPERATE{oper=I.ADDL, ra=base, rb=zeroOp, rc=rd})
    | loadImmed32(n, base, rd) = let
	val low = W32.andb(n, 0w65535)	(* unsigned (0 .. 65535) *)
	val high = W32.~>>(n, 0w16)	(* signed (~32768 .. 32768] *)
	fun loadimmed(0, high) = emit(I.LDAH{r=rd, b=base, d=I.IMMop(high)})
	  | loadimmed(low, 0) = emit(I.LDA{r=rd, b=base, d=I.IMMop(low)})
	  | loadimmed(low, high) =
	     (emit(I.LDA{r=rd, b=base, d=I.IMMop(low)});
	      emit(I.LDAH{r=rd, b=rd, d=I.IMMop(high)}))
      in 
	if W32.<(low, 0w32768) then loadimmed(W32.toInt low, W32.toIntX high)
	else let (* low = (32768 .. 65535) *)
	   val lowsgn = W32.-(low, 0w65536) (* signed (~1 .. ~32768)  *)
	   val highsgn = W32.+(high, 0w1)   (* (~32768 .. 32768) *)
	   val ilow = W32.toIntX lowsgn
	   val ihigh = W32.toIntX highsgn
	 in
	   if ihigh <> 32768 then loadimmed(ilow, ihigh)
	   else let 
	       val tmpR = C.newReg()
	     in
	       (* you gotta do what you gotta do! *)
	       emit(I.LDA{r=rd, b=base, d=I.IMMop(ilow)});
	       emit(I.OPERATE{oper=I.ADDL, ra=zeroR, rb=I.IMMop 1, rc=tmpR}); 
	       emit(I.OPERATE{oper=I.SLL, ra=tmpR, rb=I.IMMop 31, rc=tmpR});
	       emit(I.OPERATE{oper=I.ADDL, ra=tmpR, rb=I.REGop rd, rc=rd})
	     end
	 end
       end

  fun orderedFArith (exp1, exp2, T.LR) = (fregAction exp1, fregAction exp2)
    | orderedFArith (exp1, exp2, T.RL) = let
        val f2 = fregAction exp2
      in (fregAction exp1, f2)
      end

  and stmAction exp = let
    fun fbranch(_, T.FCMP(cc, exp1, exp2, order), lab) = let
      val (f1, f2) = orderedFArith(exp1, exp2, order)
      fun bcc(cmp, br) = let
	val tmpR = C.newFreg()
      in
	emit(I.DEFFREG(tmpR));
	emit(I.FOPERATE{oper=cmp, fa=f1, fb=f2, fc=tmpR});
	emit(I.TRAPB); 
 	emit(I.FBRANCH(br, tmpR, lab))
      end

      fun fall(cmp1, br1, cmp2, br2) = let
	val tmpR1 = C.newFreg()
	val tmpR2 = C.newFreg()
	val fallLab = Label.newLabel ""
      in
	emit(I.DEFFREG(tmpR1));
	emit(I.FOPERATE{oper=cmp1, fa=f1, fb=f2, fc=tmpR1});
	emit(I.TRAPB);
	emit(I.FBRANCH(br1, tmpR1, fallLab));
	emit(I.DEFFREG(tmpR2));	     
	emit(I.FOPERATE{oper=cmp2, fa=f1, fb=f2, fc=tmpR2});
	emit(I.TRAPB);
	emit(I.FBRANCH(br2, tmpR2, lab));
	F.defineLabel fallLab
      end	

      fun bcc2(cmp1, br1, cmp2, br2) = (bcc(cmp1, br1); bcc(cmp2, br2))
    in
      case cc
      of T.==  => bcc(I.CMPTEQ, I.BNE)
       | T.?<> => bcc(I.CMPTEQ, I.BEQ)
       | T.?   => bcc(I.CMPTUN, I.BNE)
       | T.<=> => bcc(I.CMPTUN, I.BEQ)
       | T.>   => fall(I.CMPTLE, I.BNE, I.CMPTUN, I.BEQ)
       | T.>=  => fall(I.CMPTLT, I.BNE, I.CMPTUN, I.BEQ)
       | T.?>  => bcc(I.CMPTLE, I.BEQ)
       | T.?>= => bcc(I.CMPTLT, I.BEQ)
       | T.<   => bcc(I.CMPTLT, I.BNE)
       | T.<=  => bcc(I.CMPTLE, I.BNE)
       | T.?<  => bcc2(I.CMPTLT, I.BNE, I.CMPTUN, I.BNE)
       | T.?<=  => bcc2(I.CMPTLE, I.BNE, I.CMPTUN, I.BNE)
       | T.<> => fall(I.CMPTEQ, I.BNE, I.CMPTUN, I.BEQ)
       | T.?= => bcc2(I.CMPTEQ, I.BNE, I.CMPTUN, I.BNE)
    end

    fun branch(cond, exp1, exp2, lab, order) = let
	fun zapHi r = emit(I.OPERATE{oper=I.ZAP, ra=r, rb=I.IMMop 0xf0, rc=r})
	val tmpR = C.newReg()
	val (r1, o2) = 
	  case order 
	   of T.LR => (regAction exp1, opndAction exp2)
	    | T.RL => let val o2' = opndAction exp2
		in
		  (regAction(exp1), o2')
		end
	fun emitBr(cmp, br) = 
	  (emit(I.OPERATE{oper=cmp, ra=r1, rb=o2, rc=tmpR});
	   emit(I.BRANCH(br, tmpR, lab)))
	fun emitUnsignedBr(cmp, br) = 
	   (case (r1, o2)
	     of (r1, I.REGop r2) => (zapHi r1; zapHi r2; emitBr(cmp, br))
	      | (r1, o2) => (zapHi r1; emitBr(cmp, br))
	   (*esac*))
    in
	case cond 
	 of I.CC_LTU => emitUnsignedBr(I.CMPULT, I.BNE)
	  | I.CC_LEU => emitUnsignedBr(I.CMPULE, I.BNE)
	  | I.CC_GTU => emitUnsignedBr(I.CMPULE, I.BEQ)
	  | I.CC_GEU => emitUnsignedBr(I.CMPULT, I.BEQ)
	  | I.CC_LT  => emitBr(I.CMPLT,  I.BNE)
	  | I.CC_LE  => emitBr(I.CMPLE,  I.BNE)
	  | I.CC_GT  => emitBr(I.CMPLE,  I.BEQ)
	  | I.CC_GE  => emitBr(I.CMPLT,  I.BEQ)
	  | I.CC_EQ  => emitBr(I.CMPEQ,  I.BNE)
	  | I.CC_NEQ => emitBr(I.CMPEQ,  I.BEQ)
    end
    fun copyTmp() = SOME(I.Direct(C.newReg()))
    fun fcopyTmp() = SOME(I.FDirect(C.newFreg()))
  in
    case exp
     of T.JMP(T.LABEL(LE.LABEL lab), _) => emit(I.BRANCH(I.BR, zeroR, lab))
      | T.JMP(T.LABEL _, _) => error "JMP(T.LABEL _, _)"
      | T.JMP(exp, labs) => 
          emit(I.JMPL({r=C.asmTmpR, b=regAction exp, d=0}, labs))
      | T.BCC(_, T.CMP(T.NEQ, T.ANDB(exp, T.LI 1), T.LI 0, _), lab) => 
	  emit(I.BRANCH(I.BLBS, regAction exp, lab))
      | T.BCC(_, T.CMP(cc, exp, T.LI n, ord), lab) => 
          branch(cond cc, exp, T.LI n, lab, ord)
      | T.BCC(_, T.CMP(cc, T.LI n, exp, ord), lab) => 
          branch(swapcc(cond cc), exp, T.LI n, lab, ord)
      | T.BCC(_, T.CMP(cc, e1, e2, ord), lab) => 
	  branch(cond cc, e1, e2, lab, ord)
      | T.BCC(_, e, lab) => emit(I.BRANCH(I.BNE, ccAction e, lab))
      | T.FBCC arg  => fbranch arg
      | T.CALL(exp, def, use) => let
	  val pv = regAction exp
	  val returnPtrR = 26
	  fun live([],acc) = acc
	    | live(T.GPR(T.REG r)::regs,acc) = live(regs, C.addReg(r,acc))
	    | live(T.CCR(T.CC cc)::regs,acc) = live(regs, C.addReg(cc,acc))
	    | live(T.FPR(T.FREG f)::regs,acc) = live(regs, C.addFreg(f,acc))
	    | live(_::regs, acc) = live(regs, acc)
        in
	  emit(I.JSR({r=returnPtrR, b=pv, d=0},
		     live(def, C.addReg(returnPtrR, C.empty)),
		     live(use, C.addReg(pv, C.empty))))
        end
      | T.RET  => emit(I.JMPL({r=zeroR, b=26, d=0}, []))
      | T.STORE8(ea, r, region) => let
	  val rs = regAction r
	  val (rd, disp) = eaAction ea
	  val t1 = C.newReg()
	  val t2 = C.newReg()
	  val t3 = C.newReg()
        in
	  app emit
	     [I.LOAD{ldOp=I.LDQ_U, r=t1, b=rd, d=disp,mem=mem},
	      I.LDA{r=t2, b=rd, d=disp},
	      I.OPERATE{oper=I.INSBL, ra=rs, rb=I.REGop(t2), rc=t3},
	      I.OPERATE{oper=I.MSKBL, ra=t1, rb=I.REGop(t2), rc=t1},
	      I.OPERATE{oper=I.BIS, ra=t1, rb=I.REGop(t3), rc=t1},
	      I.STORE{stOp=I.STQ_U, r=t1, b=rd, d=disp, mem=mem}]
        end
      | T.STORE32(ea, r, region) => let
	  val (b, d) = eaAction ea
        in emit(I.STORE{stOp=I.STL, r=regAction r, b=b, d=d, mem=region})
	end
      | T.STORED(ea, f, region) => let
	  val (b, d) = eaAction ea
        in emit(I.FSTORE{stOp=I.STT, r=fregAction f, b=b, d=d, mem=region})
	end
      | T.STORECC(ea, cc, region) => error "stmAction.STORECC"
      | T.MV(rd, exp) => let
	  fun move(dst, src) = I.OPERATE{oper=I.BIS, ra=src, rb=zeroOp, rc=dst}
        in
	  case exp
	  of T.REG(rs) => if rs = rd then () else emit(move(rd, rs))
	   | T.LI n => loadImmed(n, zeroR, rd)
	   | T.LI32 w => loadImmed32(w, zeroR, rd)
	   | _ => let val rs = regActionRd(exp, rd)
	      in if rs = rd then () else emit(move(rd,rs))
	      end
	  (*esac*)
	end
      | T.CCMV(cd, exp) => let
	  val cs = case exp of T.CC(r) => r | _ => ccActionCd(exp, cd)
	in 
	  if cs = cd then () 
	  else emit(I.OPERATE{oper=I.BIS, ra=cs, rb=zeroOp, rc=cd})
	end
      | T.FMV(fd, exp) => let
	  fun fmove(dst, src) = I.FOPERATE{oper=I.CPYS, fa=src, fb=src, fc=dst}
	in
	 case exp 
	 of T.FREG(fs) => 
	     if fs = fd then () else emit(fmove(fd, fs))
	  | _ => let 
	      val fs = fregActionFd(exp, fd)
	    in if fs = fd then () else emit(fmove(fd, fs))
	    end
        (*esac*)
        end
      | T.COPY(rds as [_], rss) => 
	  emit(I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE})
      | T.COPY(rds, rss) => 
	  emit(I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=copyTmp()})
      | T.FCOPY(fds as [_], fss)=>  
	  emit(I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE})
      | T.FCOPY(fds, fss) =>  
	  emit(I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=fcopyTmp()})
  end
  and ccAction(T.CC r) = r
    | ccAction(e) = ccActionCd(e, C.newCCreg())

  and ccActionCd(T.CC r, _) = r
      (* enough for now ... *)
    | ccActionCd(T.CMP(T.GTU, e1, e2, ord), cd) = let
        val (opnd, reg) = 
	   case ord
	     of T.LR => (opndAction e1, regAction e2)
	      | T.RL => let 
	            val r = regAction e2
		  in (opndAction e1, r)
		  end
      in
	emit(I.OPERATE{oper=I.CMPULT, ra=reg, rb=opnd, rc=cd}); 
	cd
      end
    | ccActionCd(T.LOADCC _, _) = error  "ccAction"

  and opndAction (T.LI value) =				
      if value <= 255 andalso value >= 0 then I.IMMop value
      else let
	  val tmpR = C.newReg()
        in
	  loadImmed (value, zeroR, tmpR);
	  I.REGop tmpR
        end
    | opndAction(T.LI32 value) =
      if Word32.<=(value, 0w255) then I.IMMop (Word32.toInt value)
      else let 
	  val tmpR = C.newReg () 
	in
	  loadImmed32 (value, zeroR, tmpR);
	  I.REGop tmpR
	end
    | opndAction(T.CONST const) = I.CONSTop (const)
    | opndAction exp = I.REGop (regAction exp)

  and reduceOpnd(I.IMMop i) = let
        val rd = C.newReg()
      in loadImmed(i, zeroR, rd); rd
      end
    | reduceOpnd(I.REGop rd) = rd
    | reduceOpnd(I.CONSTop c) = regAction(T.CONST c)
    | reduceOpnd(I.LABop le) = regAction(T.LABEL le)
    | reduceOpnd _ = error "reduceOpnd"

  and orderedRR(e1, e2, T.LR) = (regAction e1, regAction e2)
    | orderedRR(e1, e2, T.RL) = let
	val r2 = regAction e2
      in (regAction e1, r2)
      end

  and regAction (T.REG r) = r
    | regAction exp = regActionRd(exp, C.newReg())

  and arithOperands(e1, e2, T.LR) = (regAction e1, opndAction e2)
    | arithOperands(e1, e2, T.RL) = let
        val opnd = opndAction e2
      in (regAction e1, opnd)
      end
 
  and regActionRd(exp, rd) = let

    fun orderedArith(oper, arith, e1, e2, ord) = let
      val (reg, opnd) = arithOperands(e1, e2, ord)
    in
      emit(oper{oper=arith, ra=reg, rb=opnd, rc=rd});
      rd
    end

    fun commOrderedArith(oper, arith, e1 ,e2, ord) = let
      fun f(e1 as T.LI _, e2) = orderedArith(oper, arith, e2, e1, ord)
	| f(e1 as T.LI32 _, e2) = orderedArith(oper, arith, e2, e1, ord)
	| f(e1, e2) = orderedArith(oper, arith, e1, e2, ord)
    in
      f(e1, e2)
    end
      

    fun orderedArithTrap arg = orderedArith arg before emit(I.TRAPB)
    fun commOrderedArithTrap arg = commOrderedArith arg before emit(I.TRAPB)

    fun orderedMullTrap (e1, e2, ord, rd) = let
      val (reg, opnd) = case ord
	of T.LR => (regAction e1, opndAction e2)
         | T.RL => let
	     val opnd = opndAction e2
           in
	     (regAction e1, opnd)
	   end

      fun emitMulvImmed (reg, 0, rd) = 
	    emit(I.LDA{r=rd, b=zeroR, d=I.IMMop 0})
	| emitMulvImmed (reg, 1, rd) = 
	    emit(I.OPERATE{oper=I.ADDL, ra=reg, rb=zeroOp, rc=rd})
	| emitMulvImmed (reg, multiplier, rd) = let
	    fun log2 0w1 = 0 | log2 n = 1 + (log2 (Word.>> (n, 0w1)))

	    fun exp2 n = Word.<<(0w1, n)

	    fun bitIsSet (x,n) = Word.andb(x,exp2 n) <> 0w0

	    fun loop (~1) = ()
	      | loop n =
	        (if bitIsSet(itow multiplier, itow n) then
		   emit(I.OPERATEV{oper=I.ADDLV, ra=reg, rb=I.REGop rd, rc=rd})
		 else ();
		 if n>0 then 
		   emit(I.OPERATEV{oper=I.ADDLV, ra=rd, rb=I.REGop rd, rc=rd})
		 else ();
	         loop (n-1))
	  in
	    emit(I.OPERATEV{oper=I.ADDLV, ra=reg, rb=I.REGop reg, rc=rd});
	    loop ((log2 (itow multiplier)) - 1)
	  end
    in
      case opnd 
       of (I.IMMop multiplier) => emitMulvImmed (reg, multiplier, rd)
	| _ => emit (I.OPERATEV{oper=I.MULLV , ra=reg, rb=opnd, rc=rd})
      (*esac*);
      emit(I.TRAPB);
      rd
    end

    fun opndToWord (T.LI i) = SOME (Word32.fromInt i)
      | opndToWord (T.LI32 w) = SOME w
      | opndToWord _ = NONE
  in
    case exp
     of T.LI n              => (loadImmed(n, zeroR, rd); rd)
      | T.LI32 w            => (loadImmed32(w, zeroR, rd); rd)
      | T.LABEL le	    => (emit(I.LDA{r=rd, b=zeroR, d=I.LABop le}); rd)
      | T.CONST c	    => (emit(I.LDA{r=rd, b=zeroR, d=I.CONSTop(c)}); rd)
      | T.ADD(e, T.LABEL le)=> 
         (emit(I.LDA{r=rd, b=regAction(e), d=I.LABop(le)}); rd)
      | T.ADD(T.LI i, e)    => (loadImmed(i, regAction e, rd); rd)
      | T.ADD(e, T.LI i)    => (loadImmed(i, regAction e, rd); rd)
      | T.ADD(T.LI32 i, e)  => (loadImmed32(i, regAction e, rd); rd)
      | T.ADD(e, T.LI32 i)  => (loadImmed32(i, regAction e, rd); rd)
      | T.ADD(T.CONST c, e) => 
	 (emit(I.LDA{r=rd, b=regAction e, d=I.CONSTop c}); rd)
      | T.ADD(e, T.CONST c) => 
	 (emit(I.LDA{r=rd, b=regAction e, d=I.CONSTop c}); rd)
      | T.ADD(e1, e2)       => commOrderedArith(I.OPERATE, I.ADDL, e1, e2, T.LR)
      | T.SUB(e1, e2, ord)  => orderedArith(I.OPERATE, I.SUBL, e1, e2, ord)
      | T.MULU(e1, e2)	    => commOrderedArith(I.OPERATE, I.MULL, e1, e2, T.LR)
      | T.ADDT(e1, e2)	    =>
	  commOrderedArithTrap(I.OPERATEV, I.ADDLV, e1, e2, T.LR)
      | T.SUBT(e1, e2, ord) => orderedArithTrap(I.OPERATEV, I.SUBLV, e1, e2, ord)
      | T.MULT(e1, e2) 	    => orderedMullTrap(e1, e2, T.LR, rd)
      | T.ANDB(e1, e2)	    => let
          fun opndToByteMask (SOME (0wx0:Word32.word)) = SOME 0xf
	    | opndToByteMask (SOME 0wx000000ff) = SOME 0xe
	    | opndToByteMask (SOME 0wx0000ff00) = SOME 0xd
	    | opndToByteMask (SOME 0wx0000ffff) = SOME 0xc
	    | opndToByteMask (SOME 0wx00ff0000) = SOME 0xb
	    | opndToByteMask (SOME 0wx00ff00ff) = SOME 0xa
	    | opndToByteMask (SOME 0wx00ffff00) = SOME 0x9
	    | opndToByteMask (SOME 0wx00ffffff) = SOME 0x8
            | opndToByteMask (SOME 0wxff000000) = SOME 0x7
	    | opndToByteMask (SOME 0wxff0000ff) = SOME 0x6
	    | opndToByteMask (SOME 0wxff00ff00) = SOME 0x5
	    | opndToByteMask (SOME 0wxff00ffff) = SOME 0x4
	    | opndToByteMask (SOME 0wxffff0000) = SOME 0x3
	    | opndToByteMask (SOME 0wxffff00ff) = SOME 0x2
	    | opndToByteMask (SOME 0wxffffff00) = SOME 0x1
	    | opndToByteMask (SOME 0wxffffffff) = SOME 0x0
	    | opndToByteMask _ = NONE

	  val opndToMask = opndToByteMask o opndToWord
	in
	  case (opndToMask e1, opndToMask e2) of
	    (SOME mask, _) =>
	      orderedArith(I.OPERATE, I.ZAP, e2, T.LI mask, T.LR)
	  | (_, SOME mask) => 
	      orderedArith(I.OPERATE, I.ZAP, e1, T.LI mask, T.LR)
	  | _ => commOrderedArith(I.OPERATE, I.AND, e1, e2, T.LR)
        end
      | T.ORB(e1, e2) 	    => commOrderedArith(I.OPERATE, I.BIS, e1, e2, T.LR)
      | T.XORB(e1, e2)	    => commOrderedArith(I.OPERATE, I.XOR, e1, e2, T.LR)
      | T.SLL(e1, e2, ord)  => 
	  (case opndToWord e2 of
	     SOME 0w1 => let val r = T.REG (regAction e1)
               in orderedArith(I.OPERATE, I.ADDL, r, r, T.LR) end
	   | SOME 0w2 => orderedArith(I.OPERATE, I.S4ADDL, e1, T.LI 0, T.LR)
	   | SOME 0w3 => orderedArith(I.OPERATE, I.S8ADDL, e1, T.LI 0, T.LR)
	   | _ => (orderedArith(I.OPERATE, I.SLL, e1, e2, T.LR);
		   emit(I.OPERATE{oper=I.SGNXL, ra=rd, rb=zeroOp, rc=rd});
		   rd)
	  (*esac*))
      | T.SRA(e1, e2, ord)  => let
	    val (reg, opnd) = (regAction e1, opndAction e2)
          in
	    (* sign extend longword argument *)
	    emit(I.OPERATE{oper=I.SGNXL, ra=reg, rb=zeroOp, rc=reg});
	    emit(I.OPERATE{oper=I.SRA, ra=reg, rb=opnd, rc=rd});
	    rd
          end
      | T.SRL(e1, e2, ord)  => let
	  val (reg, opnd) = (regAction e1, opndAction e2)
        in
	  emit(I.OPERATE{oper=I.ZAP, ra=reg, rb=I.IMMop 0xf0, rc=reg});
	  emit(I.OPERATE{oper=I.SRL, ra=reg, rb=opnd, rc=rd});
	  rd
        end
      | T.DIVT arg => let
	  val (reg, opnd) = arithOperands arg
	in
	  app emit (PseudoInstrs.divl({ra=reg, rb=opnd, rc=rd}, reduceOpnd));
	  rd
        end
      | T.DIVU arg => let
	  val (reg, opnd) = arithOperands arg
	in
	  app emit (PseudoInstrs.divlu({ra=reg, rb=opnd, rc=rd}, reduceOpnd));
	  rd
	end
      | T.LOAD32(exp, region) => let
	  val (b, d) = eaAction exp
	in emit(I.LOAD{ldOp=I.LDL, r=rd, b=b, d=d, mem=region}); rd
	end
	(* Load and sign-extend a byte from a  non-aligned address  *)
      | T.LOAD8(exp, region) => let
	  val tmpR0 = C.newReg()
	  val tmpR1 = C.newReg()
	  val (rt, disp) = eaAction exp
	in
	  emit(I.LOAD{ldOp=I.LDQ_U, r=tmpR0, b=rt, d=disp, mem=mem});
	  emit(I.LDA{r=tmpR1, b=rt, d=disp});
	  emit(I.OPERATE{oper=I.EXTBL, ra=tmpR0, rb=I.REGop tmpR1, rc=rd});
	  rd
	end
      | T.SEQ(e1, e2) => (stmAction e1; regAction e2)
      | _ => error "regAction"
  end (* regActionRd *)

  and eaAction exp = let
    fun makeEA(r, n) = 
      if ~32768 <= n andalso n <= 32767 then (r, I.IMMop n)
      else let
	  val tmpR = C.newReg()
	  val low = wtoi(Word.andb(itow n, 0w65535))(* unsigned low 16 bits *)
	  val high = n div 65536
	  val (lowsgn, highsgn) =			 (* Sign-extend *)
	    if low <= 32767 then (low, high) else (low -65536, high+1)
	in
	  (emit(I.LDAH{r=tmpR, b=r, d=I.IMMop highsgn});
	   (tmpR, I.IMMop lowsgn))
	end
  in
    case exp
     of T.ADD(exp, T.LI n)    => makeEA(regAction exp,  n)
      | T.ADD(T.LI n, exp)    => makeEA(regAction exp,  n)
      | T.ADD(T.CONST c, exp) => (regAction exp, I.CONSTop(c))
      | T.ADD(exp, T.CONST c) => (regAction exp, I.CONSTop(c))
      | T.SUB(exp, T.LI n, _) => makeEA(regAction exp, ~n)
      | exp                   => makeEA(regAction exp,  0)
  end (* eaAction *)

  and fregAction (T.FREG f) = f
    | fregAction exp = fregActionFd(exp, C.newFreg())

  and fregActionFd(exp, fd) = let
    (* macho comment goes here *)
    fun doFloatArith(farith, e1, e2, fd, order) = let
      val (f1, f2) = orderedFArith(e1, e2, order)
    in
      emit(I.DEFFREG fd);
      emit(I.FOPERATEV{oper=farith, fa=f1, fb=f2, fc=fd});
      emit(I.TRAPB);
      fd
    end
  in
    case exp 
     of T.FREG f	     => f
      | T.FADDD(e1, e2)      => doFloatArith(I.ADDT, e1, e2, fd, T.LR)
      | T.FMULD(e1, e2)      => doFloatArith(I.MULT, e1, e2, fd, T.LR)
      | T.FSUBD(e1, e2, ord) => doFloatArith(I.SUBT, e1, e2, fd, ord)
      | T.FDIVD(e1, e2, ord) => doFloatArith(I.DIVT, e1, e2, fd, ord)
      | T.FABSD exp          => 
         (emit(I.FOPERATE{oper=I.CPYS, fa=zeroR, fb=fregAction exp, fc=fd}); fd)
      | T.FNEGD exp          => let
          val fs = fregAction exp 
	in
	  emit(I.FOPERATE{oper=I.CPYSN, fa=fs, fb=fs, fc=fd});  fd
	end
      | T.CVTI2D exp         => let
	  val opnd  = opndAction exp
        in
	  app emit (PseudoInstrs.cvti2d({opnd=opnd, fd=fd},reduceOpnd));
	  fd
	end
      | T.LOADD(exp, region) => let
	  val (b, d) = eaAction exp
        in emit(I.FLOAD{ldOp=I.LDT, r=fd, b=b, d=d, mem=region}); fd
        end
      | T.FSEQ(e1, e2)        => (stmAction e1; fregAction e2)
  end

  fun mltreeComp mltree = let
    (* condition code registers are mapped onto general registers *)
    fun cc (T.CCR(T.CC cc)) = T.GPR(T.REG cc)
      | cc x = x

    fun mltc(T.PSEUDO_OP pOp)    = F.pseudoOp pOp
      | mltc(T.DEFINELABEL lab)  = F.defineLabel lab
      | mltc(T.ENTRYLABEL lab)   = F.entryLabel lab
      | mltc(T.ORDERED mlts)     = F.ordered mlts
      | mltc(T.BEGINCLUSTER)     = F.beginCluster()
      | mltc(T.CODE stms)        = app stmAction stms
      | mltc(T.BLOCK_NAME name)  = F.blockName name
      | mltc(T.ENDCLUSTER regmap)= F.endCluster regmap
      | mltc(T.ESCAPEBLOCK regs) = F.exitBlock (map cc regs)

    val res = mltc mltree
  in res
  end 

  val mlriscComp  = stmAction
end


