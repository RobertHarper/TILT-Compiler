(* hppa.sml
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 * generates machine code from the mltree.
 *
 *)
functor Hppa
  (structure HppaInstr : HPPAINSTR
   structure HppaMLTree : MLTREE where Region = HppaInstr.Region
                                 and Constant = HppaInstr.Constant
   structure Flowgen : FLOWGRAPH_GEN where I = HppaInstr
                                     and T = HppaMLTree
                                     and B = HppaMLTree.BNames
   structure MilliCode : HPPA_MILLICODE where I = HppaInstr
   structure LabelComp : LABEL_COMP where I = HppaInstr
                                    and T = HppaMLTree
(* DBM: sharing/defn conflict:
     sharing Flowgen.I = MilliCode.I = LabelComp.I = HppaInstr 
     sharing Flowgen.T = LabelComp.T = HppaMLTree
     sharing HppaMLTree.Region = HppaInstr.Region
     sharing HppaMLTree.Constant = HppaInstr.Constant
     sharing HppaMLTree.BNames = Flowgen.B
*)
  ) : MLTREECOMP = 
struct
  structure I = HppaInstr
  structure F = Flowgen
  structure T = HppaMLTree
  structure C = HppaCells
  structure MC = MilliCode
  structure LC = LabelComp 
  structure Region = I.Region

  structure M = struct
    (* runtime system dependent constants. *)
    val float64TmpOffset = 0				(* must be < 16 *)
    val float32TmpOffset = float64TmpOffset		(* must be < 16 *)
    val cvti2dOffset = ~4		
  end

  fun error msg = MLRiscErrorMsg.impossible ("Hppa." ^ msg)

  val itow = Word.fromInt

  val emitInstr = F.emitInstr
  val emit = F.emitInstr
  val ldLabelEA = LC.ldLabelEA emit
  val ldLabelOpnd = LC.ldLabelOpnd emit

  datatype ea = DISPea of int * I.operand  | INDXea of int * int

  (* integer ranges *)
  fun im5 n = n < 16 andalso n >= ~16
  fun im11 n = n < 1024 andalso n >= ~1024
  fun im14 n = n < 8192 andalso n >= ~8192

  fun split n = let
    val w = Word.fromInt(n)
  in
    (Word.toIntX(Word.~>>(w, 0w11)), Word.toIntX(Word.andb(w, 0wx7ff)))
  end

  val zeroR = 0
  val zeroEA = I.Direct(zeroR)

  fun emitMove(src, dst) = emit(I.ARITH{a=I.OR, r1=src, r2=zeroR, t=dst})

  fun loadImmedRd(n, rd) = 
    if im14 n then (emit(I.LDO{i=I.IMMED n, b=0, t=rd}); rd)
    else let
        val (hi, lo) = split n
	val tmpR = C.newReg()
      in
	emit(I.LDIL{i=I.IMMED hi, t=tmpR});
	emit(I.LDO{i=I.IMMED lo, b=tmpR, t=rd});
	rd
      end

  fun loadImmed n = loadImmedRd(n, C.newReg())

  fun loadWord32Rd(w, rd) = let
    val toInt = Word32.toIntX
  in
    if Word32.<(w, 0w8192) then emit(I.LDO{i=I.IMMED(toInt w), b=0, t=rd})
    else let 
        val tmpR = C.newReg()
	val hi = Word32.~>>(w, 0w11)
	val lo = Word32.andb(w, 0wx7ff)
      in
	emit(I.LDIL{i=I.IMMED(toInt hi), t=tmpR});
	emit(I.LDO{i=I.IMMED(toInt lo), b=tmpR, t=rd})
      end;
    rd
  end
  fun loadWord32 w = loadWord32Rd(w, C.newReg())

  fun milliCall(milliFn, exp1, exp2, ord, rd) = let
    val (rs, rt) = orderedRR(exp1, exp2, ord)
  in app emit (milliFn{rs=rs, rt=rt, rd=rd}); rd
  end

  and orderedRR(exp1, exp2, T.LR) = (regAction exp1, regAction exp2)
    | orderedRR(exp1, exp2, T.RL) = let val r2 = regAction exp2
      in 
	(regAction exp1, r2)
      end
  
  and orderedFF(exp1, exp2, T.LR) = (fregAction exp1, fregAction exp2)
    | orderedFF(exp1, exp2, T.RL) = let val f2 = fregAction exp2
      in (fregAction exp1, f2)
      end

  and eaAction(T.ADD(exp, T.LI n)) = DISPea(regAction exp, I.IMMED n)
    | eaAction(T.ADD(T.LI n, exp)) = DISPea(regAction exp, I.IMMED n)
    | eaAction(T.ADD(exp, T.CONST c)) = DISPea(regAction exp, I.ConstOp c)
    | eaAction(T.ADD(T.CONST c, exp)) = DISPea(regAction exp, I.ConstOp c)
    | eaAction(T.ADD(e1 as T.LABEL _, e2)) = eaAction(T.ADD(e2, e1))
    | eaAction(T.ADD(e, T.LABEL le)) = let
        val rs = regAction(e)
      in
	case ldLabelEA(le)
	of (0, opnd) => DISPea(rs, opnd)
         | (rt, I.IMMED 0) => INDXea(rs, rt)
	 | (rt, opnd) => let 
	     val tmpR = C.newReg()
           in
	     emit(I.ARITH{a=I.ADD, r1=rs, r2=rt, t=tmpR});
	     DISPea(tmpR, opnd)
           end
      end
    | eaAction(T.ADD(exp1, exp2)) = INDXea(regAction exp1, regAction exp2)
    | eaAction(T.SUB(exp, T.LI n, _)) = DISPea(regAction exp, I.IMMED(~n))
    | eaAction(T.LABEL lexp) = DISPea(ldLabelEA(lexp))
    | eaAction exp = DISPea(regAction exp, I.IMMED 0)
    
  and stmAction exp = let
    fun store(ea, reg, instr, mem) = let
      val (b, d) = 
	  case eaAction ea
	    of DISPea (bd as (base, I.IMMED disp)) => 
		 if im14 disp then bd 
		 else let 
		     val (hi21, lo11) = split disp
		     val tmpR1 = C.newReg()
		     val tmpR2 = C.newReg()
		   in
		     emit(I.LDIL{i=I.IMMED hi21, t=tmpR1});
		     emit(I.ARITH{a=I.ADD, r1=base, r2=tmpR1, t=tmpR2});
		     (tmpR2, I.IMMED lo11)
		   end
	     | DISPea bd => bd
	     | INDXea(r1,r2) => let 
		 val t = C.newReg()
	       in
		 emit (I.ARITH {a=I.ADD, r1=r1, r2=r2, t=t});
		 (t, I.IMMED 0)
	       end
    in emit (I.STORE {st=instr, b=b, d=d, r=regAction reg, mem=mem})
    end

    fun fstore(ea, freg, mem) = let
      val r = fregAction freg
    in
      case eaAction ea
	of DISPea(b, I.IMMED d) => 
	    if im5 d then
	      emit(I.FSTORE {fst=I.FSTDS, b=b, d=d, r=r, mem=mem})
	    else
	      emit(I.FSTOREX{fstx=I.FSTDX, b=b, x=loadImmed d, r=r, mem=mem})
         | DISPea(b, d) => let
	      val tmpR = C.newReg()
	   in 
	     emit(I.ARITHI{ai=I.ADDI, r=b, i=d, t=tmpR});
	     emit(I.FSTORE{fst=I.FSTDS, b=tmpR, d=0, r=r, mem=mem})
	   end
	 | INDXea(b,x) => emit(I.FSTOREX{fstx=I.FSTDX, b=b, x=x, r=r, mem=mem})
    end

    fun branch(bc, r1, r2, t) = let
	val flab = Label.newLabel ""
	fun emitBranch(cmp, ic, r1, r2) = 
	  (emit(I.BCOND{cmp=cmp, bc=ic, r1=r1, r2=r2, t=t, f=flab, n=true});
	   F.defineLabel flab)
    in
      (case bc
	of T.LT  => emitBranch(I.COMBT, I.LT, r1, r2)
	 | T.LE  => emitBranch(I.COMBT, I.LE, r1, r2)
	 | T.GT  => emitBranch(I.COMBT, I.LT, r2, r1)
	 | T.GE  => emitBranch(I.COMBT, I.LE, r2, r1)
	 | T.EQ  => emitBranch(I.COMBT, I.EQ, r1, r2)
	 | T.LTU => emitBranch(I.COMBT, I.LTU, r1, r2)
	 | T.LEU => emitBranch(I.COMBT, I.LEU, r1, r2)
	 | T.GEU => emitBranch(I.COMBT, I.LEU, r2, r1)
	 | T.GTU => emitBranch(I.COMBT, I.LTU, r2, r1)
	 | T.NEQ  => emitBranch(I.COMBF, I.EQ,  r1, r2)
      (*esac*))
    end
    fun copyTmp() = SOME(I.Direct(C.newReg()))
    fun fcopyTmp() = SOME(I.FDirect(C.newFreg()))

    val reduce={stm=stmAction, rexp=regAction, emit=emit}
    val returnPtr = 2
  in
    case exp
    of T.MV(rd, exp) => 
       (case exp
	 of T.REG(rs) => if rd = rs then () else emitMove(rs, rd)
	  | T.LI n => (loadImmedRd(n, rd); ())
	  | T.LI32 w => (loadWord32Rd(w, rd); ())
	  | _ => let val rs = regActionRd(exp, rd)
	      in if rs = rd then () else emitMove(rs, rd)
	      end
       (*esac*))
     | T.FMV(fd, exp) => let
	  fun fmove(src,dst) = I.FUNARY{fu=I.FCPY, f=src, t=dst}
       in
	  case exp
	   of T.FREG(fs) => 
	        if fs = fd then () else emit(fmove(fs, fd))
	    | _ => let val fs = fregActionFd(exp, fd)
		in if fs = fd then () else emit(fmove(fs, fd))
		end
	  (*esac*)
       end
     | T.CCMV(cd, exp) => let
	 val cs = case exp of T.CC(r) => r | _ => ccActionCd(exp, cd)
       in if cs=cd then () else emitMove(cs, cd)
       end

     | T.COPY(rds as [_], rss) => 
        emit(I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=NONE})
     | T.COPY(rds, rss) => 
        emit(I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=copyTmp()})
     | T.FCOPY(fds as [_], fss)=>  
        emit(I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=NONE})
     | T.FCOPY(fds, fss) =>  
        emit(I.FCOPY{dst=fds, src=fss, impl=ref NONE, tmp=fcopyTmp()})

     | T.JMP _ => LC.doJmp(reduce, exp)
     | T.CALL _ => LC.doCall(reduce, exp)
     | T.RET  => emit(I.BV{labs=[], x=0, b=returnPtr, n=true}) 
     | T.STORE8(ea, r, region)  => store(ea, r, I.STB, region)
     | T.STORE32(ea, r, region) => store(ea, r, I.STW, region)
     | T.STORED(ea, f, region)  => fstore(ea, f, region)
     | T.STORECC(ea, cc, region)  => error "stmAction.STORECC"
     | T.BCC(_, T.CMP(cc, exp, T.LI n, ord), lab) => let
	 val r = regAction exp
       in
	 if im5 n then let
	     val flab = Label.newLabel ""
	     fun emitBranch(cmpi, ic) = 
	       (emit(I.BCONDI{cmpi=cmpi,bc=ic,i=n,r2=r,t=lab,f=flab, n=true});
		F.defineLabel flab)
	   in
	     case cc
	      of T.LT  => emitBranch(I.COMIBF, I.LE)
	       | T.LE  => emitBranch(I.COMIBF, I.LT)
	       | T.GT  => emitBranch(I.COMIBT, I.LT)
	       | T.GE  => emitBranch(I.COMIBT, I.LE)
	       | T.EQ  => emitBranch(I.COMIBT, I.EQ)
	       | T.LTU => emitBranch(I.COMIBF, I.LEU)
	       | T.LEU => emitBranch(I.COMIBF, I.LTU)
	       | T.GEU => emitBranch(I.COMIBT, I.LEU)
	       | T.GTU => emitBranch(I.COMIBT, I.LTU)
	       | T.NEQ  => emitBranch(I.COMIBF, I.EQ)
	   end
	 else 
	   branch(cc, r, loadImmed n, lab)
       end
     | T.BCC(_, T.CMP(cc, exp1, exp2, order), lab) => let
	 val (r1, r2) = orderedRR(exp1, exp2, order)
       in
	 branch(cc, r1, r2, lab)
       end
     | T.BCC(_, e, lab) => let
	  val cc = ccAction e
	  val flab = Label.newLabel""
       in
	 emit(I.BCOND{cmp=I.COMBT, bc=I.EQ, r1=cc, r2=zeroR, 
		      t=lab, f=flab, n=true});
	 F.defineLabel flab
       end
     | T.FBCC(_, T.FCMP(cc, exp1, exp2, order), lab) => let
	 val (f1,f2) = orderedFF(exp1, exp2, order)
	 val fallThrough = Label.newLabel ""
	 fun fcond T.==   = I.!=
	   | fcond T.?<>  = I.==
	   | fcond T.?    = I.<=>   
	   | fcond T.<=>  = I.?
	   | fcond T.>    = I.?<=   
	   | fcond T.>=   = I.?<
	   | fcond T.?>   = I.<=
	   | fcond T.?>=  = I.<
	   | fcond T.<    = I.?>=   
	   | fcond T.<=   = I.?>
	   | fcond T.?<   = I.>=
	   | fcond T.?<=  = I.>
	   | fcond T.<>   = I.?=
	   | fcond T.?=   = I.<>
       in
	 emit(I.FBRANCH{cc=fcond cc,f1=f1,f2=f2,t=lab,f=fallThrough, n=true,
                        long=false});
	 F.defineLabel fallThrough
       end
  end

  (* condition code registers are implemented using 
   * general purpose registers.
   *)
  and ccAction(T.CC r) = r
    | ccAction e = ccActionCd(e, C.newCCreg())

  and ccActionCd(T.CC r, _) = r
    | ccActionCd(T.CMP(cond, exp1, exp2, order), rd) = let
        val (r1, r2) = orderedRR(exp1, exp2, order)
      in
	case cond 
	of T.GTU =>
	     (emit(I.COMCLR{cc=I.GTU, r1=r1, r2=r2, t=rd});
	      emit(I.LDO{i=I.IMMED 1, b=0, t=rd}))
         (* enough for now *)
         | _ => error "ccAction.CMP"		
	(*esac*);
	rd
      end
    | ccActionCd(T.FCMP _, _) = error "ccAction:FCMP"
    | ccActionCd(T.LOADCC _, _) = error "ccAction:LOADCC"

  and regAction(T.REG r) = r
    | regAction exp = regActionRd(exp, C.newReg())

  and regActionRd(exp, rd) = let
    datatype opnd = REG of int | OPND of I.operand

    fun opndAction(T.LI n) = 
        if im11 n then OPND(I.IMMED n) else REG(loadImmed n)
      | opndAction(T.LI32 w) = 
	if Word32.<=(w, 0w1024) then OPND(I.IMMED(Word32.toInt w))
	else REG(loadWord32 w)
      | opndAction(T.CONST c) = OPND(I.ConstOp c)
      | opndAction(T.LABEL le) = 
	(case ldLabelOpnd{label=le, pref=NONE}
	 of LC.REG r => REG r 
          | LC.OPND opnd => OPND opnd
	 (*esac*))
      | opndAction exp = REG(regAction exp)

    fun immedArith(exp1, exp2, order, immdOp, arithOp) = let
      val (opnd, r2) = 
	case order
	 of T.LR => (opndAction exp1, regAction exp2)
	  | T.RL => let val opnd' = opndAction exp1
	    in
	      (opnd', regAction exp2)
	    end
    in
      case opnd 
      of REG r1 =>  emit(I.ARITH{a=arithOp, r1=r1, r2=r2, t=rd})
       | OPND opnd => emit(I.ARITHI{ai=immdOp, r=r2, i=opnd, t=rd})
      (*esac*);
      rd
    end (* immedArith *)

    fun commImmedArith(arg as (exp1, exp2, ord, immdOp, arithOp)) = 
      (case exp2 
       of (T.LI _ | T.LI32 _ | T.CONST _ | T.LABEL _ )=>
	    immedArith(exp2, exp1, ord, immdOp, arithOp)
        | _ => immedArith arg
      (*esac*))

    local
      fun shift (immdSht, varSht) = let
	fun f(exp, T.LI n, _) = 
	    if n < 0 orelse n > 31 then error "regActionRd:shift"
	    else let
	        val rs = regAction exp
	      in
		emit(I.SHIFT{s=immdSht, r=rs, p=31-n, len=32-n, t=rd});
		rd
	      end
	  | f(exp1, exp2, order) = let
	      val (r1, r2) = orderedRR(exp1, exp2, order)
	      val tmp = C.newReg()
	    in
	      emit(I.ARITHI{ai=I.SUBI, i=I.IMMED 31, r=r2, t=tmp});
	      emit(I.MTCTL{r=tmp, t=11});
	      emit(I.SHIFTV{sv=varSht, r=r1, len=32, t=rd});
	      rd
	    end
      in
	f
      end
    in
      val sll = shift (I.ZDEP, I.ZVDEP)
      val srl = shift (I.EXTRU, I.VEXTRU)
      val sra = shift (I.EXTRS, I.VEXTRS)
    end

    fun arith(exp1, exp2, oper) =
      (emit(I.ARITH{a=oper, r1=regAction exp1, r2=regAction exp2, t=rd});
       rd)

    fun load(ea, rd, instri, instrx, mem) = 
      (case eaAction ea
	of DISPea(b, I.IMMED d) => 
	    if im14 d then 
              emit(I.LOADI{li=instri, i=I.IMMED d, r=b, t=rd, mem=mem})
	    else 
              emit(I.LOAD{l=instrx, r1=b, r2=loadImmed d, t=rd, mem=mem})
         | DISPea(b, d) => 
	     emit(I.LOADI{li=instri, i=d, r=b, t=rd, mem=mem})
	 | INDXea(b,x) => 
             emit(I.LOAD{l=instrx, r1=b, r2=x, t=rd, mem=mem})
       (*esac*);
      rd)

    val reduce = {stm=stmAction, rexp=regAction, emit=emit}
  in
    case exp
     of T.LI n => (loadImmedRd(n, rd); rd)
      | T.LI32 w => (loadWord32Rd(w, rd); rd)
      | T.CONST c => (emit(I.LDO{i=I.ConstOp(c), b=0, t=rd}); rd)
      | T.LABEL le => 
        (case ldLabelOpnd{label=le, pref=SOME(rd)}
	 of LC.REG rs => if rd=rs then () else emitMove(rs, rd)
          | LC.OPND opnd => emit(I.LDO{i=opnd, b=zeroR, t=rd})
         (*esac*);
         rd)
      | T.ADD(exp1, exp2)    => commImmedArith(exp1, exp2, T.LR, I.ADDI, I.ADD)
      | T.ADDT(e1, e2)       => commImmedArith(e1, e2, T.LR, I.ADDIO, I.ADDO)
      | T.SUB(e, T.LI n, _)  => immedArith(T.LI(~n), e, T.LR, I.ADDIO, I.ADDO)
      | T.SUBT(e, T.LI n, _) => immedArith(T.LI(~n), e, T.LR, I.ADDIO, I.ADDO)
      | T.SUB(exp1, exp2, ord) => immedArith(exp1, exp2, ord, I.SUBI, I.SUB)
      | T.SUBT(exp1, exp2, ord) => immedArith(exp1, exp2, ord, I.SUBIO, I.SUBO)
      | T.SLL arg	       => sll arg
      | T.SRL arg	       => srl arg
      | T.SRA arg              => sra arg
      | T.ANDB(exp1, exp2)     => arith(exp1, exp2, I.AND)
      | T.ORB(exp1, exp2)      => arith(exp1, exp2, I.OR)
      | T.XORB(exp1, exp2)     => arith(exp1, exp2, I.XOR)
      | T.DIVU(exp1, exp2, ord)=> milliCall(MC.divu, exp1, exp2, ord, rd)
      | T.DIVT(exp1, exp2, ord)=> milliCall(MC.divo, exp1, exp2, ord, rd)
      | T.MULT(exp1, exp2)     => milliCall(MC.mulo, exp1, exp2, T.LR, rd)
      | T.MULU(exp1, exp2)     => milliCall(MC.mulu, exp1, exp2, T.LR, rd)
      | T.LOAD8(ea, region)    => load(ea, rd, I.LDB, I.LDBX, region)
      | T.LOAD32(ea, region)   => load(ea, rd, I.LDW, I.LDWX, region)
      | T.SEQ(exp1, exp2)      => (stmAction exp1; regAction exp2)
      | _  => error "regActionRd: missing rules"
  end (* regActionRd *)

  and fregAction (T.FREG f) = f
    | fregAction exp = fregActionFd(exp, C.newFreg())

  and fregActionFd(exp, fd) = let
    fun orderedFarith(exp1, exp2, ord, arithOp) = let
      val (f1, f2) = orderedFF(exp1, exp2, ord)
    in
      emit(I.FARITH{fa=arithOp, r1=f1, r2=f2, t=fd});
      fd
    end
      
  in
    case exp 
     of T.LOADD(ea, region) => 
        (case eaAction ea
	  of INDXea(r1, r2) => 
               emit(I.FLOADX{flx=I.FLDDX, b=r1, x=r2, t=fd, mem=region})
	   | DISPea(r, I.IMMED n) => 
	      if im5 n then
		emit(I.FLOAD{fl=I.FLDDS, b=r, d=n, t=fd, mem=region})
	      else 
		emit(I.FLOADX{flx=I.FLDDX, b=r, x=loadImmed n, t=fd, 
                              mem=region})
	   | DISPea(r,d) => let
		val tmpR = C.newReg()
	     in
	       emit(I.ARITHI{ai=I.ADDI, r=r, i=d, t=tmpR});
	       emit(I.FLOADX{flx=I.FLDDX, b=tmpR, x=zeroR, t=fd,mem=region})
	     end
	(*esac*);
	fd)
      | T.FADDD(exp1, exp2)	 => orderedFarith(exp1, exp2, T.LR, I.FADD)
      | T.FSUBD(exp1, exp2, ord) => orderedFarith(exp1, exp2, ord, I.FSUB)
      | T.FMULD(exp1, exp2)	 => orderedFarith(exp1, exp2, T.LR, I.FMPY)
      | T.FDIVD(exp1, exp2, ord) => orderedFarith(exp1, exp2, ord, I.FDIV)
      | T.FABSD exp => (emit(I.FUNARY{fu=I.FABS, f=fregAction exp, t=fd}); fd)
      | T.FNEGD exp => (emit(I.FARITH{fa=I.FSUB, r1=0, r2=fregAction exp, t=fd}); fd)
      | T.CVTI2D exp => 
	 (emit(I.STORE{st=I.STW, b=C.stackptrR, d=I.IMMED M.cvti2dOffset, 
		     r=regAction exp, mem=Region.stack});
	  emit(I.FLOAD{fl=I.FLDWS, b=C.stackptrR, d=M.cvti2dOffset, t=fd,
                       mem=Region.stack});
	  emit(I.FUNARY{fu=I.FCNVXF, f=fd, t=fd});
	  fd)
      | T.FSEQ(e1, e2) => (stmAction e1; fregAction e2)
      | _ => error "fregAction: missing rule"
  end

  and mltreeComp mltree = let
    (* condition code registers are mapped onto general registers *)
    fun cc (T.CCR(T.CC cc)) = T.GPR(T.REG cc)
      | cc x = x

    fun mltc(T.PSEUDO_OP pOp)    = F.pseudoOp pOp
      | mltc(T.DEFINELABEL lab)  = F.defineLabel lab
      | mltc(T.ENTRYLABEL lab)   = F.entryLabel lab
      | mltc(T.ORDERED mlts)     = F.ordered mlts
      | mltc(T.BEGINCLUSTER)     = F.beginCluster()
      | mltc(T.CODE stms)	 = app stmAction stms
      | mltc(T.BLOCK_NAME name)  = F.blockName name
      | mltc(T.ENDCLUSTER regmap)= F.endCluster regmap
      | mltc(T.ESCAPEBLOCK regs) = F.exitBlock (map cc regs)
  in mltc mltree 
  end 

  val mlriscComp = stmAction
end

