(*
 * Machine code generator for SPARC.
 *
 * The SPARC architecture has 32 general purpose registers (%g0 is always 0)
 * and 32 single precision floating point registers.  
 *
 * Some Ugliness: double precision floating point registers are 
 * register pairs.  There are no double precision moves, negation and absolute
 * values.  These require two single precision operations.  I've created
 * composite instructions FMOVd, FNEGd and FABSd to stand for these. 
 *
 * All integer arithmetic instructions can optionally set the condition 
 * code register.  We use this to simplify certain comparisons with zero.
 *
 * Integer multiplication, division and conversion from integer to floating
 * go thru the pseudo instruction interface, since older sparcs do not
 * implement these instructions in hardware.
 *
 * In addition, the trap instruction for detecting overflow is a parameter.
 * This allows different trap vectors to be used.
 *
 * The virtual register 65 is used to represent the %psr register.
 *
 * -- Allen 
 *)

functor Sparc
  (structure SparcInstr : SPARCINSTR
   structure SparcMLTree : MLTREE where Region = SparcInstr.Region
                                  and Constant = SparcInstr.Constant
   structure Flowgen : FLOWGRAPH_GEN where I = SparcInstr
                                     and T = SparcMLTree
                                     and B = SparcMLTree.BNames
   structure PseudoInstrs : SPARC_PSEUDO_INSTR where I = SparcInstr
(* DBM: sharing/defn conflict:
     sharing SparcInstr.Region = SparcMLTree.Region
     sharing Flowgen.I=PseudoInstrs.I=SparcInstr
     sharing Flowgen.T=SparcMLTree 
     sharing SparcMLTree.Constant = SparcInstr.Constant
     sharing SparcMLTree.BNames = Flowgen.B
*)
   val overflowtrap : SparcInstr.instruction list
  ) : MLTREECOMP = 
struct
  structure F = Flowgen
  structure T = SparcMLTree
  structure R = SparcMLTree.Region
  structure I = SparcInstr
  structure C = SparcInstr.C
  structure LE = LabelExp
  structure W  = Word32
  structure P  = PseudoInstrs

  datatype trapping = TRAPS | SILENT
  datatype commutative = COMMUTE | DONTCOMMUTE
  datatype target = 
      CC     (* condition code only *)
    | REG    (* register only *)
    | CC_REG (* conditional code and register *) 

  fun error msg = MLRiscErrorMsg.impossible ("Sparc." ^ msg)

  val emitInstr = F.emitInstr
  val emit = F.emitInstr
  fun newReg () = C.newReg()
  fun newFreg() = C.newFreg()

  (* load/store has 13 bits sign extended immediates *) 
  fun immed13 n = ~4096 <= n andalso n < 4096
  fun immed13w w = let val x = W.~>>(w,0w12)
		   in  x = 0w0 orelse (W.notb x) = 0w0
                   end


  (* split into 22 high bits/10 low bits *)

  fun splitw w = 
      {hi=W.toInt(W.>>(w,0w10)), lo=W.toInt(W.andb(w,0wx3ff))} 

  fun split n = splitw(W.fromInt n)

  (* load immediate *)
  fun loadImmed(n,d) =
      if immed13 n then emit(I.ARITH{a=I.OR,r=0,i=I.IMMED n,cc=false,d=d})
      else let 
	  val t = newReg()
	  val {hi,lo} = split n
        in
	  if lo = 0 then emit(I.SETHI{i=hi,d=d})
	  else
	    (emit(I.SETHI{i=hi,d=t});
	     emit(I.ARITH{a=I.OR,r=t,i=I.IMMED lo,cc=false,d=d}))
        end

  (* load word constant *)
  fun loadImmed32(w,d) =
      if immed13w w then 
         emit(I.ARITH{a=I.OR,r=0,i=I.IMMED(W.toIntX w),cc=false,d=d})
      else let 
	  val t = newReg()
	  val {hi,lo} = splitw w
        in
	  if lo = 0 then emit(I.SETHI{i=hi,d=d})
	  else
	    (emit(I.SETHI{i=hi,d=t});
	     emit(I.ARITH{a=I.OR,r=t,i=I.IMMED lo,cc=false,d=d}))
        end

  (* load constant *)
  fun loadConst(c,d) = emit(I.ARITH{a=I.OR,r=0,i=I.CONST c,cc=false,d=d})

  (* load label expression *)
  fun loadLabel(lab,d) = emit(I.ARITH{a=I.OR,r=0,i=I.LAB lab,cc=false,d=d})

  (* emit parallel copies *) 
  fun copy(dst,src) = 
    emit(I.COPY{dst=dst, src=src, impl=ref NONE,
		tmp=case dst 
		    of [_] => NONE 
		     | _ => SOME(I.Direct(newReg()))})
	
  fun fcopy(dst,src) = 
    emit(I.FCOPY{dst=dst, src=src, impl=ref NONE,
              tmp=case dst 
		   of [_] => NONE 
	   	    | _ => SOME(I.FDirect(newReg()))})

  (* move register s to register d *) 
  fun move(s,d) = 
    if s = d orelse d = 0 then () 
    else emit(I.COPY{dst=[d],src=[s],tmp=NONE, impl=ref NONE})

  (* move floating point register s to register d *) 
  fun fmove(s,d) = 
    if s = d then ()
    else emit(I.FCOPY{dst=[d],src=[s],tmp=NONE,impl=ref NONE})

  (* order the generation of instructions *)
  fun order(gen,e1,e2,T.LR) = (gen e1, gen e2)
    | order(gen,e1,e2,T.RL) = let 
	val y = gen e2
      in (gen e1, y) 
      end 

  (* generate arithmetic *)
  fun arith(opcode,e1,e2,ord,d,cc,commutative,checkOverflow) =
     let val d = case cc of
                   CC => 0
                 | _  => d
         val cc = case cc of
                    CC => true
                  | CC_REG => true
                  | REG => false
     in  case (order(genOperand,e1,e2,ord),commutative) of
           ((i,I.REG r),COMMUTE) => emit(I.ARITH{a=opcode,r=r,i=i,d=d,cc=cc})
         | ((I.REG r,i),_)       => emit(I.ARITH{a=opcode,r=r,i=i,d=d,cc=cc})
         | ((a,i),_) => 
           let val r = newReg()
           in  emit(I.ARITH{a=I.OR,r=0,i=a,d=r,cc=false});
               emit(I.ARITH{a=opcode,r=r,i=i,d=d,cc=cc})
           end;
         case checkOverflow of
            TRAPS => app emit overflowtrap
          | SILENT => ()
     end

  (* generate shift *)
  and shift(opcode,e1,e2,ord,d) =
      case order(genOperand,e1,e2,ord) of
         (I.REG r,i) => emit(I.SHIFT{s=opcode,r=r,i=i,d=d})
      |  (a,i) => let val r = newReg()
                  in  emit(I.ARITH{a=I.OR,r=0,i=a,d=r,cc=false});
                      emit(I.SHIFT{s=opcode,r=r,i=i,d=d})
                  end

  (* generate external arithmetic operation *)
  and externalarith(gen,e1,e2,ord,d,cc,commutative) =
      let val instrs =
            case (order(genOperand,e1,e2,ord),commutative) of
              ((i,I.REG r),COMMUTE) => gen({r=r,i=i,d=d},reduceOperand)
            | ((I.REG r,i),_) => gen({r=r,i=i,d=d},reduceOperand)
            | ((a,i),_)   => let val r = newReg()
                             in  emit(I.ARITH{a=I.OR,r=0,i=a,d=r,cc=false});
                                 gen({r=r,i=i,d=d},reduceOperand)
                             end
      in  app emit instrs;
          genCmp0(cc,d)
      end

  (* Convert an operand into a register *)
  and reduceOperand(I.REG r) = r
    | reduceOperand(I.IMMED 0) = 0 (* %g0 *)
    | reduceOperand i = let val d = newReg()
                        in  emit(I.ARITH{a=I.OR,r=0,i=i,d=d,cc=false}); d end

  (* floating point arithmetic *)
  and funary(opcode,e,d) = emit(I.FPop1{a=opcode,r=genFexpr e,d=d})

  and farith(opcode,e1,e2,d,ord) =
      let val (r1,r2) = order(genFexpr,e1,e2,ord)
      in  emit(I.FPop2{a=opcode,r1=r1,r2=r2,d=d})
      end

  (* compute addressing mode
   * Sparc has only two addressing modes: displacement and indexed.
   *)
  and addrMode(T.ADD(e,T.LI n))      = 
        if immed13 n then (genExpr e,I.IMMED n) 
        else let val t = newReg()
                 val _ = loadImmed(n,t)
             in  (t,genOperand e) end
    | addrMode(T.ADD(e,T.CONST c))   = (genExpr e,I.CONST c) 
    | addrMode(T.ADD(e,T.LABEL l))   = (genExpr e,I.LAB l)
    | addrMode(T.ADD(i as T.LI _,e)) = addrMode(T.ADD(e,i))
    | addrMode(T.ADD(T.CONST c,e))   = (genExpr e,I.CONST c)
    | addrMode(T.ADD(T.LABEL l,e))   = (genExpr e,I.LAB l)
    | addrMode(T.ADD(e1,e2))         = (genExpr e1,I.REG(genExpr e2))
    | addrMode(T.SUB(e,T.LI n,_))    = addrMode(T.ADD(e,T.LI(~n)))
    | addrMode(T.LABEL l)            = (0,I.LAB l)
    | addrMode addr                  = (genExpr addr,I.IMMED 0)

  (* load integer values *)
  and load(opcode,addr,mem,d) =
      let val (r,i) = addrMode addr
      in  emit(I.LOAD{l=opcode,r=r,i=i,d=d,mem=mem}) end

  (* store integer values *)
  and store(opcode,addr,data,mem) =
      let val (r,i) = addrMode addr
      in  emit(I.STORE{s=opcode,d=data,r=r,i=i,mem=mem}) end

  (* load floating point value *)
  and fload(opcode,addr,mem,d) =
      let val (r,i) = addrMode addr
      in  emit(I.FLOAD{l=opcode,r=r,i=i,d=d,mem=mem}) end

  (* store floating point value *)
  and fstore(opcode,addr,data,mem) =
      let val (r,i) = addrMode addr
      in  emit(I.FSTORE{s=opcode,d=data,r=r,i=i,mem=mem}) end

  and jmp(addr,labs) =
      let val (r,i) = addrMode addr
      in  emit(I.JMP{r=r,i=i,labs=labs,nop=true}) end

  and call(addr,defs,uses) =
      let val (r,i) = addrMode addr
          fun live([],acc) = acc
            | live(T.GPR(T.REG r)::regs,acc) = live(regs, C.addReg(r,acc))
            | live(T.CCR(T.CC 65)::regs,acc) = live(regs, acc)
            | live(T.CCR(T.CC cc)::regs,acc) = live(regs, C.addReg(cc,acc))
            | live(T.FPR(T.FREG f)::regs,acc) = live(regs, C.addFreg(f,acc))
            | live(_::regs, acc) = live(regs, acc)
          val defs=live(defs,C.empty)
          val uses=live(uses,C.empty)
      in  case (r,i) of
            (0,I.LAB(LE.LABEL l)) =>
             emit(I.CALL{label=l,defs=C.addReg(C.linkReg,defs),uses=uses,
                         nop=true})
          | _ => emit(I.JMPL{r=r,i=i,d=C.linkReg,defs=defs,uses=uses,nop=true}) 
      end

  (* Generate code for a statement *)
  and doStmt stmt =
      case stmt of
         T.MV(d,e)        => doExpr(e,d,REG)
      |  T.FMV(d,e)       => doFexpr(e,d)
      |  T.CCMV(d,e)      => doCCexpr(e,d)
      |  T.COPY(dst,src)  => copy(dst,src)
      |  T.FCOPY(dst,src) => fcopy(dst,src)
      |  T.JMP(T.LABEL(LE.LABEL l),_) => 
             emit(I.Bicc{b=I.BA,a=true,label=l,nop=false})
      |  T.JMP(e,labs) => jmp(e,labs)
      |  T.CALL(e,def,use) => call(e,def,use)
      |  T.RET => emit(I.RET{leaf=false,nop=true})
      |  T.STORE8(addr,data,mem) => store(I.STB,addr,genExpr data,mem)
      |  T.STORE32(addr,data,mem) => store(I.ST,addr,genExpr data,mem)
      |  T.STORED(addr,data,mem) => fstore(I.STDF,addr,genFexpr data,mem)
      |  T.STORECC(addr,data,mem) => store(I.ST,addr,genCCexpr data,mem)
      |  T.BCC(cond,cc,lab) => branch(cond,cc,lab)
      |  T.FBCC(cond,cc,lab) => fbranch(cond,cc,lab)

       (* 
        * generate conditional branches 
        * Perform a subtract (with cc), then branch on cc.
        * Note: when we are comparing with zero, do something smarter.
        *)
   and branch(_,T.CMP(cond,e1,e2,order),lab) = 
       let val (cond,e1,e2) = 
            case e1 of
              (T.LI _ | T.LI32 _ | T.CONST _ | T.LABEL _) => (flip cond,e2,e1)
            | _ => (cond,e1,e2)
       in  doExpr(T.SUB(e1,e2,order),newReg(),CC); br(cond,lab)
       end
     | branch(cond,T.CC 65,lab) = (* psr *)
         br(cond,lab)
     | branch(cond,T.CC r,lab) = 
         (genCmp0(CC,r); br(cond,lab))
     | branch _ = error "branch"

   and cond T.LT  = I.BL
     | cond T.LTU = I.BCS
     | cond T.LE  = I.BLE
     | cond T.LEU = I.BLEU
     | cond T.EQ  = I.BE
     | cond T.NEQ = I.BNE
     | cond T.GE  = I.BGE
     | cond T.GEU = I.BCC
     | cond T.GT  = I.BG
     | cond T.GTU = I.BGU

       (* exchange the order of the arguments to a comparison *)
   and flip T.LT  = T.GT
     | flip T.LTU = T.GTU
     | flip T.LE  = T.GE
     | flip T.LEU = T.GEU
     | flip T.EQ  = T.EQ
     | flip T.NEQ = T.NEQ
     | flip T.GE  = T.LE
     | flip T.GEU = T.LEU
     | flip T.GT  = T.LT
     | flip T.GTU = T.LTU

   and fcond T.==  = I.FBE
     | fcond T.?<> = I.FBNE
     | fcond T.?   = I.FBU
     | fcond T.<=> = I.FBO
     | fcond T.>   = I.FBG
     | fcond T.>=  = I.FBGE
     | fcond T.?>  = I.FBUG
     | fcond T.?>= = I.FBUGE
     | fcond T.<   = I.FBL
     | fcond T.<=  = I.FBLE
     | fcond T.?<  = I.FBUL
     | fcond T.?<= = I.FBULE
     | fcond T.<>  = I.FBLG
     | fcond T.?=  = I.FBUE

   and br(c,lab) = emit(I.Bicc{b=cond c,a=true,label=lab,nop=true})

   and fbranch(_,T.FCMP(cond,e1,e2,ord),lab) =
        let val (r1,r2) = order(genFexpr,e1,e2,ord)
        in  emit(I.FCMP{cmp=I.FCMPd,r1=r1,r2=r2,nop=true});
            emit(I.FBfcc{b=fcond cond,a=false,label=lab,nop=true})
        end
     | fbranch _ = error "fbranch"

       (* compute expr and write the result to register d,
        * optionally set the condition code register.
        *)
   and doExpr(expr,d,cc) =
       case expr of
          T.REG r          => (move(r,d); genCmp0(cc,r))
       |  T.LI n           => (loadImmed(n,d); genCmp0(cc,d))
       |  T.LI32 w         => (loadImmed32(w,d); genCmp0(cc,d))
       |  T.LABEL lab      => (loadLabel(lab,d); genCmp0(cc,d))
       |  T.CONST c        => (loadConst(c,d); genCmp0(cc,d))
       |  T.ADD(e1,e2)     => arith(I.ADD,e1,e2,T.LR,d,cc,COMMUTE,SILENT)
       |  T.SUB(e1,T.LI 0,_) => doExpr(e1,d,cc)
       |  T.SUB(e1,T.LI32 0w0,_) => doExpr(e1,d,cc)
       |  T.SUB(e1,e2,ord) => arith(I.SUB,e1,e2,ord,d,cc,DONTCOMMUTE,SILENT)
       |  T.ADDT(e1,e2)    => arith(I.ADD,e1,e2,T.LR,d,CC_REG,COMMUTE,TRAPS)
       |  T.SUBT(e1,e2,ord)=> arith(I.SUB,e1,e2,ord,d,CC_REG,DONTCOMMUTE,TRAPS)
       |  T.ANDB(e1,e2)    => arith(I.AND,e1,e2,T.LR,d,cc,COMMUTE,SILENT)
       |  T.ORB(e1,e2)     => arith(I.OR,e1,e2,T.LR,d,cc,COMMUTE,SILENT)
       |  T.XORB(e1,e2)    => arith(I.XOR,e1,e2,T.LR,d,cc,COMMUTE,SILENT)
       |  T.SRA(e1,e2,ord) => (shift(I.SRA,e1,e2,ord,d); genCmp0(cc,d))
       |  T.SRL(e1,e2,ord) => (shift(I.SRL,e1,e2,ord,d); genCmp0(cc,d))
       |  T.SLL(e1,e2,ord) => (shift(I.SLL,e1,e2,ord,d); genCmp0(cc,d))
       |  T.LOAD8(addr,mem) => (load(I.LDUB,addr,mem,d); genCmp0(cc,d))
       |  T.LOAD32(addr,mem) => (load(I.LD,addr,mem,d); genCmp0(cc,d))
       |  T.SEQ(stmt,e) => (doStmt stmt; doExpr(e,d,cc))
       |  T.MULU(e1,e2)    => externalarith(P.umul,e1,e2,T.LR,d,cc,COMMUTE)
       |  T.MULT(e1,e2)    => externalarith(P.smul,e1,e2,T.LR,d,cc,COMMUTE)
       |  T.DIVU(e1,e2,ord)=> externalarith(P.udiv,e1,e2,ord,d,cc,DONTCOMMUTE)
       |  T.DIVT(e1,e2,ord)=> externalarith(P.sdiv,e1,e2,ord,d,cc,DONTCOMMUTE)

      (* Compare with zero if cc is set *)
  and genCmp0(cc,d) = 
      case cc of
         REG => ()
      |  _   => emit(I.ARITH{a=I.SUB,r=d,i=I.IMMED 0,d=0,cc=true})

  and doFexpr(expr,d) =
      case expr of
         T.FREG r           => fmove(r,d)
      |  T.LOADD(addr,mem)  => fload(I.LDDF,addr,mem,d)
      |  T.FADDD(e1,e2)     => farith(I.FADDd,e1,e2,d,T.LR)
      |  T.FMULD(e1,e2)     => farith(I.FMULd,e1,e2,d,T.LR)
      |  T.FSUBD(e1,e2,ord) => farith(I.FSUBd,e1,e2,d,ord)
      |  T.FDIVD(e1,e2,ord) => farith(I.FDIVd,e1,e2,d,ord)
      |  T.FABSD e          => funary(I.FABSd,e,d)
      |  T.FNEGD e          => funary(I.FNEGd,e,d)
      |  T.CVTI2D e         => app emit 
                                 (P.cvti2d({i=genOperand e,d=d},reduceOperand))
      |  T.FSEQ(stmt,e)     => (doStmt stmt; doFexpr(e,d))

   and doCCexpr(T.CMP(cond,e1,e2,ord),65) = (* psr *)
         doExpr(T.SUB(e1,e2,ord),newReg(),CC)
     | doCCexpr(_,65) = error "doCCexpr 65"
     | doCCexpr(expr,d) =
       case expr of
          T.CC r => move(r,d)
       |  T.LOADCC(addr,mem) => load(I.LD,addr,mem,d)
       |  _ => error "doCCexpr"

      (* 
       * generate an expression and return the register that holds its value   
       *)
  and genExpr(T.LI 0) = 0 (* register %g0 *)
    | genExpr(T.LI32 0w0) = 0 (* register %g0 *)
    | genExpr(T.REG r) = r 
    | genExpr expr = let val r = newReg() in doExpr(expr,r,REG); r end

  and genFexpr(T.FREG r) = r
    | genFexpr expr = let val r = newFreg() in doFexpr(expr,r); r end

  and genCCexpr(T.CC 65) = error "genCCexpr"
    | genCCexpr(T.CC r) = r
    | genCCexpr expr = let val r = newReg() in doCCexpr(expr,r); r end

      (*
       * generate an expression and returns it as an operand
       *)
  and genOperand(T.LI 0)     = I.REG 0
    | genOperand(T.LI32 0w0) = I.REG 0
    | genOperand(e as T.LI n) = 
          if immed13 n then I.IMMED n else I.REG(genExpr e)
    | genOperand(e as T.LI32 w)  = 
          if immed13w w then I.IMMED(W.toIntX w) else I.REG(genExpr e)
    | genOperand(T.CONST c) = I.CONST c
    | genOperand(T.LABEL l) = I.LAB l
    | genOperand(e)         = I.REG(genExpr e)

  fun mltreeComp mltree =
  let (* condition code registers are mapped onto general registers *)
      fun cc (x as T.CCR(T.CC 65),l) = l
        | cc (T.CCR(T.CC cc),l) = T.GPR(T.REG cc)::l
        | cc (x,l) = x::l
      fun comp(T.BEGINCLUSTER)      = F.beginCluster()
        | comp(T.PSEUDO_OP p)       = F.pseudoOp p
        | comp(T.DEFINELABEL lab)   = F.defineLabel lab
        | comp(T.ENTRYLABEL lab)    = F.entryLabel lab
        | comp(T.CODE stmts)        = app doStmt stmts
        | comp(T.BLOCK_NAME name)  = F.blockName name
        | comp(T.ORDERED mltrees)   = F.ordered mltrees
        | comp(T.ESCAPEBLOCK regs)  = F.exitBlock(foldl cc [] regs)
        | comp(T.ENDCLUSTER regmap) = F.endCluster regmap
  in  comp mltree 
  end

  val mlriscComp = doStmt 

end

(* 
 * $Log$
# Revision 1.2  2001/12/13  16:32:28  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:46  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:48  pscheng
# *** empty log message ***
#
 * Revision 1.4  1998/09/30 19:36:54  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.3  1998/08/12 13:36:15  leunga
 *
 *
 *   Fixed the 2.0 + 2.0 == nan bug by treating FCMP as instrs with delay slots
 *
 * Revision 1.2  1998/08/11 14:03:25  george
 *   Exposed emitInstr in MLTREECOMP to allow a client to directly
 *   inject native instructions into the flowgraph.
 *
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
