(* alpha32Asm.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor Alpha32AsmEmitter
  (structure Instr : ALPHA32INSTR
   structure PseudoOps : PSEUDO_OPS 
   structure Shuffle : ALPHA32SHUFFLE where I = Instr)  : EMITTER_NEW = 
struct
  structure I = Instr
  structure C = I.C
  structure P = PseudoOps
  structure R = I.Region

  structure Constant = I.Constant

  fun ms n = if n<0 then ("-" ^ Int.toString (~n)) else Int.toString n

  fun error msg = MLRiscErrorMsg.impossible ("Alpha32AsEmitter." ^ msg)

  fun emit s = TextIO.output(!AsmStream.asmOutStream,s)

  fun pseudoOp pOp = emit(P.toString pOp)

  fun defineLabel(lab) = emit(Label.nameOf lab ^ ":\n")

  fun comment msg = emit ("\t/* " ^ msg ^ " */")

  fun init size = (comment ("Code Size = " ^ ms size);
		   emit ".set\tnoat\n")

    
  fun emitInstr(instr,regmap) = let
    datatype register = REG | FREG

    fun rmap r			= (Intmap.map regmap r) handle _ => r
    fun eReg (i) 		= emit ("$" ^ ms(rmap i))
    fun eFreg(f)         	= emit("$f" ^ ms(rmap f))
    fun eLabel lab       	= emit (Label.nameOf lab)
    fun newline ()          	= emit "\n"
    fun comma() 	    	= emit ", "
    fun tab()               	= emit "\t"
    fun emitLExp lexp = emit(LabelExp.toString lexp)

    fun eOperand (I.REGop r) = eReg r
      | eOperand (I.IMMop n) = emit (ms n)
      | eOperand (I.CONSTop c) = emit(Constant.toString c)
      | eOperand (I.LOLABop l)  = (emit "LO("; emitLExp l;emit ")")
      | eOperand (I.HILABop l) =  (emit "HI("; emitLExp l; emit ")")
      | eOperand (I.LABop l) = emitLExp l

    fun parenthesize f = (emit "("; f(); emit ")")

    fun eDisp(rd, disp)	= (eOperand(disp); parenthesize (fn () => eReg rd))

    fun eMemFormat REG (reg, base, disp) =
         (eReg reg; comma(); eDisp (base, disp))
      | eMemFormat FREG (freg, base, disp) =
         (eFreg freg; comma(); eDisp (base, disp))

    fun eBrFormat REG (reg, lab) =
          (eReg reg; comma(); eLabel lab)
      | eBrFormat FREG (freg, lab) =
	  (eFreg freg; comma(); eLabel lab)

    fun eOpFormat (rs, opnd, rd) =
      (eReg rs; comma(); eOperand opnd; comma(); eReg rd)

    fun eFOpFormat (f1, f2, fd) = 
      (eFreg f1; comma(); eFreg f2; comma(); eFreg fd)

    fun eFOpFormat2 (31, f2, fd) = (eFreg f2; comma(); eFreg fd)
      | eFOpFormat2 arg = eFOpFormat arg

    fun emitLDA {r, b, d} =
      (eReg r; comma();  
       case (b, d)
       of (31, _) => eOperand d
        | _ => (eOperand d;  parenthesize(fn () => eReg b))
       (*esac*))

    fun emitJumps{r, b, d=0} = 
         (eReg r; comma(); parenthesize(fn () => eReg b))
      | emitJumps _ = error "emitJumps"

    fun branch(I.BR) 	= "br"
      | branch(I.BEQ) 	= "beq"
      | branch(I.BGE) 	= "bge"
      | branch(I.BGT) 	= "bgt"
      | branch(I.BLE) 	= "ble"
      | branch(I.BLT) 	= "blt"
      | branch(I.BNE) 	= "bne"
      | branch(I.BLBC) 	= "blbc"
      | branch(I.BLBS) 	= "blbs"
  in
    ((case instr 
      of I.DEFFREG f   => comment("deffreg\t$f" ^ ms(rmap f))
       | I.LDA arg => (emit "\tlda\t"; emitLDA arg)
       | I.LDAH arg =>(emit "\tldah\t"; emitLDA arg)
       | I.LOAD{ldOp, r, b, d, mem} => 
	  (emit(case ldOp
	    of I.LDL => "\tldl\t"
  	     | I.LDQ => "\tldq\t"
	     | I.LDQ_U => "\tldq_u\t"
            (*esac*));
	   eMemFormat REG (r, b, d);
	   comment(R.toString mem))
       | I.FLOAD{ldOp, r, b, d, mem} => 
	  (emit(case ldOp
	        of I.LDT => "\tldt\t"
	         | I.LDS => "\tlds\t"
		(*esac*));
	   eMemFormat FREG (r, b, d);
	   comment (R.toString mem))
       | I.STORE{stOp, r, b, d, mem} => 
	  (emit(case stOp
	     of I.STL =>  "\tstl\t"
              | I.STQ => "\tstq\t"
  	      | I.STQ_U => "\tstq_u\t"
             (*esac*));
	   eMemFormat REG (r, b, d); 
	   comment(R.toString mem))
       | I.FSTORE{stOp=I.STT, r, b, d, mem} => 
	  (emit "\tstt\t"; eMemFormat FREG (r, b, d); comment(R.toString mem))

       | I.JMPL(arg, _)	=> (emit "\tjmp\t"; emitJumps arg)
       | I.JSR(arg, defs, uses)=> (emit "\tjsr\t"; emitJumps arg)
       | I.BRANCH(brOp, reg, lab) => 
	   (emit("\t" ^ branch brOp ^ "\t"); eBrFormat REG (reg, lab))
       | I.FBRANCH(fbrOp, freg, lab) => 
	   (emit("\tf" ^ branch fbrOp ^ "\t"); eBrFormat FREG (freg, lab))
       | I.OPERATE{oper=I.BIS, ra=27, rb=I.REGop 31, rc=29} => 
	   emit "\tldgp\t$29, 0($27)"
       | I.OPERATE{oper=I.BIS, ra=26, rb=I.REGop 31, rc=29} => 
	   emit "\tldgp\t$29, 0($26)"
	   (* ignore empty stack frame allocation/deallocation instructions.
	    * This can be generalized to any register if SGNXL is no longer
	    * required.
	    *)
       | I.OPERATE{oper=I.ADDL, ra=30, rb=rb as I.CONSTop b, rc=30} =>
	   if Constant.valueOf b = 0 then ()
	   else (emit "\taddl\t"; eOpFormat(30, rb, 30))
       | I.OPERATE{oper=I.SUBL, ra=30, rb=rb as I.CONSTop b, rc=30} =>
	   if Constant.valueOf b = 0 then ()
	   else (emit "\tsubl\t"; eOpFormat(30, rb, 30))
       | I.OPERATE{oper, ra, rb, rc} => 
	  (emit(case oper
	     of I.ZAP    	=> "\tzap\t"  
	      | I.ADDL   	=> "\taddl\t"  
	      | I.ADDQ   	=> "\taddq\t"  
	      | I.SUBL   	=> "\tsubl\t"  
	      | I.SUBQ   	=> "\tsubq\t"  
	      | I.MULL   	=> "\tmull\t"  
	      | I.S4ADDL	=> "\ts4addl\t"
	      | I.S8ADDL	=> "\ts8addl\t"
	      | I.CMPULE  	=> "\tcmpule\t"  
	      | I.CMPULT  	=> "\tcmpult\t"  
	      | I.CMPEQ   	=> "\tcmpeq\t"  
	      | I.CMPLE   	=> "\tcmple\t"  
	      | I.CMPLT   	=> "\tcmplt\t"  
	      | I.SGNXL         => "\taddl\t" 
	      | I.AND   	=> "\tand\t"  
	      | I.BIS   	=> "\tbis\t"  
	      | I.XOR   	=> "\txor\t"  
	      | I.SRA   	=> "\tsra\t"  
	      | I.SRL   	=> "\tsrl\t"  
	      | I.SLL   	=> "\tsll\t"  
	      | I.INSBL   	=> "\tinsbl\t"  
	      | I.EXTBL   	=> "\textbl\t"  
	      | I.EXTQH   	=> "\textqh\t"  
	      | I.MSKBL   	=> "\tmskbl\t"  
	      | I.MSKLH   	=> "\tmsklh\t"
             (*esac*));
           eOpFormat(ra, rb, rc))  
       | I.PSEUDOARITH{oper, ra, rb, rc, ...} => 
	  (emit(case oper
	     of I.DIVL		=> "\tdivl\t"
	      | I.DIVLU		=> "\tdivlu\t"
	     (*esac*));
	   eOpFormat(ra, rb, rc))

       | I.OPERATEV{oper, ra, rb, rc} => 
	  (emit(case oper
	     of I.ADDLV  	=> "\taddlv\t"  
	      | I.SUBLV  	=> "\tsublv\t"  
	      | I.MULLV   	=> "\tmullv\t" 
             (*esac*));
           eOpFormat(ra, rb, rc))

       | I.FOPERATE{oper, fa, fb, fc} => 
	  (emit(case oper
	     of I.CPYS 	 => "\tcpys\t"
	      | I.CPYSN	 => "\tcpysn\t"
	      | I.CVTLQ  => "\tcvtlq\t"
	      | I.CVTQT  => "\tcvtqt\t"
	      | I.CMPTEQ => "\tcmpteqsu\t"
	      | I.CMPTLT => "\tcmptltsu\t"
	      | I.CMPTLE => "\tcmptlesu\t"
	      | I.CMPTUN => "\tcmptunsu\t"
	     (*esac*));
	  case oper 
	     of I.CVTQT => eFOpFormat2(fa, fb, fc)
	      | _ => eFOpFormat2(fa, fb, fc)
             (*esac*))
       | I.FOPERATEV{oper, fa, fb, fc} => 
	  (emit(case oper 
	     of I.CVTTQ	=> "\tcvttqc\t"
	      | I.ADDT 	=> "\taddtsud\t"
	      | I.SUBT 	=> "\tsubtsud\t"
	      | I.MULT 	=> "\tmultsud\t"
	      | I.DIVT 	=> "\tdivtsud\t"
             (*esac*));
           case oper
	     of I.CVTTQ => eFOpFormat2(fa, fb, fc)
	      | _ => eFOpFormat(fa, fb, fc)
           (*esac*))

       | I.COPY{dst, src, tmp, ...} => 
	  app (fn instr => (emit "\t"; emitInstr(instr, regmap)))
	      (Shuffle.shuffle
	         {regMap=rmap, temp=tmp, dst=dst, src=src})
       | I.FCOPY{dst, src, tmp, ...} => 
	  app (fn I => (emit "\t"; emitInstr(I, regmap)))
	      (Shuffle.shufflefp
	         {regMap=rmap, temp=tmp, dst=dst, src=src})
       | I.TRAPB 	=> emit"\ttrapb\t"

       | I.CALL_PAL{code, ...} => 
	  (emit "\tcall_pal\t";
	   emit(case code
             of I.BPT => "0x80" | I.BUGCHK => "0x81" | I.CALLSYS => "0x83"
	      | I.GENTRAP => "0xaa" | I.IMB => "0x86"
	      | I.RDUNIQUE => "0x9e" | I.WRUNIQUE => "0x9f"
	   (*esac*)))
     (* esac *));
     emit "\n"
    (*esac*))
  end

end





