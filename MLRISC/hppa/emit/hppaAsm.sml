(* hppaAsm.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor HppaAsmEmitter
  (structure Instr : HPPAINSTR
   structure PseudoOps : PSEUDO_OPS
   structure Shuffle : HPPASHUFFLE where I = Instr) : EMITTER_NEW = 
struct
  structure I = Instr
  structure C = I.C
  structure P = PseudoOps
  structure LE = LabelExp
  structure Constant = I.Constant
  structure Region = I.Region

  fun ms n = if n<0 then ("-"^ Int.toString (~n)) else Int.toString n

  fun error msg = MLRiscErrorMsg.impossible ("HppaAsEmitter." ^ msg)

  fun emit s = TextIO.output(!AsmStream.asmOutStream,s)

  fun defineLabel(lab) =     emit(Label.nameOf lab ^ "\n")

  fun comment msg = emit ("\t/* " ^ msg ^ " */")
  fun region r = comment(Region.toString r)
  fun pseudoOp pOp = emit(P.toString pOp)
  fun init size = (comment ("Code Size = " ^ ms size); emit"\n")
    
  fun emitInstr(instr,regmap) = let
    fun rmap r		= Intmap.map regmap r handle _ => r
    fun eReg r		= emit ("%r" ^ ms(rmap r))
    fun eFreg f		= emit ("%f" ^ ms(rmap f))
    fun emitCreg c      = emit ("%f" ^ ms c)
    fun eLabel lab	= emit (Label.nameOf lab)
    fun comma() 	= emit ", "
    fun tab()           = emit "\t"
    fun newline ()      = emit "\n"
    fun paren f         = (emit "("; f(); emit ")")
    fun prList l = let
      fun pr [] = emit "]"
	| pr [r] = (eReg r; pr [])
        | pr (r::rs) = (eReg r; emit ","; pr rs)
    in emit "["; pr l
    end
    fun cond I.EQ  = "="
      | cond I.LT  = "<"
      | cond I.LE  = "<="
      | cond I.LTU = "<<"
      | cond I.LEU = "<<="
      | cond I.NE  = "<>"
      | cond I.GE  = ">="
      | cond I.GT  = ">"
      | cond I.GTU = ">>"
      | cond I.GEU = ">>="

    fun fcond I.?    = "?"
      | fcond I.!<=> = "!<=>"
      | fcond I.==   = "=="
      | fcond I.?=   = "?="
      | fcond I.!<>  = "!<>"
      | fcond I.!?>= = "!?>="
      | fcond I.<    = "<"
      | fcond I.?<   = "?<"
      | fcond I.!>=  = "!>="
      | fcond I.!?>  = "!?>"
      | fcond I.<=   = "<="
      | fcond I.?<=  = "?<+"
      | fcond I.!>   = "!>"
      | fcond I.!?<= = "!?<="
      | fcond I.>    = ">"
      | fcond I.?>   = "?>"
      | fcond I.!<=  = "!<="
      | fcond I.!?<  = "!?<"
      | fcond I.>=   = ">="
      | fcond I.?>=  = "?>="
      | fcond I.!<   = "!<"
      | fcond I.!?=  = "!?="
      | fcond I.<>   = "<>"
      | fcond I.!=   = "!="
      | fcond I.!?   = "!?"
      | fcond I.<=>  = "<=>"

    fun emitFieldSel fs = 
      emit(case fs
	   of I.F => "F'"
            | I.T => ""
	    | I.P => "P'"
	    | _ => error "emitFieldSel")

    fun emitHiFieldSel fs =
      emit(case fs
	   of I.F => "L'"
	    | I.S => "LS'"
	    | I.D => "LD'"
	    | I.R => "LR'"
	    | I.T => "LT'"
	    | I.P => "LP'")

    fun emitLoFieldSel fs = 
      emit(case fs
	   of I.F => "R'"
	    | I.S => "RS'"
	    | I.D => "RD'"
	    | I.R => "RR'"
	    | I.T => "RT'"
	    | I.P => "RP'")

    fun emitLExp lexp = emit(LabelExp.toString lexp)
    fun eOperand(I.IMMED  i) = emit (ms i)
      | eOperand(I.LabExp(lexp, fs)) = (emitFieldSel fs; emitLExp lexp)
      | eOperand(I.HILabExp(lexp, fs)) = (emitHiFieldSel fs; emitLExp lexp)
      | eOperand(I.LOLabExp(lexp, fs)) = (emitLoFieldSel fs; emitLExp lexp)
      | eOperand(I.ConstOp c) = emit(Constant.toString c)

    fun store(st, b, d, r, mem) = 
      (emit st; eReg r; comma(); eOperand d; paren(fn () => eReg b); region mem)
    fun fstore(fst, b, d, r, mem) = 
      (emit fst; eFreg r; comma(); emit(ms d); paren(fn () => eReg b);
       region mem)
    fun fstorex(fstx, b, x, r, mem) = 
      (emit fstx; eFreg r; eReg x; paren(fn () => eReg b); region mem)
    fun loadx(ld, r1, r2, t, mem) = 
      (emit ld; eReg r2; paren(fn () => eReg r1); comma(); eReg t; region mem)
    fun floadx(flx, b, x, t, mem) = 
      (emit flx; eReg x; paren(fn () => eReg b); comma(); eFreg t; region mem)
    fun arith(a, r1, r2, t) = 
      (emit a; eReg r1; comma(); eReg r2; comma(); eReg t)
    fun loadi(ld, i, r, t, mem) = 
      (emit ld; eOperand i; paren(fn () => eReg r); comma(); eReg t; region mem)
    fun fload(fl, b, d, t, mem) = 
      (emit fl; emit(ms d); paren(fn () => eReg b); comma(); eFreg t; region mem)
    fun arithi(ai, i, r ,t) =
      (emit ai; eOperand i; comma(); eReg r; comma(); eReg t)
    fun cmp{cc, r1, r2, t} = arith("\tCOMCLR," ^ cond cc ^ "\t", r1, r2, t)
    fun shiftv(sv, r, len, t) = 
      (emit sv; eReg r; comma(); emit(ms len); comma(); eReg t)
    fun shift(s, r, p, len, t) = 
      (emit s; eReg r; comma(); 
       emit(ms p); comma(); 
       emit(ms len); comma(); eReg t)
    fun emitCmp I.COMBT  = emit "\tCOMBT,"
      | emitCmp I.COMBF  = emit "\tCOMBF,"
    fun emitCmpi I.COMIBT = emit "\tCOMIBT,"  
      | emitCmpi I.COMIBF = emit "\tCOMIBF," 
    fun nullify true  = emit ",n"
      | nullify false = ()
    fun bcond{cmp, bc, r1, r2, t, f, n} = 
      (emitCmp cmp;  emit (cond bc); nullify n; emit "\t"; 
       eReg r1; comma(); eReg r2; comma(); eLabel t;
       emit "\n\tNOP" )
    fun bcondi{cmpi, bc, i, r2, t, f, n} = 
      (emitCmpi cmpi; emit (cond bc); nullify n; emit "\t";
       emit(ms i); comma(); eReg r2; comma(); eLabel t;
       emit "\n\tNOP")

    fun farith{fa, r1, r2, t} = let
      val oper = case fa of I.FADD => "\tFADD\t" | I.FSUB => "\tFSUB\t"
			  | I.FDIV => "\tFDIV\t" | I.FMPY => "\tFMPY\t"
			  | I.XMPYU => "\tXMPYU\t"
    in emit oper; eFreg r1; comma(); eFreg r2; comma(); eFreg t
    end

    fun funary(fu, f, t) = (emit fu; eFreg f; comma(); eFreg t)
  in
    case instr
     of I.STORE{st=I.STW, b, d, r, mem}	=> store("\tSTW\t", b, d, r, mem)
      | I.STORE{st=I.STH, b, d, r, mem}	=> store("\tSTH\t", b, d, r, mem)
      | I.STORE{st=I.STB, b, d, r, mem}	=> store("\tSTB\t", b, d, r, mem)

      | I.LOAD{l=I.LDWX, r1, r2, t, mem}=> loadx("\tLDWX\t", r1, r2, t, mem)
      | I.LOAD{l=I.LDHX, r1, r2, t, mem}=> loadx("\tLDHX\t", r1, r2, t, mem)
      | I.LOAD{l=I.LDBX, r1, r2, t, mem}=> loadx("\tLDBX\t", r1, r2, t, mem)
      | I.ARITH{a=I.ADD,  r1, r2, t}	=> arith("\tADD\t", r1, r2, t)
      | I.ARITH{a=I.ADDO,  r1, r2, t}	=> arith("\tADDO\t", r1, r2, t)
      | I.ARITH{a=I.SH1ADD,  r1, r2, t}	=> arith("\tSH1ADD\t", r1, r2, t)
      | I.ARITH{a=I.SH1ADDO, r1, r2, t} => arith("\tSH1ADDO\t", r1, r2, t)
      | I.ARITH{a=I.SUB,  r1, r2, t}	=> arith("\tSUB\t", r1, r2, t)
      | I.ARITH{a=I.SUBO,  r1, r2, t}	=> arith("\tSUBO\t", r1, r2, t)
      | I.ARITH{a=I.OR,  r1, r2, t}	=> arith("\tOR\t", r1, r2, t)
      | I.ARITH{a=I.XOR,  r1, r2, t}	=> arith("\tXOR\t", r1, r2, t)
      | I.ARITH{a=I.AND,  r1, r2, t}	=> arith("\tAND\t", r1, r2, t)

      | I.LOADI{li=I.LDB, i, r, t, mem}	=> loadi("\tLDB\t", i, r, t, mem)
      | I.LOADI{li=I.LDH,  i, r, t,mem}	=> loadi("\tLDH\t", i, r, t, mem)
      | I.LOADI{li=I.LDW, i, r, t, mem}	=> loadi("\tLDW\t", i, r, t, mem)
      | I.ARITHI{ai=I.ADDI, i, r, t}	=> arithi("\tADDI\t", i, r, t)
      | I.ARITHI{ai=I.ADDIO, i, r, t}	=> arithi("\tADDIO\t", i, r, t)
      | I.ARITHI{ai=I.ADDIL, i, r, t}	=> arithi("\tADDIL\t", i, r, t)
      | I.ARITHI{ai=I.SUBI, i, r, t}	=> arithi("\tSUBI\t", i, r, t)
      | I.ARITHI{ai=I.SUBIO, i, r, t}	=> arithi("\tSUBIO\t", i, r, t)

      | I.COMCLR arg			=> cmp arg
 
      | I.SHIFTV{sv=I.VEXTRU, r, len, t}=> shiftv("\tVEXTRU\t",  r, len, t)
      | I.SHIFTV{sv=I.VEXTRS, r, len, t}=> shiftv("\tVEXTRS\t",  r, len, t)
      | I.SHIFTV{sv=I.ZVDEP, r, len, t}	=> shiftv("\tZVDEP\t", r, len, t)

      | I.SHIFT{s=I.EXTRU, r, p, len, t}=> shift("\tEXTRU\t", r, p, len, t)
      | I.SHIFT{s=I.EXTRS, r, p, len, t}=> shift("\tEXTRS\t", r, p, len, t)
      | I.SHIFT{s=I.ZDEP, r, p, len, t}	=> shift("\tZDEP\t", r, p, len, t)

      | I.BCOND arg			=> bcond arg
      | I.BCONDI arg			=> bcondi arg
      | I.B{lab,n}			=> (emit "\tB"; nullify n; 
                                            emit "\t"; eLabel lab;
					    emit "\n\tNOP" )
      | I.FBRANCH{cc,f1,f2,t,n,...} =>
	  (emit ("\tFCMP," ^ fcond cc ^"\t"); eFreg f1; comma(); eFreg f2;
           emit "\n\tFTEST\n";
           emit "\tB"; nullify n; emit "\t"; eLabel t; emit "\n\tNOP")
      | I.BLE{d, sr, b, t, ...} => 
          (emit "\tBLE\t"; eOperand d;
	   paren(fn () => (emit(ms sr); comma(); eReg b));
	   emit "\n\tCOPY %r31,"; eReg t)
      | I.BL{x=I.LabExp(LE.LABEL lab, fs), t, n, ...} => 
	  (emit "\tBL"; nullify n; 
           emit "\t"; emitFieldSel fs; eLabel lab; comma(); eReg t;
	   emit "\n\tNOP" )
      | I.BL _ => error "emitInstr:bl"
      | I.BV{x, b, n, ...} => 
	  (emit "\tBV"; nullify n; emit "\t"; eReg x; paren(fn () => eReg b);
           emit "\n\tNOP" )
      | I.BLR{x, t, n, labs, ...} => 
          (emit "\tBLR"; nullify n; emit "\t"; eReg x; comma(); eReg t;
           emit "\n\tNOP\n";
           app (fn l => (emit "\tB,n\t"; eLabel l; emit "\n\tNOP\n")) labs
          )
      | I.LDIL{i, t} => (emit "\tLDIL\t"; eOperand i; comma(); eReg t)
      | I.LDO{i,b,t} => 
	  (emit "\tLDO\t";  eOperand i; paren(fn () => eReg b); 
	   comma(); eReg t)
      | I.MTCTL{r,t} => (emit "\tMTCTL\t"; eReg r; emitCreg t)
      | I.FSTORE{fst, b, d, r, mem} => 
	  (case fst 
	    of I.FSTDS => fstore("\tFSTDS\t", b, d, r, mem)
	     | I.FSTWS => fstore("\tFSTWS\t", b, d, r, mem)
	  (*esac*))
      | I.FSTOREX{fstx, b, x, r, mem} => 
	  (case fstx
	    of I.FSTDX => fstorex("\tFSTDX\t", b, x, r, mem)
	     | I.FSTWX => fstorex("\tFSTWX\t", b, x, r, mem)
	  (*esac*))
      | I.FLOAD{fl=I.FLDDS, b, d, t, mem} => fload("\tFLDDS\t", b, d, t, mem)
      | I.FLOAD{fl=I.FLDWS, b, d, t, mem} => fload("\tFLDWS\t", b, d, t, mem)
      | I.FLOADX{flx=I.FLDDX, b, x, t, mem} => floadx("\tFLDDX\t", b, x, t, mem)
      | I.FLOADX{flx=I.FLDWX, b, x, t, mem} => floadx("\tFLDWX\t", b, x, t, mem)
      | I.FARITH arg => farith arg
      | I.FUNARY{fu=I.FCPY, f, t} => funary("\tFCPY\t", f, t)
      | I.FUNARY{fu=I.FABS, f, t} => funary("\tFABS\t", f, t)
      | I.FUNARY{fu=I.FCNVXF, f, t} => funary("\tFCNVXF\t", f, t)
      | I.BREAK(i, j) => emit ("\tBREAK(" ^ ms i ^ ", " ^ ms j ^ ")")
      | I.NOP => emit "\tNOP"
      | I.COPY{dst, src, tmp, ...} => 
	  app (fn I => (emit "\t"; emitInstr(I, regmap)))
	      (Shuffle.shuffle {regMap=rmap, temp=tmp, dst=dst, src=src})
      | I.FCOPY{dst, src, tmp, ...} =>
	  app (fn I => (emit "\t"; emitInstr(I, regmap)))
	      (Shuffle.shufflefp {regMap=rmap, temp=tmp, dst=dst, src=src})
      (*esac*);
    emit "\n"
  end
end


(*
 * $Log$
# Revision 1.1  99/02/17  21:16:20  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:57  pscheng
# *** empty log message ***
#
 * Revision 1.6  1998/10/06 14:04:27  george
 *   The instruction sequence FCMP, FTEST, FBCC is being replaced
 *   by the composite instruction FBRANCH.  This makes scheduling and
 *   other tasks easier.  Also, added BLR and BL in the instruction set.
 * 							[leunga]
 *
 * Revision 1.5  1998/09/30 19:35:22  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.4  1998/07/25 03:08:15  george
 *   added to support block names in MLRISC
 *
 * Revision 1.3  1998/05/25 15:10:53  george
 *   Fixed RCS keywords
 *
 *)
