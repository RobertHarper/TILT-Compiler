(* hppaMC.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)


functor HppaMCEmitter
  (structure Instr : HPPAINSTR
   structure Assembler : EMITTER_NEW where I = Instr
   structure CodeString : CODE_STRING) : EMITTER_NEW =
struct
  structure I = Instr
  structure P = Assembler.P

  val << = Word.<<
  val >> = Word.>>
  val ~>> = Word.~>>
  val ++ = Word.orb
  val & = Word.andb
  infix << >> ~>> ++ &

  fun error msg = MLRiscErrorMsg.impossible ("HppaMCEmitter." ^ msg)

  val itow = Word.fromInt

  val loc = ref 0

  fun emitByte n = let
    val i = !loc
    val wtob  = Word8.fromLargeWord o Word.toLargeWord
  in loc:= i+1; CodeString.update(i, wtob n)
  end

  fun emitbyte w8 = let
    val i = !loc
  in loc:= i+1; CodeString.update(i, w8)
  end

  fun emitWord w = (emitByte((w >> 0w8) & 0w255); emitByte(w & 0w255))

  fun defineLabel  lab = ()
  fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc, emit=emitbyte}
  fun comment msg = ()
  fun init n = (CodeString.init n; loc:=0)

  fun emitInstr(instr,regmap) = let
    val rMap = Intmap.map regmap 
    val rNum = itow o rMap
    val fNum = rNum

    fun low_sign_ext_im14 n = ((n & 0wx1fff) << 0w1) ++ ((n & 0wx2000) >> 0w13)
    fun low_sign_ext_im11 n = ((n &  0wx3ff) << 0w1) ++ ((n &  0wx400) >> 0w10)
    fun low_sign_ext_im5 n  = ((n &    0wxf) << 0w1) ++ ((n &   0wx10) >>  0w4)

    fun assemble_12 n = let
      val w = (n & 0wx800) >> 0w11
      val w1 = ((n & 0wx3ff) << 0w1) ++ ((n & 0wx400) >> 0w10)
    in
      (w1, w)
    end

    fun assemble_17 n = let
      val w = (n & 0wx10000) >> 0w16
      val w1 = (n & 0wxf800) >> 0w11
      val w2 =  (((n & 0wx3ff) << 0w1) ++ ((n & 0wx400) >> 0w10))
    in
      (w, w1, w2)
    end

    fun assemble_21 disp = let
      val w =
	(((disp & 0wx000003) << 0w12) ++
	 ((disp & 0wx00007c) << 0w14) ++
	 ((disp & 0wx000180) << 0w7) ++
	 ((disp & 0wx0ffe00) >> 0w8) ++
	 ((disp & 0wx100000) >> 0w20))
    in
      ((w >> 0w16) & 0wx1f, (w & 0wxffff))
    end
  
    fun operand opnd = let
      fun hi21 n = Word.>>(itow n, 0w11)
      fun hi21X n = Word.~>>(itow n, 0w11)
      fun lo11 n = Word.andb(itow n, 0wx7ff)

      (* BUG: should respect the field selectors instead of ignoring them *)
      fun f(I.HILabExp(lexp, _)) = hi21X(LabelExp.valueOf lexp)
	| f(I.LOLabExp(lexp, _)) = lo11(LabelExp.valueOf  lexp)
	| f(I.ConstOp _)     = error "ConstOp"
	| f(I.LabExp(lexp, _))   = itow(LabelExp.valueOf lexp)
	| f(I.IMMED i)       = itow i
    in
      f opnd
    end

    fun longImmediates(opcode, opnd, t) = let
      val (wh, wl) = assemble_21 (operand opnd)
    in
      emitWord((opcode << 0w10) ++ ((rNum t) << 0w5) ++ wh);
      emitWord wl
    end

    fun loadStore(opcode, b, t, im14) = 
      (emitWord((opcode << 0w10) ++ ((rNum b) << 0w5) ++ (rNum t));
       emitWord(low_sign_ext_im14 (operand im14)))

    (* indexed loads use sr3 which is guaranteed to contain sr5 --- see
     * HPPA.prim.asm.
     *)
    fun indexedLoads(opcode, b, x, ext4, t) = 
      (emitWord((opcode << 0w10) ++ ((rNum b) << 0w5) ++ (rNum x));
       emitWord((0w3 << 0w14) ++ (ext4 << 0w6) ++ (rNum t)))

    fun arithmeticLogical(opcode, r2, r1, ext7, b6, t) = 
      (emitWord((opcode << 0w10) ++ ((rNum r2) << 0w5) ++ (rNum r1));
       emitWord((ext7 << 0w6) ++ (b6 << 0w5) ++ (rNum t)))

    fun arithmeticImmediate(opcode, r, t, e, immed) = 
      (emitWord((opcode << 0w10) ++ ((rNum r) << 0w5) ++ (rNum t));
       emitWord((e << 0w11) ++ (low_sign_ext_im11 (operand immed))))

    fun conditionalBranch (opcode, bc, r1, r2, t, nullify) = let
      val wdisp = ((Label.addrOf t) - !loc - 8) div 4
      val c = (case bc of I.EQ  => 0w1 | I.LT  => 0w2 | I.LE  => 0w3
		        | I.LTU => 0w4 | I.LEU => 0w5
			|  _ => error "conditionalBranch"
	      (*esac*))
      val (w1, w) = assemble_12 (itow wdisp)
    in
      emitWord((opcode << 0w10) ++ (r2 << 0w5) ++ r1);
      emitWord((c << 0w13) ++ (w1 << 0w2) ++ nullify ++ w)
    end

    fun branchLink(opcode, r, lab, ext3, nullify) = let
      val disp = ((Label.addrOf lab) - !loc - 8) div 4
      val (w, w1, w2) = assemble_17 (itow disp)
    in
       emitWord((opcode << 0w10) ++ ((rNum r) << 0w5) ++ w1);
       emitWord((ext3 << 0w13) ++ (w2 << 0w2) ++ nullify ++ (w))
    end

    fun branchVectored(opcode, b, x, ext3, nullify) = 
      (emitWord((opcode << 0w10) ++ ((rNum b) << 0w5) ++ rNum x);
       emitWord((ext3 << 0w13) ++ nullify))

    fun branchBit(opcode, p, r1, c, lab) = let
      val wdisp = ((Label.addrOf lab) - !loc - 8) div 4
      val (w1, w) = assemble_12 (itow wdisp)
    in
      emitWord((opcode << 0w10) ++ (p << 0w5) ++ (rNum r1));
      emitWord((c << 0w13) ++ (w1 << 0w2) ++ w)
    end

    fun extractDeposit(opcode, r, t, ext3, p, clen) =
      (emitWord((opcode << 0w10) ++ ((rNum r) << 0w5) ++ rNum t);
       emitWord((ext3 << 0w10) ++ (itow p << 0w5) ++ itow clen))

    fun coProcShort(opcode, b, immed, b12, b10, b9, uid, r) =
      (emitWord((opcode << 0w10) ++ ((rNum b) << 0w5) ++ 
		      (low_sign_ext_im5 (itow immed)));
       emitWord((b12 << 0w12) ++ (b10 << 0w10) ++ (b9 << 0w9) ++ 
		(uid << 0w6) ++ (fNum r)))

    fun coProcIndexed(opcode, b, x, b12, b10, b9, uid, t) =
      (emitWord((opcode << 0w10) ++ ((rNum b) << 0w5) ++ (rNum x));
       emitWord((b12 << 0w12) ++ (b10 << 0w10) ++ (b9 << 0w9) ++ 
		(uid << 0w6) ++ (fNum t)))

    fun floatOpMaj0C(r1, r2, sub, fmt, b10, b6, t) = 
      (emitWord((0wx0c << 0w10) ++ (r1 << 0w5) ++ r2);
       emitWord((sub << 0w13) ++ (fmt << 0w11) ++ 
			(b10 << 0w9) ++ (b6 << 0w5) ++ t))

    fun floatOpMaj0E(r1, r2, sub, fmt, b10, b6, t) = 
      (emitWord((0wx0e << 0w10) ++ (r1 << 0w5) ++ r2);
       emitWord((sub << 0w13) ++ (fmt << 0w11) ++ 
			(b10 << 0w9) ++ (b6 << 0w5) ++ t))

    fun compare(opcode, r2, r1, (c, f), ext, t) = 
      (emitWord((opcode << 0w10) ++ ((rNum r2) << 0w5) ++ (rNum r1));
       emitWord((c << 0w13) ++ (f << 0w12) ++ (ext << 0w6) ++ (rNum t)))

    fun control(opcode, b, r, rv, ext8, t) = 
      (emitWord((opcode << 0w10) ++ (itow b << 0w5) ++ r);
       emitWord((rv << 0w13) ++ (ext8 << 0w5) ++ t))

    fun fcond I.?    = 0w2
      | fcond I.!<=> = 0w3
      | fcond I.==   = 0w4
      | fcond I.?=   = 0w6
      | fcond I.!<>  = 0w7
      | fcond I.!?>= = 0w8
      | fcond I.<    = 0w9
      | fcond I.?<   = 0w10
      | fcond I.!>=  = 0w11
      | fcond I.!?>  = 0w12
      | fcond I.<=   = 0w13
      | fcond I.?<=  = 0w14
      | fcond I.!>   = 0w15
      | fcond I.!?<= = 0w16
      | fcond I.>    = 0w17
      | fcond I.?>   = 0w18
      | fcond I.!<=  = 0w19
      | fcond I.!?<  = 0w20
      | fcond I.>=   = 0w21
      | fcond I.?>=  = 0w22
      | fcond I.!<   = 0w23
      | fcond I.!?=  = 0w24
      | fcond I.<>   = 0w25
      | fcond I.!=   = 0w26
      | fcond I.!?   = 0w28
      | fcond I.<=>  = 0w29

    fun cmpCond I.EQ  = (0w1, 0w0)
      | cmpCond I.LT  = (0w2, 0w0)
      | cmpCond I.LE  = (0w3, 0w0)
      | cmpCond I.LTU = (0w4, 0w0)
      | cmpCond I.LEU = (0w5, 0w0)
      | cmpCond I.NE  = (0w1, 0w1)
      | cmpCond I.GE  = (0w2, 0w1)
      | cmpCond I.GT  = (0w3, 0w1)
      | cmpCond I.GEU = (0w4, 0w1)
      | cmpCond I.GTU = (0w5, 0w1)

        (* nullify bit in instruction *)
    fun nullify true  = 0w2
      | nullify false = 0w0
  in
    case instr 
     of I.STORE{st, b, d, r, ...} =>
	 (case st 
	   of I.STW => loadStore(0wx1a, b, r, d)
	    | I.STH => loadStore(0wx19, b, r, d)
	    | I.STB => loadStore(0wx18, b, r, d)
	  (*esac*))
      | I.LOAD{l, r1, r2, t, ...} => 
	 (case l
	   of I.LDWX 	=> indexedLoads(0wx3, r1, r2, 0w2, t)
            | I.LDHX 	=> indexedLoads(0wx3, r1, r2, 0w1, t)
            | I.LDBX 	=> indexedLoads(0wx3, r1, r2, 0w0, t)
	 (*esac*))
      | I.LOADI{li, r, i, t, ...} => 
	 (case li
	   of I.LDW 	=> loadStore(0wx12, r, t, i)
	    | I.LDH 	=> loadStore(0wx11, r, t, i)
	    | I.LDB 	=> loadStore(0wx10, r, t, i)
	 (*esac*))
      | I.ARITH{a, r1, r2, t} => 
	 (case a
	   of I.ADD 	=> arithmeticLogical(0wx2, r2, r1, 0wx18, 0w0, t)
            | I.ADDO    => arithmeticLogical(0wx2, r2, r1, 0wx38, 0w0, t)
	    | I.SH1ADD  => arithmeticLogical(0wx2, r2, r1, 0wx19, 0w0, t)
	    | I.SH1ADDO => arithmeticLogical(0wx2, r2, r1, 0wx39, 0w0, t)
	    | I.SUB 	=> arithmeticLogical(0wx2, r2, r1, 0wx10, 0w0, t)
	    | I.SUBO 	=> arithmeticLogical(0wx2, r2, r1, 0wx30, 0w0, t)
	    | I.OR  	=> arithmeticLogical(0wx2, r2, r1, 0wx09, 0w0, t)
	    | I.XOR 	=> arithmeticLogical(0wx2, r2, r1, 0wx0a, 0w0, t)
	    | I.AND 	=> arithmeticLogical(0wx2, r2, r1, 0wx08, 0w0, t)
	 (*esac*))
      | I.ARITHI{ai, r, i, t} => 
	 (case ai
	   of I.ADDI 	=> arithmeticImmediate(0wx2d, r, t, 0w0, i)
            | I.ADDIO 	=> arithmeticImmediate(0wx2d, r, t, 0w1, i)
	    | I.ADDIL   => longImmediates(0wxa, i, r)
	    | I.SUBI 	=> arithmeticImmediate(0wx25, r, t, 0w0, i)
	    | I.SUBIO 	=> arithmeticImmediate(0wx25, r, t, 0w1, i)
	 (*esac*))
      | I.COMCLR{cc, r1, r2, t} => compare(0wx2, r2, r1, cmpCond cc, 0wx22, t)
      | I.SHIFTV{sv, r, len, t} => 
	 (case sv 
	   of I.VEXTRU => extractDeposit(0wx34, r, t, 0w4, 0, 32-len)
            | I.VEXTRS => extractDeposit(0wx34, r, t, 0w5, 0, 32-len)
            | I.ZVDEP =>  extractDeposit(0wx35, t, r, 0w0, 0, 32-len)
	 (*esac*))
      | I.SHIFT{s, r, p, len, t} => 
	 (case s 
	   of I.EXTRU => extractDeposit(0wx34, r, t, 0w6, p, 32-len)
            | I.EXTRS => extractDeposit(0wx34, r, t, 0w7, p, 32-len)
            | I.ZDEP =>  extractDeposit(0wx35, t, r, 0w2, 31-p, 32-len)
	  (*esac*))
      | I.BCOND{cmp, bc, r1, r2, t, n, ...}  => let
	  val opcode = case cmp of I.COMBT => 0wx20 | I.COMBF => 0wx22
	in
	  conditionalBranch(opcode, bc, rNum r1, rNum r2, t, nullify n)
	end
      | I.BCONDI{bc, i, r2, t, cmpi, n, ...} => let
	  val opcode = case cmpi of I.COMIBT => 0wx21 | I.COMIBF => 0wx23
	  val r1 = low_sign_ext_im5(itow(i))
        in
	  conditionalBranch(opcode, bc, r1, rNum r2, t, nullify n)
	end
      | I.LDIL{i, t}      => longImmediates(0wx8, i, t)
      | I.LDO{i, b, t}    => loadStore(0wx0d, b, t, i)
      | I.BL _		  => error "emitInstr:bl"
      | I.BLE{d=I.IMMED 0, sr=5, b, t=31, ...} => 
	  (emitWord((0wx39 << 0w10) ++ (rNum b << 0w5));
	   emitWord((0w3 << 0w13) ++ 0w2))
      | I.BLE _		  => error "BLE: not implemented"
      | I.B{lab, n, ...}  => branchLink(0wx3a, 0, lab, 0w0, nullify n)
      | I.BV{b, x, n,...} => branchVectored(0wx3a, b, x, 0w6, nullify n)
      | I.BLR{x, t, n,...} => branchVectored(0wx3a, t, x, 0w2, nullify n)
      | I.MTCTL{r, t}     => control(0w0, t, rNum r, 0w0, 0wxc2, 0w0)
      | I.FSTORE{fst, b, d, r, ...} =>
	 (case fst
	   of I.FSTDS => coProcShort(0wxb, b, d, 0w1, 0w0, 0w1, 0w0, r)
	    | I.FSTWS => coProcShort(0wx9, b, d, 0w1, 0w0, 0w1, 0w1, r)
         (*esac*))
      | I.FSTOREX{fstx, b, x, r, ...} =>
	 (case fstx 
	   of I.FSTDX => coProcIndexed(0wxb, b, x, 0w0, 0w0, 0w1, 0w0, r)
	    | I.FSTWX => coProcIndexed(0wx9, b, x, 0w0, 0w0, 0w1, 0w1, r)
	 (*esac*))
      | I.FLOAD{fl, b, d, t, ...} => 
	 (case fl
	   of I.FLDDS => coProcShort(0wxb, b, d, 0w1, 0w0, 0w0, 0w0, t)
  	    | I.FLDWS => coProcShort(0wx9, b, d, 0w1, 0w0, 0w0, 0w1, t)
         (*esac*))
      | I.FLOADX{flx, b, x, t, ...} => 
	 (case flx
	   of I.FLDDX => coProcIndexed(0wxb, b, x, 0w0, 0w0, 0w0, 0w0, t)
	    | I.FLDWX => coProcIndexed(0wx9, b, x, 0w0, 0w0, 0w0, 0w1, t)
	 (*esac*))
      | I.FARITH{fa, r1, r2, t} =>
	(case fa
	  of I.FADD => floatOpMaj0C(fNum r1, fNum r2, 0w0, 0w1, 0w3, 0w0, fNum t)
	   | I.FSUB => floatOpMaj0C(fNum r1, fNum r2, 0w1, 0w1, 0w3, 0w0, fNum t)
	   | I.FMPY => floatOpMaj0C(fNum r1, fNum r2, 0w2, 0w1, 0w3, 0w0, fNum t)
	   | I.FDIV => floatOpMaj0C(fNum r1, fNum r2, 0w3, 0w1, 0w3, 0w0, fNum t)
	   | I.XMPYU=> 
	       (emitWord((0wxe << 0w10) ++ (fNum r1 << 0w5) ++ (fNum r2));
		emitWord((0w5 << 0w12) ++ (0wxf << 0w7) ++ (fNum t)))

	(*esac*))
      | I.FUNARY{fu, f, t} =>
	 (case fu
	   of I.FCPY   => floatOpMaj0C(fNum f, 0w0, 0w2, 0w1, 0w0, 0w0, fNum t)
	    | I.FABS   => floatOpMaj0C(fNum f, 0w0, 0w3, 0w1, 0w0, 0w0, fNum t)
	    | I.FCNVXF => floatOpMaj0E(fNum f, 0w0, 0w5, 0w0, 0w1, 0w4, fNum t)
	 (*esac*))
      | I.FBRANCH{cc,f1,f2,n,t,f,long} =>
        ( (* FCMP *)
	 floatOpMaj0C(fNum f1, fNum f2, 0w0, 0w1, 0w2, 0w0, fcond cc);
           (* FTEST *)
	 emitWord(0wxc << 0w10); 
	 emitWord((0w1 << 0w13) ++ (0w2 << 0w9) ++ (0w1 << 0w5));
           (* B,n t *)
         branchLink(0wx3a, 0, t, 0w0, nullify n)
        )
      | I.BREAK(i, j) => 
	 (emitWord((itow i & 0wx1fff) >> 0w3); 
	  emitWord(((itow i & 0wx7) << 0w13) & (itow j & 0wx1f)))
      | I.NOP => emitInstr(I.ARITH{a=I.OR, r1=0, r2=0, t=0}, regmap)
      | I.COPY _ => error "emitInstr:COPY"
      | I.FCOPY _ => error "emitInstr:FCOPY"
  end (*emitInstr*)
end

(*
 *)

