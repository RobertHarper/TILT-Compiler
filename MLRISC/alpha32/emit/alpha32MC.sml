(* alpha32MC.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor Alpha32MCEmitter
  (structure Instr : ALPHA32INSTR
   structure PseudoOps : PSEUDO_OPS
   structure CodeString : CODE_STRING) : EMITTER_NEW =
struct
    structure I = Instr
    structure P = PseudoOps 
    structure C = I.C

    structure LE = LabelExp
 
    val << = Word.<<
    val >> = Word.>>
    val ~>> = Word.~>>
    val ++ = Word.orb
    val & = Word.andb
    infix << >> ~>> ++ &

    val itow  = Word.fromInt

    fun error msg = MLRiscErrorMsg.impossible ("Alpha32MCEmitter." ^ msg)

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

    (* Alpha32 is low endian *)
    fun emitWord n = (emitByte(n & 0w255); emitByte((n >> 0w8) & 0w255))
    fun defineLabel  lab = ()
    fun emitstring s = Word8Vector.app emitbyte (Byte.stringToBytes s)
    fun comment msg = ()
    fun init n = (CodeString.init n;  loc:=0)
    fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc, emit=emitbyte}

    open Label
    fun emitInstr(instr,regmap) = let
      datatype register = REG | FREG

      val rMap = Intmap.map regmap
      val rNum = itow o rMap
      val fNum = rNum

      local 
	fun split i = let
	  val w = Word.fromInt i
	  val hi = Word.~>>(w, 0w16)
	  val lo = Word.andb(w, 0w65535)
	in if lo <  0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
	end
      in
	fun high n = #1 (split n)
	fun low n  = #2 (split n)
      end

      fun immed16(I.IMMop i) =
	  (if i < ~32768 orelse i > 32767 then
	     error ("immed16 - " ^ Int.toString i) else ();
	  (itow i & 0w65535))
	| immed16(I.LOLABop labexp) = low (LE.valueOf labexp)
	| immed16(I.HILABop labexp) = high (LE.valueOf labexp)
	| immed16 _ = error "immed16"

      fun immed8(I.IMMop i) =
	(if i < 0 orelse i > 255 then
	   error ("immed8 - " ^ Int.toString i) else ();
	   (itow i & 0w255))                     
	| immed8 _ = error "immed8"
	
      fun immed21 i =
	(if i < ~1048576 orelse i > 1048575 then
	   error ("immed21 - " ^ Int.toString i) else ();
	   itow i & 0w2097151)                      

	  
      fun Branch typ (opcode, ra', lab) = let
	  val ra = (case typ of REG => rNum ra' | FREG => fNum ra')
	  val disp = (((Label.addrOf lab) - !loc - 4) div 4)
	  val testdisp = immed21 disp
	  val lowdisp = testdisp & 0w65535
	in
	  emitWord lowdisp;
	  emitWord((opcode << 0w10) ++ (ra << 0w5) ++ (testdisp >> 0w16))
	end

      val branch = Branch REG
      val fbranch = Branch FREG

      fun Memory typ (opcode, ra', rb, disp) = let
	  val ra = (case typ of REG => rNum ra' | FREG => fNum ra')
	in
	  emitWord (disp);
	  emitWord((itow opcode << 0w10) ++ (ra << 0w5) ++ rNum rb)
	end

      val memory = Memory REG
      val fmemory = Memory FREG

      fun MemoryW typ (opcode, ra', rb, disp) = let
	  val ra = (case typ of REG => rNum ra' | FREG => fNum ra')
	in
	  emitWord disp;
	  emitWord((itow opcode << 0w10) ++ (ra << 0w5) ++ rNum rb)
	end

      val memoryW = MemoryW REG
	
      val fmemoryW = MemoryW FREG

      fun Jump (opcode, bits, {r=ra, b=rb, d=disp}) =
	let
        (* disp is just a hint to the alpha - it does not affect destination *)
	  val _ = if disp <> 0 then error "Jump" else ()
	  val d = (itow disp & 0w16383) ++ (itow bits << 0w14)
	in
	  memory (opcode, ra, rb, itow disp)
	end
      
      fun Operate (opcode, func, ra, I.REGop rb, rc) =
 	   (emitWord((func << 0w5) ++ rNum rc);
	    emitWord((opcode << 0w10) ++ (rNum ra << 0w5) ++ rNum rb))
	| Operate (opcode, func, ra, opnd, rc) =
	  let
	    val testi = immed8 opnd
	  in
	    emitWord((testi << 0w13) ++ 0w4096 ++ (func << 0w5) ++ rNum rc);
	    emitWord((opcode << 0w10) ++ (rNum ra << 0w5) ++ (testi >> 0w3))
	  end
	
      fun FOperate (opcode, func, fa, fb, fc) =
	 (emitWord((func << 0w5) ++ fNum fc);
	  emitWord((opcode << 0w10) ++ (fNum fa << 0w5) ++ fNum fb))

      fun Pall(opcode) = (emitWord(opcode & 0wxffff); emitWord(opcode >> 0w16))

      val zeroR = 31
    in
	case instr of
	  I.DEFFREG _           => error "DEFFREG"
	| I.LDA{r, b, d} => memoryW(8, r, b, immed16 d)
	| I.LDAH{r, b, d} => memoryW(9, r, b, immed16 d)
	| I.LOAD{ldOp, r, b, d, ...} => 
	   (case ldOp
	    of I.LDL => memory(40, r, b, immed16 d)
	     | I.LDQ => memory(41, r, b, immed16 d)
	     | I.LDQ_U => memory(11, r, b, immed16 d)
            (* esac *))
	| I.STORE{stOp, r, b, d, ...} => 
	   (case stOp
	    of I.STL => memory(44, r, b,  immed16 d)
	     | I.STQ => memory(45, r, b,  immed16 d)
	     | I.STQ_U => memory(15, r, b, immed16 d)
	   (* esac *))
	 
	| I.FLOAD{ldOp, r, b, d, ...} => 
	   (case ldOp
	    of I.LDT => fmemory(35, r, b, immed16 d)
	     | I.LDS => fmemory(34, r, b, immed16 d)
	    (*esac*))

	| I.FSTORE{stOp=I.STT, r, b, d, ...} => fmemory(39, r, b, immed16 d)
	
	| I.JMPL (arg, _) 	=> Jump(26, 0, arg)
	| I.JSR(arg,_,_)	=> Jump(26, 1, arg)
	| I.BRANCH(brOp, reg, lab) => 
	    (case brOp
	      of I.BR 	=> branch(0w48, reg, lab)
  	       | I.BEQ 	=> branch(0w57, reg, lab)
	       | I.BGE 	=> branch(0w62, reg, lab)
	       | I.BGT 	=> branch(0w63, reg, lab)
	       | I.BLE 	=> branch(0w59, reg, lab)
	       | I.BLT 	=> branch(0w58, reg, lab)
	       | I.BNE 	=> branch(0w61, reg, lab)
	       | I.BLBS	=> branch(0w60, reg, lab)
	       | I.BLBC	=> branch(0w56, reg, lab)
	    (*esac*))

	| I.FBRANCH(fbrOp, freg, lab) => 
	    (case fbrOp
	      of I.BEQ	=> fbranch(0w49, freg, lab)
	       | I.BGE	=> fbranch(0w54, freg, lab)
	       | I.BGT	=> fbranch(0w55, freg, lab)
	       | I.BLE	=> fbranch(0w51, freg, lab)
	       | I.BLT	=> fbranch(0w50, freg, lab)
	       | I.BNE	=> fbranch(0w53, freg, lab)
	       | _ => error "FBRANCH"
	    (*esac*))

	| I.OPERATE{oper, ra, rb, rc} => 
	    (case oper
	     of I.ZAP	 => Operate(0w18, 0w48, ra, rb, rc)
	      | I.ADDL	 => Operate(0w16, 0w0, ra, rb, rc)
	      | I.ADDQ 	 => Operate(0w16, 0w32, ra, rb, rc)
	      | I.SUBL 	 => Operate(0w16, 0w9, ra, rb, rc)
	      | I.SUBQ 	 => Operate(0w16, 0w41, ra, rb, rc)
	      | I.MULL 	 => Operate(0w19, 0w0, ra, rb, rc)
	      | I.S4ADDL => Operate(0w16, 0w2, ra, rb, rc)
	      | I.S8ADDL => Operate(0w16, 0w18, ra, rb, rc)
	      | I.CMPULE => Operate(0w16, 0w61, ra, rb, rc)
	      | I.CMPULT => Operate(0w16, 0w29, ra, rb, rc)
	      | I.CMPEQ  => Operate(0w16, 0w45, ra, rb, rc)
	      | I.CMPLE  => Operate(0w16, 0w109, ra, rb, rc)
	      | I.CMPLT  => Operate(0w16, 0w77, ra, rb, rc)
	      | I.SGNXL  => Operate(0w16, 0w0, ra, rb, rc) (* ADDL *)
	      | I.AND 	 => Operate(0w17, 0w0, ra, rb, rc)
	      | I.BIS 	 => Operate(0w17, 0w32, ra, rb, rc)
	      | I.XOR 	 => Operate(0w17, 0w64, ra, rb, rc)
	      | I.SRA 	 => Operate(0w18, 0w60, ra, rb, rc)
	      | I.SRL 	 => Operate(0w18, 0w52, ra, rb, rc)
	      | I.SLL 	 => Operate(0w18, 0w57, ra, rb, rc)
	      | I.INSBL  => Operate(0w18, 0w11, ra, rb, rc)
	      | I.EXTBL  => Operate(0w18, 0w6, ra, rb, rc)
	      | I.EXTQH  => Operate(0w18, 0w122, ra, rb, rc)
	      | I.MSKBL  => Operate(0w18, 0w2, ra, rb, rc)
	      | I.MSKLH  => Operate(0w18, 0w98, ra, rb, rc)
	    (*esac*))
	| I.PSEUDOARITH _ => error "emitInstr:PSEUDOOP"
	| I.OPERATEV{oper, ra, rb, rc} => 
	  (case oper
	   of I.ADDLV	=> Operate(0w16, 0w64, ra, rb, rc)
	    | I.SUBLV	=> Operate(0w16, 0w73, ra, rb, rc)
	    | I.MULLV	=> Operate(0w19, 0w64, ra, rb, rc)
	  (*esac*))
	| I.FOPERATE{oper, fa, fb, fc} => 
	  (case oper
	   of I.CPYS	=> FOperate(0w23, 0w32, fa, fb, fc)
	    | I.CPYSN	=> FOperate(0w23, 0w33, fa, fb, fc)
	    | I.CMPTUN	=> FOperate(0w22, 0wx5a4, fa, fb, fc)
	    | I.CMPTEQ	=> FOperate(0w22, 0wx5a5, fa, fb, fc)
	    | I.CMPTLT	=> FOperate(0w22, 0wx5a6, fa, fb, fc)
	    | I.CMPTLE	=> FOperate(0w22, 0wx5a7, fa, fb, fc)
	    | I.CVTQT 	=> FOperate(0w22, 0wx0be, fa, fb, fc)
	    | I.CVTLQ   => FOperate(0w23, 0w16, fa, fb, fc)
	   (*esac*))
	| I.FOPERATEV{oper, fa, fb, fc} => 
	  (case oper
	   of I.ADDT	=> FOperate(0w22, 0wx5e0, fa, fb, fc)
	    | I.SUBT 	=> FOperate(0w22, 0wx5e1, fa, fb, fc)
	    | I.MULT 	=> FOperate(0w22, 0wx5e2, fa, fb, fc)
	    | I.DIVT	=> FOperate(0w22, 0wx5e3, fa, fb, fc)  (* chopped *)
	    | I.CVTTQ	=> FOperate(0w22, 0wx02f, fa, fb, fc) (* towards zero*)
	   (*esac*))
	| I.TRAPB   => memoryW(24, zeroR, zeroR, 0w0)
	| I.CALL_PAL{code, ...} => 
	  (case code
	   of I.BPT => Pall(0wx80)
  	    | I.BUGCHK => Pall(0wx81)
	    | I.CALLSYS => Pall(0wx83)
	    | I.GENTRAP => Pall(0wxaa)
	    | I.IMB => Pall(0wx86)
	    | I.RDUNIQUE => Pall(0wx9e)
	    | I.WRUNIQUE => Pall(0wx9f)
           (*esac*))
	| I.COPY _  => error "emitInstr:COPY"
	| I.FCOPY _ => error "emitInstr:FCOPY"
    end
  end			      






  


(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:50  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:15:10  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:42  pscheng
# *** empty log message ***
#
 * Revision 1.4  1998/12/30 20:19:06  jhr
 *   Modifications to support direct generation of code into code objects.
 *
 * Revision 1.3  1998/10/06 14:07:30  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.2  1998/09/30 19:34:21  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
