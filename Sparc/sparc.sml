(*$import Prelude TopLevel TilWord32 Int Name Word32 Core SPARC String Rtl Util Char Listops Stats List *)

structure  Sparc :> SPARC =
struct

    val exclude_intregs = []
    val error = fn s => Util.error "sparc.sml" s
    (* Check against Runtime/thread.h *)
    val iregs_disp          = 0
    val fregs_disp          = iregs_disp + 4 * 32
    val threadScratch_disp  = fregs_disp + 8 * 32 + 4 + 4
    val request_disp        = threadScratch_disp + 8
    val requestInfo_disp    = request_disp + 4
    val writelistAlloc_disp = requestInfo_disp + 4 + 4 * 32 + 8 * 32
    val writelistLimit_disp = writelistAlloc_disp + 4
    val stackLimit_disp     = writelistLimit_disp + 4
    val globalOffset_disp   = stackLimit_disp + 4
    val stackletOffset_disp = globalOffset_disp + 4
    val arrayOffset_disp    = stackletOffset_disp + 4

    val heapLimit_disp      = iregs_disp + 4 * 5

structure Machine = 
  struct
    open Rtl
    open Core


    val Rpv     = NONE
    val Rcc     = R (~1) (* useful for defining interferences WRT status register *)
    val RY      = R (~2) (* useful for defining interferences WRT Y register *)
    val Rzero   = R 0   (* Standard Sparc convention *)
    val Rsp     = R 14  (* Standard Sparc convention *)
    val Rat     = R 16  (* Standard Sparc convention *)
    val Rframe  = R 30   (* Standard Sparc convention - we reserve but do not keep this up to date
			    Messing up the frame pointer can cause extremely weird behavior.
			    For example, the VM system may be triggered at any point and it will 
			    freak out if this register contains a weird value. To demonstrate,
			    the following program will act weird when compiled with cc or gcc 
			    unless the static option is used:

			    .text
			    .global main
			  loop:
			    retl
			    !	ba	loop		! PC is here when illegal instruction is delivered
			    nop			! delay slot
			  main:
			    save	%sp,-96,%sp
			    mov	1, %fp		! this is the cause of the illegal instruction; change to 0 for seg fault
			    call	loop
			    nop			! delay slot
			    ret
			    restore
			  *)
    val Rra     = R 15  (* Standard Sparc convention *)
    val Rexnarg = R 15  (* Rexnarg and Rra are never simultaneously live
			      since exnarg is active only when about to raise
			      an exception which means we are not normally returning *)

    val Rhandler = R 17 (* This register contains the exception handler's addr as we jump to it *)
    val Rat2    = R 17  (* Put our second temporary in a volatile *)
    val Rexnptr = R 1   (* non-volatile *)
    val Rhlimit = R 5   (* non-volatile *)
    val Rheap   = R 4   (* non-volatile *)
    val Rth     = R 2   (* non-volatile *)


    val Fat     = F 62  (* volatile *)
    val Fat2    = F 60   (* volatile *)


    fun isPhysical (R i) = i<32
      | isPhysical (F i) = i<64

  datatype storei_instruction = ST | STUB | STD
  datatype storef_instruction = STF | STDF
  datatype loadi_instruction  = LD | LDUB | LDD
  datatype loadf_instruction  = LDF | LDDF
  (* BCC = branch on carry-clear = BGEU;  BCS = branch on carry-set = BLU *)
  datatype cbri_instruction   = BE | BNE | BG | BGE | BL | BLE | BGU | BLEU | BCC | BCS | BVC | BVS
  datatype cbrr_instruction   = BRZ | BRLEZ | BRLZ | BRNZ | BRGZ | BRGEZ
  datatype cbrf_instruction   = FBE | FBNE | FBG | FBGE | FBL | FBLE 
  datatype trap_instruction   = TVS | TNE
  datatype int_instruction    =
    ADD | ADDCC | SUB | SUBCC
  | SMUL | SMULCC | UMUL | UMULCC
  | SDIV | SDIVCC | UDIV | UDIVCC 
  | AND | OR | XOR | ANDNOT | ORNOT | XORNOT
  | SRA | SRL | SLL

  datatype fp_instruction    = 
    FADDD | FSUBD | FMULD | FDIVD

  datatype fpmove_instruction = 
    FABSD | FNEGD | FMOVD | FITOD | FDTOI

  datatype imm = INT of int             (* Must fit in 13 bits sign-extended *)
               | LOWINT of Word32.word  (* The low 10 bits of the word *)
               | HIGHINT of Word32.word (* The high 22 bits of the word *)
               | LOWLABEL of label * Word32.word  (* The low 10 bits of the label plus offset *)
               | HIGHLABEL of label * Word32.word (* The high 22 bits of the label plus offset *)

  datatype operand = 
    REGop of register
  | IMMop of imm

  datatype software_trap_number = ST_INT_OVERFLOW
      
  datatype specific_instruction =
    NOP  (* stylized for easier reading *)
  (* For sethi, the imm must be of the HIGH flavor *)
  | SETHI  of imm * register
  | WRY    of register
  | RDY    of register
  | CMP    of register * operand
  | FCMPD  of register * register
  (* For the load/store instructions, imm must be of the LOW flavors *)
  | STOREI of storei_instruction * register * imm * register
  | LOADI  of loadi_instruction * register * imm * register
  | STOREF of storef_instruction * register * imm * register
  | LOADF  of loadf_instruction * register * imm * register
  | CBRANCHI of cbri_instruction * label * bool (* predict taken? *)
  | CBRANCHR of cbrr_instruction * register * label * bool (* predict taken? *)
  | CBRANCHF of cbrf_instruction * label
  | INTOP  of int_instruction * register * operand * register
  | FPOP   of fp_instruction * register * register * register
  | FPMOVE  of fpmove_instruction * register * register
  | TRAP of trap_instruction * software_trap_number

    datatype instruction = 
	BASE     of base_instruction
      | SPECIFIC of specific_instruction

    val maxImm = 4095
    val maxImm8 = maxImm - (maxImm mod 8)

    fun in_imm_range i = (i >= ~4096) andalso (i <= maxImm)
    fun in_ea_disp_range i = (i >= ~4096) andalso (i <= 4095)

    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
    fun ms n = if n<0 then ("-"^(Int.toString (~n))) else Int.toString n
    val msw = TilWord32.toDecimalString

    fun msReg (R 14) = "%sp"
      | msReg (R 30) = "%fp"
      | msReg (R n) = "%r" ^ (ms n)
      | msReg (F n) = "%f" ^ (ms n)
	
    val makeAsmLabel =
	let 
	    fun loop [] = ""
	      | loop (#"'" :: rest) = "PRIME" ^ (loop rest)
	      | loop (#"!" :: rest) = "BANG" ^ (loop rest)
	      | loop (#"%" :: rest) = "PERCENT" ^ (loop rest)
	      | loop (#"&" :: rest) = "AND" ^ (loop rest)
	      | loop (#"$" :: rest) = "DOLLAR" ^ (loop rest)
	      | loop (#"#" :: rest) = "HASH" ^ (loop rest)
	      | loop (#"+" :: rest) = "PLUS" ^ (loop rest)
	      | loop (#"-" :: rest) = "MINUS" ^ (loop rest)
	      | loop (#"/" :: rest) = "SLASH" ^ (loop rest)
	      | loop (#":" :: rest) = "COLON" ^ (loop rest)
	      | loop (#"<" :: rest) = "LT" ^ (loop rest)
	      | loop (#"=" :: rest) = "EQ" ^ (loop rest)
	      | loop (#">" :: rest) = "GT" ^ (loop rest)
	      | loop (#"?" :: rest) = "QUEST" ^ (loop rest)
	      | loop (#"@" :: rest) = "AT" ^ (loop rest)
	      | loop (#"\\" :: rest) = "BACKSLASH" ^ (loop rest)
	      | loop (#"~" :: rest) = "TILDE" ^ (loop rest)
	      | loop (#"`" :: rest) = "ANTIQUOTE" ^ (loop rest)
	      | loop (#"^" :: rest) = "HAT" ^ (loop rest)
	      | loop (#"|" :: rest) = "BAR" ^ (loop rest)
	      | loop (#"*" :: rest) = "STAR" ^ (loop rest)
        | loop (s :: rest) = (String.str s) ^ (loop rest)
	in
	    loop o explode
	end
    

  fun msLabel (LOCAL_CODE s) = makeAsmLabel s
    | msLabel (LOCAL_DATA s) = makeAsmLabel s
    | msLabel (ML_EXTERN_LABEL label) = (makeAsmLabel label)


  fun loadi_to_ascii LD  = "ld"
    | loadi_to_ascii LDUB = "ldub"
    | loadi_to_ascii LDD = "ldd"

  fun loadf_to_ascii LDF  = "ld"
    | loadf_to_ascii LDDF  = "ldd"

  fun storei_to_ascii ST  = "st"
    | storei_to_ascii STUB = "stub"
    | storei_to_ascii STD = "std"

  fun storef_to_ascii STF = "st"
    | storef_to_ascii STDF = "std"

  fun cbri_to_ascii BE   = "be"
    | cbri_to_ascii BNE  = "bne"
    | cbri_to_ascii BG   = "bg"
    | cbri_to_ascii BGE  = "bge"
    | cbri_to_ascii BL   = "bl"
    | cbri_to_ascii BLE  = "ble"
    | cbri_to_ascii BGU  = "bgu"
    | cbri_to_ascii BLEU = "bleu"
    | cbri_to_ascii BCC  = "bcc"
    | cbri_to_ascii BCS  = "bcs"
    | cbri_to_ascii BVS  = "bvs"
    | cbri_to_ascii BVC  = "bvc"

  fun cbrr_to_ascii BRZ   = "brz"
    | cbrr_to_ascii BRLEZ = "brlez"
    | cbrr_to_ascii BRLZ  = "brlz"
    | cbrr_to_ascii BRNZ  = "brnz"
    | cbrr_to_ascii BRGZ  = "brgz"
    | cbrr_to_ascii BRGEZ = "brgez"
      
  fun trap_to_ascii TVS  = "tvs"
    | trap_to_ascii TNE  = "tne"

  (* constants from <sys/trap.h> *)
  fun trap_code_to_ascii ST_INT_OVERFLOW = "0x07"

  fun cbrf_to_ascii FBE  = "fbe"
    | cbrf_to_ascii FBNE = "fbne"
    | cbrf_to_ascii FBG  = "fbg"
    | cbrf_to_ascii FBGE = "fbge"
    | cbrf_to_ascii FBL  = "fbl"
    | cbrf_to_ascii FBLE = "fble"

  fun int_to_ascii  ADD    = "add"
    | int_to_ascii  ADDCC  = "addcc"
    | int_to_ascii  SUB    = "sub"
    | int_to_ascii  SUBCC  = "subcc"
    | int_to_ascii  SMUL   = "smul"
    | int_to_ascii  SMULCC = "smulcc"
    | int_to_ascii  UMUL   = "umul"
    | int_to_ascii  UMULCC = "umulcc"
    | int_to_ascii  SDIV   = "sdiv"
    | int_to_ascii  SDIVCC = "sdivcc"
    | int_to_ascii  UDIV   = "udiv"
    | int_to_ascii  UDIVCC = "udivcc"
    | int_to_ascii  AND    = "and"  
    | int_to_ascii  OR     = "or"  
    | int_to_ascii  XOR    = "xor"  
    | int_to_ascii  ANDNOT = "andn"
    | int_to_ascii  ORNOT  = "orn"
    | int_to_ascii  XORNOT = "xnor"
    | int_to_ascii  SRA    = "sra"
    | int_to_ascii  SRL    = "srl"
    | int_to_ascii  SLL    = "sll"


  fun fp_to_ascii FADDD    = "faddd"
    | fp_to_ascii FSUBD    = "fsubd"
    | fp_to_ascii FMULD    = "fmuld"
    | fp_to_ascii FDIVD    = "fdivd"

  fun fpmove_to_ascii FABSD = "fabsd"
    | fpmove_to_ascii FNEGD = "fnegd"
    | fpmove_to_ascii FMOVD = "fmovd"
    | fpmove_to_ascii FITOD = "fitod"
    | fpmove_to_ascii FDTOI = "fdtoi"


  fun reglist_to_ascii [] = ""
    | reglist_to_ascii [r] = (msReg r)
    | reglist_to_ascii (r::rs) = (msReg r) ^ "," ^ (reglist_to_ascii rs)

  fun rtl_to_ascii (CALL {func=DIRECT (func,_), calltype = Rtl.ML_TAIL _,
			  args, results,...}) =
           "(tail)CALL " ^ (msLabel func) ^ " (" ^
	   (reglist_to_ascii args) ^ " ; " ^ (reglist_to_ascii results)
           ^ ")"
    | rtl_to_ascii (CALL {func=INDIRECT Raddr, calltype = Rtl.ML_TAIL _, ...}) = 
           "(tail)CALL via " ^ msReg Raddr
    | rtl_to_ascii (CALL {func=INDIRECT Raddr, ...}) = 
           "CALL via " ^ msReg Raddr
    | rtl_to_ascii (CALL {func=DIRECT (func,_), args,results,...}) = 
           "CALL " ^ (msLabel func) ^ " (" ^
	   (reglist_to_ascii args) ^ " ; " ^ (reglist_to_ascii results)
           ^ ")"
    | rtl_to_ascii (Core.JMP (Raddr,_)) = "JMP " ^ (msReg Raddr)
    | rtl_to_ascii (RETURN{results}) = "RETURN [" ^ (reglist_to_ascii results) ^ "]"
    | rtl_to_ascii HANDLER_ENTRY = "HANDLER_ENTRY"
    | rtl_to_ascii (SAVE_CS _) = "SAVE_CS"


  val comma  	    	       = ", "
  val tab                      = "\t"
  val newline                  = "\n"

  fun msImm (INT i) = ms i
    | msImm (LOWINT w) = "%lo(" ^ (msw w) ^ ")"
    | msImm (HIGHINT w) = "%hi(" ^ (msw w) ^ ")"
    | msImm (LOWLABEL (l,w))  = "%lo(" ^ (msLabel l) ^ (if w = 0w0 then "" else "+" ^ (msw w))  ^ ")"
    | msImm (HIGHLABEL (l,w)) = "%hi(" ^ (msLabel l) ^ (if w = 0w0 then "" else "+" ^ (msw w))  ^ ")"
  fun msOperand (REGop r) = msReg r
    | msOperand (IMMop imm) = msImm imm
  fun msDisp(rd, INT 0)     = "[" ^ (msReg rd) ^ "]"
    | msDisp(rd, imm) = (case imm of
			     HIGHINT _ => error "msDisp with HIGHINT"
			   | HIGHLABEL _ => error "msDisp with HIGHLABEL"
			   | _ =>  "[" ^ (msReg rd) ^ "+" ^ (msImm imm) ^ "]")

  fun msInstrSpecific instrSpecific = 
      (case instrSpecific of
         NOP => "nop"
       | (SETHI (imm, Rdest)) => ("sethi" ^ tab ^
				  (msImm imm) ^ comma ^ (msReg Rdest))
       | (WRY Rsrc) => ("mov" ^ tab ^ (msReg Rsrc) ^ ",%y")
       | (RDY Rdest) => ("mov" ^ tab ^ "%y," ^  (msReg Rdest))
       | (CMP (Rsrc1, op2)) => ("cmp" ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msOperand op2))
       | (FCMPD (Rsrc1, Rsrc2)) =>
                                ("fcmpd" ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msReg Rsrc2))
       | (STOREI (instr, Rsrc, disp, Raddr)) =>
                                ((storei_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msDisp(Raddr, disp)))
       | (STOREF (instr, Rsrc, disp, Raddr)) =>
                                ((storef_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msDisp(Raddr, disp)))
       | (LOADI (instr, Rdest, disp, Raddr)) =>
                                ((loadi_to_ascii instr) ^ tab ^
				 (msDisp(Raddr, disp)) ^ comma ^ (msReg Rdest))
       | (LOADF (instr, Rdest, disp, Raddr)) =>
                                ((loadf_to_ascii instr) ^ tab ^
				 (msDisp(Raddr, disp)) ^ comma ^ (msReg Rdest))
       | (CBRANCHI (instr, label, true)) =>
                                ((cbri_to_ascii instr) ^ tab ^ (msLabel label))
       | (CBRANCHI (instr, label, false)) =>
                                ((cbri_to_ascii instr) ^ ",pn" ^ tab ^ "%icc," ^ (msLabel label))
       | (CBRANCHR (instr, Rsrc, label, true)) =>
                                ((cbrr_to_ascii instr) ^ tab ^ (msReg Rsrc) ^ comma ^ (msLabel label))
       | (CBRANCHR (instr, Rsrc, label, false)) =>
                                ((cbrr_to_ascii instr) ^ ",pn" ^ tab ^ (msReg Rsrc) ^ comma ^ (msLabel label))
       | (CBRANCHF (instr, label)) =>
                                ((cbrf_to_ascii instr) ^ tab ^ (msLabel label))
       | (INTOP(instr, Rsrc1, op2, Rdest)) =>
                                ((int_to_ascii instr) ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msOperand op2) ^ 
				 comma ^ (msReg Rdest))
       | (FPOP(instr, Rsrc1, Rsrc2, Rdest)) =>
                                ((fp_to_ascii instr)^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msReg Rsrc2) ^ 
				 comma ^ (msReg Rdest))
       | (FPMOVE(instr, Rsrc, Rdest)) =>
                                ((fpmove_to_ascii instr)^ tab ^
				 (msReg Rsrc) ^ comma ^ (msReg Rdest))
       | (TRAP (instr, code)) => ((trap_to_ascii instr) ^ tab ^
				  (trap_code_to_ascii code)))


  datatype finalInstr = NO_INSTRUCTION of string        (* do not prepend tab *)
                      | ONE_INSTRUCTION of string
                      | DELAY_INSTRUCTION of string     (* must insert nop *)
                      | MULTIPLE_INSTRUCTION of string list 

  fun msInstrBase (base : base_instruction) = 
      (case base of
	BSR (label, NONE, _) => DELAY_INSTRUCTION ("call" ^ tab ^ (msLabel label))
      | BSR (label, SOME sra, _) => error "can't generate code for BSR(label, SOME linkreg, _)"
	    (* The problem is jmpl is a register-indirect jump.  We could use sra as a temp. *)
	    (* DELAY_INSTRUCTION ("jmpl" ^ tab ^ (msLabel label) ^ comma ^ (msReg sra)) *)
      | (TAILCALL label) => DELAY_INSTRUCTION ("TAILCALL\t" ^ (msLabel label))
      | (BR label) => DELAY_INSTRUCTION ("ba" ^ tab ^ (msLabel label))
      | (ILABEL label) => NO_INSTRUCTION ((msLabel label) ^ ":")
      | (ICOMMENT str) => NO_INSTRUCTION ("\t! " ^ str)
      | (Core.JSR(link, Raddr, hint, _)) =>
	    DELAY_INSTRUCTION("jmpl" ^ tab ^
			      (msReg Raddr) ^ comma ^
			      (if link then (msReg Rra) else (msReg Rzero)))
      | (Core.RET(link, hint)) =>
	    DELAY_INSTRUCTION (if link
				   then ("jmpl" ^ tab ^ (msReg Rra) ^ comma ^ (msReg Rra))
			       else "retl")
      | (RTL instr) => ONE_INSTRUCTION (rtl_to_ascii instr)
      | (MOVE (Rsrc,Rdest)) =>
	 let val scratch = INT threadScratch_disp
	     val scratch2 = INT (threadScratch_disp + 4)
	 in
	     case (Rsrc,Rdest) of
		 (R _, R _) => ONE_INSTRUCTION("mov\t" ^ (msReg Rsrc) ^ comma ^ msReg Rdest)
	       | (F m, F n) => ONE_INSTRUCTION("fmovd\t" ^ (msReg Rsrc) ^ comma ^ msReg Rdest)
	       | (R n, F _) => MULTIPLE_INSTRUCTION
		                [("st\t" ^ (msReg Rsrc) ^ comma ^ (msDisp(Rth, scratch))),
				 ("st\t" ^ (msReg (R (n+1))) ^ comma ^ (msDisp(Rth, scratch2))),
				 ("ldd\t" ^ (msDisp(Rth, scratch)) ^ comma ^ (msReg Rdest))]
		| (F _, R n) => MULTIPLE_INSTRUCTION
				[("std\t" ^ (msReg Rsrc) ^ comma ^ (msDisp(Rth, scratch))),
				 ("ld\t" ^ (msDisp(Rth, scratch)) ^ comma ^ (msReg Rdest)),
				 ("ld\t" ^ (msDisp(Rth, scratch2) ^ comma ^ (msReg (R (n+1)))))]
	 end
			 
      | PUSH (Rsrc, sloc) => ONE_INSTRUCTION ("PUSH\t" ^ (msReg Rsrc) ^ comma ^ (msStackLocation sloc))
      | POP (Rdest, sloc) => ONE_INSTRUCTION ("POP\t" ^ (msReg Rdest) ^ comma ^ (msStackLocation sloc))
      | PUSH_RET NONE => ONE_INSTRUCTION "PUSH_RET none"
      | POP_RET NONE => ONE_INSTRUCTION "POP_RET none"
      | PUSH_RET (SOME(ACTUAL4 offset)) => ONE_INSTRUCTION (msInstrSpecific(STOREI(ST,Rra,INT offset, Rsp)))
      | POP_RET (SOME(ACTUAL4 offset)) => ONE_INSTRUCTION (msInstrSpecific(LOADI(LD,Rra,INT offset, Rsp)))
      | PUSH_RET (SOME sloc) => ONE_INSTRUCTION ("PUSH_RET\t" ^ (msStackLocation sloc))
      | POP_RET (SOME sloc) => ONE_INSTRUCTION ("POP_RET\t" ^ (msStackLocation sloc))
      | GC_CALLSITE label => ONE_INSTRUCTION ("GC CALLING SITE\t" ^ (msLabel label))
      | LADDR (Rdest, label) =>
	   let val str1 = msInstrSpecific(SETHI(HIGHLABEL (label, 0w0), Rdest))
	       val str2 = msInstrSpecific(INTOP(OR, Rdest, 
						IMMop (LOWLABEL (label,0w0)), Rdest))
	   in  MULTIPLE_INSTRUCTION[str1,str2]
	   end)

  fun msInstr (SPECIFIC i) = ONE_INSTRUCTION(msInstrSpecific i)
    | msInstr (BASE i) = msInstrBase i

  fun msInstrHelp(cmt, finalInstr) = 
      let val cmt' = if (cmt = "") then "" else cmt ^ "\n"
      in  (case finalInstr of
	       NO_INSTRUCTION str => cmt' ^ str ^ "\n"
	     | ONE_INSTRUCTION str => "\t" ^ str ^ cmt ^ "\n"
	     | DELAY_INSTRUCTION str => "\t" ^ str ^ cmt ^ " ! delay slot empty\n\tnop\n"
	     | MULTIPLE_INSTRUCTION strings => foldl (fn (str,acc) => acc ^"\t" ^ str ^ "\n") cmt' strings)
      end

  fun msInstruction (cmt, instr) = msInstrHelp(cmt, msInstr instr)


  fun wms arg = "0x" ^ (W.toHexString arg)

  fun fixupFloat float_string =
    let
      fun fixSigns [] = []
	| fixSigns (#"~" :: rest) = #"-" :: fixSigns rest
	| fixSigns (d :: rest) = d :: (fixSigns rest)
      fun explodeToSci f_s =
	  (explode f_s) @
	  (if ((Char.contains f_s #"e") orelse (Char.contains f_s #"E"))
	       then [] 
	   else [#"e", #"0"])
    in
      (implode o fixSigns o explodeToSci) float_string
    end


  fun fixupString string =
    let 
      fun makeDigit n = chr (n + 48)
      fun octal n =
	  implode [#"\\",
		   (makeDigit (n div 64)),
		   (makeDigit ((n div 8) mod 8)),
		   (makeDigit (n mod 8))]
	

      fun charLoop [] = ""
	| charLoop (ch::chs) =
	  if (ch = 34) then  
	      "\\\"" ^ (charLoop chs) (* quotation mark " *)
	  else if (ch = 92) then (* backslash *)
	    "\\\\" ^ (charLoop chs)
	  else if (ch >= 32 andalso ch <= 126) then
	    (String.str (chr ch)) ^ (charLoop chs)
	  else if (ch = 10) then (* newline *)
	    "\\n" ^ (charLoop chs)
	  else
	      (octal ch) ^ (charLoop chs)
    in
	charLoop (map ord (explode string))
    end

  fun single s = [(1, "\t" ^ s ^ "\n")]
  fun msData (COMMENT com) =  single ("\t! " ^ com)
    | msData (STRING (s)) = single
        (if (s = "") then
	  "! .ascii \"\" (zero length string)"
	else
	  (".ascii \"" ^ (fixupString s) ^ "\"\n.align 4"))
    | msData (INT32 (w))  = single (".word " ^ (wms w))
    | msData (FLOAT (f))  = single (".double 0r" ^ (fixupFloat f))
    | msData (DATA (label)) = single (".long " ^ (msLabel label))
    | msData (DLABEL (label))   = 
	   [(1,case label of
		 LOCAL_CODE _ => ((msLabel label) ^ ":\n")
	       | LOCAL_DATA _ => ((msLabel label) ^ ":\n")
	       | _ => ("\t.globl " ^ (msLabel label) ^ "\n" ^
		       (msLabel label) ^ ":\n"))]

   fun freshCodeLabel () = Rtl.fresh_code_label "code"
   fun freshDataLabel () = Rtl.fresh_data_label "data"
   fun freshIreg  () = let val v = Name.fresh_var() in R (Name.var2int v) end
   fun freshFreg  () = let val v = Name.fresh_var() in F (Name.var2int v) end


   fun ireg n = R n
   fun freg n = F n

   fun regLE (R n) (R n') = n <= n'
     | regLE (F n) (F n') = n <= n'
     | regLE _ _ = false

   fun eqRegs'(a,b) = eqRegs a b
   fun member (r,regs) = Listops.member_eq(eqRegs',r,regs)
   fun listdiff(r1,r2) = Listops.list_diff_eq(eqRegs',r1,r2)
   fun listintersect (r1,r2) = Listops.list_inter_eq(eqRegs',r1,r2)
   fun listunion (a,b) = a @ b
   fun listunique [] = []
     | listunique (a::b) = let val b = listunique b
			   in  if member(a,b) then b else a::b
			   end
   local
       val nums = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
		   20,21,22,23,24,25,26,27,28,29,30,31]
       val lower = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
   in
     val num_iregs = 32
     val num_fregs = 32
     val int_regs = (map ireg nums)
     val fp_regs  = (map (fn n => F(2 * n)) nums)
     val physical_regs = listunion(int_regs, fp_regs)
   end


   fun defUse (SPECIFIC(STOREI (_, Rsrc, _, Raddr)))                         = ([], [Rsrc, Raddr])
     | defUse (SPECIFIC(LOADI (_, Rdest, _, Raddr)))                        = ([Rdest], [Raddr])
     | defUse (SPECIFIC(STOREF (_, Rsrc, _, Raddr)))                        = ([], [Raddr,Rsrc])
     | defUse (SPECIFIC(LOADF (_, Rdest, _, Raddr)))                        = ([Rdest], [Raddr])
     | defUse (SPECIFIC(CBRANCHI (_, _, _)))                                = ([], [])
     | defUse (SPECIFIC(CBRANCHR (_, Rsrc, _, _)))                          = ([], [Rsrc])
     | defUse (SPECIFIC(CBRANCHF (_, _)))                                   = ([], [])
     | defUse (BASE(BR _))                                                  = ([], [])
     | defUse (BASE(BSR (_,NONE,{regs_modified,regs_destroyed,args})))      = (Rra::regs_destroyed, args)
     | defUse (BASE(BSR (_,SOME sra, {regs_modified,regs_destroyed,args}))) = (sra::regs_destroyed, args)
     | defUse (SPECIFIC(INTOP (opcode, Rsrc1, REGop Rsrc2, Rdest)))         = ([Rdest], [Rsrc1, Rsrc2])
     | defUse (SPECIFIC(INTOP (opcode, Rsrc1, IMMop _, Rdest)))             = ([Rdest], [Rsrc1])
     | defUse (SPECIFIC(FPOP (opcode, Fsrc1, Fsrc2, Fdest)))                = ([Fdest], [Fsrc1, Fsrc2])
     | defUse (SPECIFIC(FPMOVE (opcode, Fsrc, Fdest)))                      = ([Fdest], [Fsrc])
     | defUse (BASE (Core.JSR (false, Raddr, _, _)))                        = ([], [Raddr])
     | defUse (BASE (Core.JSR (true, Raddr, _, _)))                         = ([Rra], [Raddr])
     | defUse (BASE (Core.RET (false, _)))                                  = ([], [Rra])
     | defUse (BASE (Core.RET (true, _)))                                   = ([Rra], [Rra])
     | defUse (SPECIFIC (TRAP _))                                           = ([], [])
     | defUse (BASE (LADDR (Rdest,_)))                                      = ([Rdest], [])
     | defUse (BASE (RTL (Core.JMP (Raddr, _))))                            = ([], [Raddr])
     | defUse (BASE (RTL (CALL {func=DIRECT (_,SOME sra),args, results,...}))) = (results, sra :: args)
     | defUse (BASE (RTL (CALL {func=DIRECT (_,NONE), args,results,...})))  = (results, args)
     | defUse (BASE (RTL (CALL {func=INDIRECT reg,args,results,...})))      = (results, reg :: args)
     | defUse (BASE (RTL (RETURN{results})))                                = ([], Rra :: results)
     | defUse (BASE (RTL _))                                                = ([], [])
     | defUse (BASE(MOVE (Rsrc,Rdest)))                                     = ([Rdest],[Rsrc])
     | defUse (BASE(PUSH (Rsrc, _)))                                        = ([], [Rsrc, Rsp])
     | defUse (BASE(POP (Rdest, _)))                                        = ([Rdest], [Rsp])
     | defUse (BASE(PUSH_RET (_)))                                          = ([], [Rra, Rsp])
     | defUse (BASE(POP_RET (_)))                                           = ([Rra], [Rsp])
     | defUse (BASE(TAILCALL _))                                            = ([], [])
     | defUse (BASE(GC_CALLSITE _))                                         = ([], [])
     | defUse (SPECIFIC NOP)                                                = ([], [])
     | defUse (SPECIFIC (SETHI (_,Rdest)))                                  = ([Rdest], [])
     | defUse (SPECIFIC (WRY Rsrc))                                         = ([RY], [Rsrc])
     | defUse (SPECIFIC (RDY Rdest))                                        = ([Rdest], [RY])
     | defUse (SPECIFIC (CMP (Rsrc1, REGop Rsrc2)))                         = ([],[Rsrc1, Rsrc2])
     | defUse (SPECIFIC (CMP (Rsrc1, IMMop _)))                             = ([],[Rsrc1])
     | defUse (SPECIFIC (FCMPD (Rsrc1, Rsrc2)))                             = ([],[Rsrc1, Rsrc2])
     | defUse (BASE(ILABEL _))                                              = ([], [])
     | defUse (BASE(ICOMMENT _))                                            = ([], [])


   datatype instr_flow = NOBRANCH | BRANCH of bool * label list | DELAY_BRANCH of bool * label list
    (* XXX -- cbranch?  with ML_EXTERN_LABEL -- should label be mentioned? *)
    (* BASE(BR/BSR/...) is not DELAY_BRANCH because we include the nop in the printing *)
    fun cFlow (BASE (BR (label as LOCAL_CODE _)))      = BRANCH (false, [label])
      | cFlow (BASE (BR (label as LOCAL_DATA _)))      = BRANCH (false, [label])
      | cFlow (BASE (BR (label as _)))                 = BRANCH (false, [])
      | cFlow (BASE (BSR (label as LOCAL_CODE _,_,_))) = BRANCH (true, [label])
      | cFlow (BASE (BSR (label as LOCAL_DATA _,_,_))) = BRANCH (true, [label])
      | cFlow (BASE (BSR (label as _,_,_)))            = BRANCH (true, [])
      | cFlow (SPECIFIC (CBRANCHI(_, ML_EXTERN_LABEL _, _))) = DELAY_BRANCH (true, [])
      | cFlow (SPECIFIC (CBRANCHI(_, llabel, _)))                = DELAY_BRANCH (true, [llabel])
      | cFlow (SPECIFIC (CBRANCHR(_, _, ML_EXTERN_LABEL _, _)))  = DELAY_BRANCH (true, [])
      | cFlow (SPECIFIC (CBRANCHR(_, _, llabel, _)))             = DELAY_BRANCH (true, [llabel])
      | cFlow (SPECIFIC (CBRANCHF(_, ML_EXTERN_LABEL _)))        = DELAY_BRANCH (true, [])
      | cFlow (SPECIFIC (CBRANCHF(_, llabel)))                   = DELAY_BRANCH (true, [llabel])
      | cFlow (BASE (Core.JSR(_,_,_,labels))) = BRANCH (false, labels)
      | cFlow (BASE (Core.RET(_,_)))  = BRANCH (false, [])
      | cFlow (BASE(RTL(CALL {calltype=(Rtl.ML_TAIL _), ...})))  = BRANCH (true, []) (* why possible *)
      | cFlow (BASE(RTL(CALL _))) = BRANCH (true, [])
      | cFlow (BASE(RTL(RETURN _)))      = BRANCH (false, [])
      | cFlow (BASE(RTL(SAVE_CS label))) = BRANCH (true, [label])
      | cFlow (BASE(TAILCALL label))     = BRANCH (false, [])
      | cFlow _ = NOBRANCH


  (* We perform one peephole optimization: filling of jmpl/retl delay slots.
     If the first two instructions do not interfere in that their 
     def-use sets are entirely disjoint and the second instruction
     has a delay slot while the first does not (and is ultimately exactly one instruction),
     then the second can fill the delay slot of the first.
  *)

  fun msInstructions cmt_instr_list = 
      let val ci_list = map (fn (cmt,instr) => (cmt, instr, msInstr instr)) cmt_instr_list
	  fun loop [] = []
            | loop [(c,i,s)] = [msInstrHelp (c,s)]
	    | loop ((cis1 as (c1,i1,s1))::(cis2 as (c2,i2,s2))::cisRest) =
	      (case (s1,s2) of
		   (ONE_INSTRUCTION str1, DELAY_INSTRUCTION str2) =>
		       let val (def1,use1) = defUse i1
			   val (def2,use2) = defUse i2
			   val disjoint = null(listintersect(def1 @ use1, def2 @ use2))
		       in  if disjoint
			   then (msInstrHelp (c2,ONE_INSTRUCTION str2)) ::
			        (msInstrHelp (c1,ONE_INSTRUCTION str1)) ::
				loop cisRest
			   else (msInstrHelp (c1,s1)) :: (loop (cis2 :: cisRest))
		       end
		 | _ => (msInstrHelp (c1,s1)) :: (loop (cis2 :: cisRest)))
      in  loop ci_list
      end

   (* map src registers using fs and destination using fd and return mapped instruction *)
   fun translate_to_real_reg(i,fs,fd) = 
     let 
       fun xspec (STOREI(oper, Rsrc, offset, Raddr)) = STOREI(oper, fs Rsrc, offset, fs Raddr)
         | xspec (LOADI(oper, Rdst, offset, Raddr)) = LOADI(oper, fd Rdst, offset, fs Raddr)
	 | xspec (STOREF(oper, Fsrc, offset, Raddr)) = STOREF(oper, fs Fsrc,offset, fs Raddr)
         | xspec (LOADF(oper, Fdst, offset, Raddr)) = LOADF(oper, fd Fdst,offset, fs Raddr)
	 | xspec (CBRANCHI(oper, label, taken)) = CBRANCHI(oper, label, taken)
	 | xspec (CBRANCHR(oper, Rsrc, label, taken)) = CBRANCHR(oper, fs Rsrc, label, taken)
	 | xspec (CBRANCHF(oper, label)) = CBRANCHF(oper, label)
	 | xspec (INTOP(oper, Rsrc1, REGop Rsrc2, Rdst)) = INTOP(oper, fs Rsrc1,REGop (fs  Rsrc2),fd Rdst)
	 | xspec (INTOP(oper, Rsrc1, src2, Rdst) ) = INTOP(oper, fs Rsrc1,src2, fd Rdst)
	 | xspec (FPOP(oper, Fsrc1, Fsrc2, Fdest)) = FPOP(oper, fs Fsrc1,fs Fsrc2,fd Fdest)
	 | xspec (FPMOVE(oper, Fsrc, Fdest)) = FPMOVE(oper, fs Fsrc, fd Fdest)
	 | xspec (TRAP oper) = TRAP oper
	 | xspec NOP = NOP
	 | xspec (SETHI (value, Rdest)) = SETHI(value, fd Rdest)
	 | xspec (RDY Rdest) = RDY(fd Rdest)
	 | xspec (WRY Rsrc) = WRY(fs Rsrc)
	 | xspec (CMP (Rsrc1, op2 as (IMMop _))) = CMP(fs Rsrc1, op2)
	 | xspec (CMP (Rsrc1, REGop Rsrc2)) = CMP(fs Rsrc1, REGop(fs Rsrc2))
	 | xspec (FCMPD (Rsrc1, Rsrc2)) = FCMPD(fs Rsrc1, fs Rsrc2)
       fun xbase (MOVE(src,dest)) = MOVE(fs src, fd dest)
         | xbase (PUSH(src,sloc)) = PUSH(fs src, sloc)
         | xbase (POP(dest,sloc)) = POP(fd dest, sloc)
         | xbase (PUSH_RET arg) = PUSH_RET(arg)
         | xbase (POP_RET arg) = POP_RET(arg)
         | xbase (RTL rtli) = error "Cannot translate RTL instrs"
	 | xbase (TAILCALL l) = TAILCALL l
         | xbase (BR l) = BR l
         | xbase (BSR (l, NONE, md)) = BSR(l, NONE, md)
         | xbase (BSR (l, SOME d, md)) = BSR(l, SOME (fd d), md)
         | xbase (Core.JSR(link,Raddr,hint,labels)) = Core.JSR(link,fs Raddr,hint,labels)
         | xbase (Core.RET(link,hint)) = Core.RET(link,hint)
         | xbase (GC_CALLSITE l) = GC_CALLSITE l
         | xbase (ILABEL l) = ILABEL l
	 | xbase (ICOMMENT l) = ICOMMENT l
	 | xbase (LADDR (Rdst, label)) = LADDR(fd Rdst,label)
      in 
	case i of
	  SPECIFIC ii => SPECIFIC(xspec ii)
	| BASE ii => BASE(xbase ii)
      end
   fun extern_decl s = ""

   (* On the SPARC, the OS can save some state to the top of the stack
      when entering the kernel.  Since an interrupt can occur at any
      time, we must maintain some invariants:

      (1) SP must always be 8-byte aligned.

      (2) SP must point into stack space and we must not depend on the
          contents of the memory at the top of the stack (which can be
          clobbered at any time).
    *)

   (* check_not_sp : register -> unit *)
   fun check_not_sp r = if eqRegs'(r,Rsp)
			    then error ("attempt to violate stack invariatns")
			else ()

   (* load_imm' : word * register -> instruction list *)
   (* Rdest may not be SP: Between SETHI and OR in the general case,
      SP may violate invariant (2).  *)
   fun load_imm' (immed, Rdest) =
       let
	   val _ = check_not_sp Rdest
	   (* SETHI sets the upper 22 bits and zeroes the low 10 bits; 
	      OR can take a 13-bit signed immediate *)
	   val high20 = w2i(W.rshifta(immed, 12))
	   val high22  = w2i(W.rshifta(immed, 10))
	   val low10   = w2i(W.andb(immed, 0w1023))
	   val high22op = HIGHINT immed
	   val low10op = LOWINT immed
       in
	   if (high20 = 0 orelse high20 = ~1)
	       then let val low13op = INT (w2i immed) (* assumes upper 22 bits all set or all clear *)
		    in  [SPECIFIC(INTOP(OR,Rzero,IMMop low13op, Rdest))]
		    end
	   else let val setlowbits = if (low10 = 0)
					 then nil
				     else [SPECIFIC(INTOP(OR,Rdest,IMMop low10op, Rdest))]
		in  SPECIFIC(SETHI(high22op,Rdest)) :: setlowbits
		end
       end

   (* bumpReg : register * int * register -> instruction list *)
   (* r = r + offset (destroys rtemp), where offset can be large *)

   (* In order to maintain the stack invariants, we assume that if r
      is SP, then offset is aligned and it is safe to clobber the
      memory between SP and SP+offset.  When bumpReg is used for stack
      frame (de)allocation, this assumption is justified.  *)
  
   fun bumpReg (_, 0, _) = nil
     | bumpReg (r, offset, rtemp) =
       let val (intop, sz) = if offset > 0
				 then (ADD, offset)
			     else (SUB, ~offset)
       in
	   if in_imm_range sz then	(* one instruction *)
	       [SPECIFIC(INTOP(intop, r, IMMop (INT sz), r))]
	   else
	       if sz <= maxImm8 + maxImm then (* two instructions *)
		   [SPECIFIC(INTOP(intop, r, IMMop (INT maxImm8), r)),
		    (* If r is SP, then memory at SP `intop`
		       maxImm8 can be clobbered.  *)
		    SPECIFIC(INTOP(intop, r, IMMop (INT (sz - maxImm8)), r))]
	       else			(* two or three instructions *)
		   load_imm' (i2w sz, rtemp) @
		   [SPECIFIC(INTOP(intop, r, REGop rtemp, r))]
       end

   val counter
       : string -> (unit -> unit)
       = fn name => ignore o (Stats.counter name)
   val large_stack_frame = counter "Large Stack Frames"
   val large_frame_access = counter "Large Frame Accesses"
       
   fun allocate_stack_frame (sz, prevframe_maxoffset) = 
       let val _ = if sz < 0 then error "allocate_stack_frame given negative size" else ()
	   val after = freshCodeLabel()
	   val _ = if in_imm_range sz then () else large_stack_frame()
	   (* On procedure entry, Rat is not in use. *)
	   val bumpSp = fn offset => bumpReg (Rsp, offset, Rat)
       in
	   List.concat
	   [[BASE(MOVE(Rsp, Rframe))],	(* Frame pointer always gets old stack pointer value - Is this right with stacklets? *)
	    bumpSp (~sz),		(* Try to allocate frame on current stacklet *)
	    [SPECIFIC(LOADI(LD, Rat, INT stackLimit_disp, Rth)),
	     SPECIFIC(CMP (Rsp, REGop Rat)),
	     SPECIFIC(CBRANCHI(BG, after, true)),
	     BASE(MOVE(Rsp, Rframe))],
	    bumpSp sz,			(* Restore stack pointer to original value *)
	    [SPECIFIC(INTOP(OR, Rzero, IMMop (INT prevframe_maxoffset), Rat)),
	     BASE (MOVE(Rra, Rat2)),
	     BASE (BSR (Rtl.ML_EXTERN_LABEL ("NewStackletFromML"), NONE,
			{regs_modified=[Rat], regs_destroyed=[Rat],
			 args=[Rat]}))],
	    bumpSp (~sz),		(* Allocate frame on new stacklet *)
	    [BASE(ILABEL after)]]
       end
				
   fun deallocate_stack_frame sz =
       let val _ = if (sz >= 0) then ()
		   else error "deallocate_stack_frame given negative stack size"
	   (* Prior to a RET or tail call, Rat is not in use. *)
       in  bumpReg (Rsp, sz, Rat)
       end

   fun std_entry_code() = []
   fun std_return_code(NONE) = []
     | std_return_code(SOME sra) = []

   (* reduce_offset : register * int * register * (register * int -> instruction list) -> instruction list *)
   (*
      Generic support for integer and floating point loads and stores
      with effective addresses of the form base register + large
      offset.

      Note that the code generated for large offsets is not optimal
      because STOREI, LOADI, STOREF, and LOADF don't support using a
      second source register.  (This is a legal addressing mode on the
      SPARC.)  We should perhaps make the load and store instructions
      take an "operand" rather than an "imm".
	      
      For example, we want to write

	        sethi   %hi(offset),%rtmp       ! one- or two- instruction
	        or      %rtmp,%lo(offset),%rtmp ! sequence to load 32-bit immediate
	        stw     %r, [%sp + %rtmp]

      we have written
	      
	        sethi   %hi(offset),%rtmp       ! one- or two- instruction
	        or      %rtmp,%lo(offset),%rtmp ! sequence to load 32-bit immediate
		add     %sp, %rtmp, %rtmp
	        stw     %r, [%rtmp + 0]
   *)
   fun reduce_offset (base, offset, temp, f) =
       if in_ea_disp_range offset then
	   f(base, offset)
       else
	   let val _ = large_frame_access()
	   in
	       List.concat [load_imm' (i2w offset, temp),
			    [SPECIFIC(INTOP(ADD,base,REGop temp,temp))],
			    f(temp, 0)]
	   end

   fun push (src,actual_location,tmp) =
       let val reduce_offset = fn (offset, f) => reduce_offset (Rsp, offset, tmp, f)
       in
	   (case (src,actual_location)
	      of (R n, ACTUAL8 offset) => reduce_offset (offset + 4, fn (base, offset) =>
							 [SPECIFIC(STOREI(ST,   src,     INT (offset-4), base)),
							  SPECIFIC(STOREI(ST,   R (n+1), INT offset,     base))])
	       | (R _, ACTUAL4 offset) => reduce_offset (offset, fn (base, offset) =>
							 [SPECIFIC(STOREI(ST,   src,     INT offset,     base))])
	       | (F _, ACTUAL8 offset) => reduce_offset (offset, fn (base, offset) =>
							 [SPECIFIC(STOREF(STDF, src,     INT offset,     base))])
	       | (F _, ACTUAL4 offset) => reduce_offset (offset, fn (base, offset) =>
							 [SPECIFIC(STOREF(STF,  src,     INT offset,     base))])
	       | _ => error "push")
       end
   
   fun pop (dst,actual_location) =
       let val reduce_offset = fn (offset, f) => reduce_offset (Rsp, offset, dst, f)
       in
	   (case (dst,actual_location)
	      of (R n,ACTUAL8 offset) => reduce_offset (offset + 4, fn (base, offset) =>
							[SPECIFIC(LOADI(LD, R (n+1), INT offset,     base)),
							 SPECIFIC(LOADI(LD, dst,     INT (offset-4), base))])
	       | (R _,ACTUAL4 offset) => reduce_offset (offset, fn (base, offset) =>
							[SPECIFIC(LOADI(LD,   dst,   INT offset,     base))])
	       | (F _,ACTUAL8 offset) => reduce_offset (offset, fn (base, offset) =>
							[SPECIFIC(LOADF(LDDF, dst,   INT offset,     base))])
	       | (F _,ACTUAL4 offset) => reduce_offset (offset, fn (base, offset) =>
							[SPECIFIC(LOADF(LDF,  dst,   INT offset,     base))])
	       | _ => error "pop")
       end

  fun assign2s (IN_REG r) = msReg r
    | assign2s (ON_STACK s) = "STACK:" ^ (msStackLocation s)
    | assign2s (HINT r) = "HINT(" ^ (msReg r) ^ ")"
    | assign2s UNKNOWN = "UNKNOWN"    

   val special_iregs = listunique[Rzero, Rexnptr, Rth, R 3, Rheap, Rhlimit, R 6, R 7,
				  Rat, Rat2, Rsp, Rexnarg, Rra, Rframe]
   val special_fregs = listunique[Fat, Fat2]

   val general_iregs = listdiff(int_regs, listunion(special_iregs, map ireg exclude_intregs))
   val general_fregs = listdiff(fp_regs,special_fregs)

   val special_regs  = listunion(special_iregs, special_fregs) 
   val general_regs  = listdiff(physical_regs, special_regs)

      
 end
 
 open Machine

end
