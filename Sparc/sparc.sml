(*$import RTL SPARC String Rtl Util Char *)

structure  Sparc :> SPARC =
struct

    val exclude_intregs = []
    val error = fn s => Util.error "sparc.sml" s

structure Machine = 
  struct
    open Rtl
    open Core

    datatype operand = REGop of register
	             | IMMop of int

    val Rpv     = NONE
    val Rcc     = R (~1) (* useful for defining interferences WRT status register *)
    val Rzero   = R 0   (* Standard Sparc convention *)
    val Rsp     = R 14  (* Standard Sparc convention *)
    val Rat     = R 16  (* Standard Sparc convention *)
    val Rra     = R 15  (* Standard Sparc convention *)
    val Rexnarg = R 16  (* Rexnarg and Rra are never simultaneously live
			      since exnarg is active only when about to raise
			      an exception which means we are not normally returning *)

    val Rat2    = R 17  (* Put our second temporary in a volatile *)
    val Rexnptr = R 1   (* non-volatile *)
    val Rhlimit = R 5   (* non-volatile *)
    val Rheap   = R 4   (* non-volatile *)
    val Rth     = R 2   (* non-volatile *)

    val Fat     = F 31  (* volatile *)
    val Fat2    = F 30  (* volatile *)


    fun isPhysical (R i) = i<32
      | isPhysical (F i) = i<32

  datatype storei_instruction = ST | STUB | STD
  datatype storef_instruction = STF | STDF
  datatype loadi_instruction  = LD | LDUB | LDD
  datatype loadf_instruction  = LDF | LDDF
  (* BCC = branch on carry-clear = BGEU;  BCS = branch on carry-set = BLU *)
  datatype cbri_instruction   = BE | BNE | BG | BGE | BL | BLE | BGU | BLEU | BCC | BCS 
  datatype cbrf_instruction   = FBE | FBNE | FBG | FBGE | FBL | FBLE 
  datatype trap_instruction   = TVS
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

  datatype specific_instruction =
    IALIGN of align
  | NOP  (* stylized for easier reading *)
  | SETHI  of int * register
  | CMP    of register * operand
  | FCMPD  of register * register
  | STOREI of storei_instruction * register * int * register
  | LOADI  of loadi_instruction * register * int * register
  | STOREF of storef_instruction * register * int * register
  | LOADF  of loadf_instruction * register * int * register
  | CBRANCHI of cbri_instruction * label
  | CBRANCHF of cbrf_instruction * label
  | INTOP  of int_instruction * register * operand * register
  | FPOP   of fp_instruction * register * register * register
  | FPMOVE  of fpmove_instruction * register * register
  | TRAP of trap_instruction

    datatype instruction = 
	BASE     of base_instruction
      | SPECIFIC of specific_instruction

    fun in_imm_range i = (i >= 0) andalso (i <= 255)
    fun in_ea_disp_range i = (i >= ~32768) andalso (i <= 32767)

    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
    fun ms n = if n<0 then ("-"^(Int.toString (~n))) else Int.toString n
	
    fun msReg (R 14) = "%sp"
      | msReg (R 30) = "%fp"
      | msReg (R n) = "%r" ^ (ms n)
      | msReg (F n) = "%f" ^ (ms (2 * n))
	
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
    | msLabel (ML_EXTERN_LABEL label)     = (makeAsmLabel label)


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

  fun trap_to_ascii TVS  = "tvs"

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



  fun msStackLocation (CALLER_FRAME_ARG i) = "CallerFrameArg["^(ms i)^"]"
    | msStackLocation (THIS_FRAME_ARG i) = "ThisFrameArg["^(ms i)^"]"
    | msStackLocation (SPILLED_INT i) = "Spill-I["^(ms i)^"]"
    | msStackLocation (SPILLED_FP i) = "Spill-F["^(ms i)^"]"
    | msStackLocation (ACTUAL4 i) = "4@" ^ (ms i)
    | msStackLocation (ACTUAL8 i) = "8@" ^ (ms i)
    | msStackLocation (RETADD_POS) = "RETADD_POS"

  val comma  	    	       = ", "
  val tab                      = "\t"
  val newline                  = "\n"
  fun msDisp(rd, 0)	       = "[" ^ (msReg rd) ^ "]"
    | msDisp(rd, disp) 	       = "[" ^ (msReg rd) ^ "+" ^ (ms disp) ^ "]"
  fun msOperand (REGop r) = msReg r
    | msOperand (IMMop n) = (ms n)


  fun msInstr' (IALIGN x) =
         let val i = 
	        case x of
		   LONG => 4
		 | QUAD => 8
		 | OCTA => 16
		 | ODDLONG => error "ODDLONG not handled"
		 | ODDOCTA => error "ODDOCTA not handled"
	 in tab^".align "^Int.toString i
         end
    | msInstr' NOP = tab ^ "nop"
    | msInstr' (SETHI (value, Rdest)) =
                                (tab ^ "sethi" ^ tab ^
				 (ms value) ^ comma ^ (msReg Rdest))
    | msInstr' (CMP (Rsrc1, op2)) =
                                (tab ^ "cmp" ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msOperand op2))
    | msInstr' (FCMPD (Rsrc1, Rsrc2)) =
                                (tab ^ "fcmpd" ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msReg Rsrc2))
    | msInstr' (STOREI (instr, Rsrc, disp, Raddr)) =
                                (tab ^ (storei_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msDisp(Raddr, disp)))
    | msInstr' (STOREF (instr, Rsrc, disp, Raddr)) =
                                (tab ^ (storef_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msDisp(Raddr, disp)))
    | msInstr' (LOADI (instr, Rdest, disp, Raddr)) =
                                (tab ^ (loadi_to_ascii instr) ^ tab ^
				 (msDisp(Raddr, disp)) ^ comma ^ (msReg Rdest))
    | msInstr' (LOADF (instr, Rdest, disp, Raddr)) =
                                (tab ^ (loadf_to_ascii instr) ^ tab ^
				 (msDisp(Raddr, disp)) ^ comma ^ (msReg Rdest))
    | msInstr' (CBRANCHI (instr, label)) =
                                (tab ^ (cbri_to_ascii instr) ^ tab ^ (msLabel label))
    | msInstr' (CBRANCHF (instr, label)) =
                                (tab ^ (cbrf_to_ascii instr) ^ tab ^ (msLabel label))
    | msInstr' (INTOP(instr, Rsrc1, op2, Rdest)) =
                                (tab ^ (int_to_ascii instr) ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msOperand op2) ^ 
				 comma ^ (msReg Rdest))
    | msInstr' (FPOP(instr, Rsrc1, Rsrc2, Rdest)) =
                                (tab ^ (fp_to_ascii instr)^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msReg Rsrc2) ^ 
				 comma ^ (msReg Rdest))
    | msInstr' (FPMOVE(instr, Rsrc, Rdest)) =
                                (tab ^ (fpmove_to_ascii instr)^ tab ^
				 (msReg Rsrc) ^ comma ^ (msReg Rdest))
    | msInstr' (TRAP instr) = (tab ^ (trap_to_ascii instr))


  fun msInstr_base (BSR (label, NONE, _)) = (tab ^ "call" ^ tab ^ (msLabel label) ^ "\n\tnop")
    | msInstr_base (BSR (label, SOME sra, _)) = (tab ^ "jmpl" ^ tab ^ (msLabel label) ^
						     comma ^ (msReg sra) ^ "\n\tnop")
    | msInstr_base (TAILCALL label) = ("\tTAILCALL\t" ^ (msLabel label))
    | msInstr_base (BR label) = (tab ^ "ba" ^ tab ^ (msLabel label) ^ "\n\tnop")
    | msInstr_base (ILABEL label) = (msLabel label) ^ ":"
    | msInstr_base (ICOMMENT str) = (tab ^ "! " ^ str)
    | msInstr_base (Core.JSR(link, Raddr, hint, _)) =
                                (tab ^ "jmpl" ^ tab ^
				 (msReg Raddr) ^ comma ^
				 (if link then (msReg Rra) else (msReg Rzero)) ^ "\n\tnop")
    | msInstr_base (Core.RET(link, hint)) =
                                (if link
				  then (tab ^ "jmpl" ^ tab ^ (msReg Rra) ^ comma ^ (msReg Rra))
				else (tab ^ "retl"))
				^ "\n\tnop"
    | msInstr_base (RTL instr) =  (tab ^ (rtl_to_ascii instr))
    | msInstr_base (MOVI (Rsrc,Rdest)) =
                                ("\tmov\t" ^ (msReg Rsrc) ^ comma ^ msReg Rdest)
    | msInstr_base (MOVF (Rsrc,Rdest)) =
                                ("\tfmov\t" ^ (msReg Rsrc) ^ comma ^ msReg Rdest)
    | msInstr_base (PUSH (Rsrc, sloc)) = 
                                ("\tPUSH\t" ^ (msReg Rsrc) ^ comma ^ (msStackLocation sloc))
    | msInstr_base (POP (Rdest, sloc)) = 
                                ("\tPOP\t" ^ (msReg Rdest) ^ comma ^ (msStackLocation sloc))
    | msInstr_base (PUSH_RET NONE) = "PUSH_RET none"
    | msInstr_base (POP_RET NONE) = "POP_RET none"
    | msInstr_base (PUSH_RET (SOME(ACTUAL4 offset))) = msInstr'(STOREI(ST,Rra,offset, Rsp))
    | msInstr_base (POP_RET (SOME(ACTUAL4 offset))) = msInstr'(LOADI(LD,Rra,offset, Rsp))
    | msInstr_base (PUSH_RET (SOME sloc)) = ("\tPUSH_RET\t" ^ (msStackLocation sloc))
    | msInstr_base (POP_RET (SOME sloc)) =  ("\tPOP_RET\t" ^ (msStackLocation sloc))
    | msInstr_base (GC_CALLSITE label) = ("\tGC CALLING SITE\t" ^ (msLabel label))
    | msInstr_base (LADDR (Rdest, label)) = ("\tsethi\t%hi(" ^ (msLabel label) ^ "), " ^ (msReg Rdest)) ^
					  ("\n\tor\t" ^ (msReg Rdest) ^ 
						",%lo(" ^ (msLabel label) ^ "), " ^ (msReg Rdest))

  fun msInstr (SPECIFIC i) = msInstr' i
    | msInstr (BASE i) = msInstr_base i

  fun msInstruction cmt instr =
    ((msInstr instr) ^ (if (cmt <> "") then ("\t! " ^ cmt) else "") ^ "\n")

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
	  (".ascii \"" ^ (fixupString s) ^ "\""))
    | msData (INT32 (w))  = single (".word " ^ (wms w))
    | msData (FLOAT (f))  = single (".double 0r" ^ (fixupFloat f))
    | msData (DATA (label)) = single (".long " ^ (msLabel label))
    | msData (ALIGN (LONG)) = single (".align 4")
    | msData (ALIGN (QUAD)) = single (".align 8")
    | msData (ALIGN (ODDLONG)) = [(1, "\t.align 8\t\t! ODDLONG\n"),
					  (1,"\t.word 0\n")]
    | msData (ALIGN (OCTA)) = single (".align 16\n")
    | msData (ALIGN (ODDOCTA)) = [(1, "\t.align 16\t\t! ODDOCTA\n"),
					  (1,"\t.word 0\n\t.word 0\n\t.long 0\n")]
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
   in
     val num_iregs = 32
     val num_fregs = 32
       val int_regs = (map ireg nums)
       val fp_regs  = (map freg nums)
       val physical_regs = listunion(int_regs, fp_regs)
   end


    fun defUse (SPECIFIC(STOREI (_, Rsrc, _, Raddr)))   = ([], [Rsrc, Raddr])
      | defUse (SPECIFIC(LOADI (_, Rdest, _, Raddr)))   = ([Rdest], [Raddr])
      | defUse (SPECIFIC(STOREF (_, Rsrc, _, Raddr)))   = ([], [Raddr,Rsrc])
      | defUse (SPECIFIC(LOADF (_, Rdest, _, Raddr)))   = ([Rdest], [Raddr])
      | defUse (SPECIFIC(CBRANCHI (_, _)))        = ([], [])
      | defUse (SPECIFIC(CBRANCHF (_, _)))        = ([], [])
      | defUse (BASE(BR _))                       = ([], [])
      | defUse (BASE(BSR (_,NONE,{regs_modified,regs_destroyed,args}))) = (Rra::regs_destroyed, args)
      | defUse (BASE(BSR (_, SOME sra, {regs_modified,regs_destroyed,args}))) = (sra::regs_destroyed, args)
      | defUse (SPECIFIC(INTOP (opcode, Rsrc1, REGop Rsrc2, Rdest))) = ([Rdest], [Rsrc1, Rsrc2])
      | defUse (SPECIFIC(INTOP (opcode, Rsrc1, IMMop _, Rdest))) = ([Rdest], [Rsrc1])
      | defUse (SPECIFIC(FPOP (opcode, Fsrc1, Fsrc2, Fdest))) = ([Fdest],[Fsrc1,Fsrc2])
      | defUse (SPECIFIC(FPMOVE (opcode, Fsrc, Fdest))) = ([Fdest],[Fsrc])
      | defUse (BASE (Core.JSR (false, Raddr, _, _)))       = ([], [Raddr])
      | defUse (BASE (Core.JSR (true, Raddr, _, _)))       = ([Rra], [Raddr])
      | defUse (BASE (Core.RET (false, _)))              = ([], [Rra])
      | defUse (BASE (Core.RET (true, _)))               = ([Rra], [Rra])
      | defUse (SPECIFIC (TRAP _))                        = ([], [])
      | defUse (BASE (LADDR (Rdest,_)))             = ([Rdest], [])
      | defUse (BASE (RTL (Core.JMP (Raddr, _))))        = ([], [Raddr])
      | defUse (BASE (RTL (CALL {func=DIRECT (_,SOME sra),args,
				 results, ...})))   = (results, sra :: args)
      | defUse (BASE (RTL (CALL {func=DIRECT (_,NONE), args,
				 results, ...})))   = (results, args)
      | defUse (BASE (RTL (CALL {func=INDIRECT reg,
				 args, results, ...})))  = (results, reg :: args)
      | defUse (BASE (RTL (RETURN{results})))      = ([], Rra :: results)
      | defUse (BASE (RTL _))                      = ([], [])
      | defUse (BASE(MOVI (Rsrc,Rdest)))           = ([Rdest],[Rsrc])
      | defUse (BASE(MOVF (Rsrc,Rdest)))           = ([Rdest],[Rsrc])
      | defUse (BASE(PUSH (Rsrc, _)))              = ([], [Rsrc, Rsp])
      | defUse (BASE(POP (Rdest, _)))              = ([Rdest], [Rsp])
      | defUse (BASE(PUSH_RET (_)))                = ([], [Rra, Rsp])
      | defUse (BASE(POP_RET (_)))                 = ([Rra], [Rsp])
      | defUse (BASE(TAILCALL _))                  = ([], [])
      | defUse (BASE(GC_CALLSITE _))               = ([], [])
      | defUse (SPECIFIC (IALIGN _))               = ([], [])
      | defUse (SPECIFIC NOP)                      = ([], [])
      | defUse (SPECIFIC (SETHI (_,Rdest)))        = ([Rdest], [])
      | defUse (SPECIFIC (CMP (Rsrc1, REGop Rsrc2)))     = ([],[Rsrc1, Rsrc2])
      | defUse (SPECIFIC (CMP (Rsrc1, IMMop _)))     = ([],[Rsrc1])
      | defUse (SPECIFIC (FCMPD (Rsrc1, Rsrc2)))   = ([],[Rsrc1, Rsrc2])
      | defUse (BASE(ILABEL _))                    = ([], [])
      | defUse (BASE(ICOMMENT _))                  = ([], [])


   datatype instr_flow = NOBRANCH | BRANCH of bool * label list | DELAY_BRANCH of bool * label list
    (* BASE(BR/BSR/...) is not DELAY_BRANCH because we include the nop in the printing *)
    fun cFlow (BASE (BR (label as LOCAL_CODE _)))      = BRANCH (false, [label])
      | cFlow (BASE (BR (label as LOCAL_DATA _)))      = BRANCH (false, [label])
      | cFlow (BASE (BR (label as _)))                 = BRANCH (false, [])
      | cFlow (BASE (BSR (label as LOCAL_CODE _,_,_))) = BRANCH (true, [label])
      | cFlow (BASE (BSR (label as LOCAL_DATA _,_,_))) = BRANCH (true, [label])
      | cFlow (BASE (BSR (label as _,_,_)))            = BRANCH (true, [])
      | cFlow (SPECIFIC (CBRANCHI(_, label))) = DELAY_BRANCH (true, [label])
      | cFlow (SPECIFIC (CBRANCHF(_, label))) = DELAY_BRANCH (true, [label])
      | cFlow (BASE (Core.JSR(_,_,_,labels))) = BRANCH (false, labels)
      | cFlow (BASE (Core.RET(_,_)))  = BRANCH (false, [])
      | cFlow (BASE(RTL(CALL {calltype=(Rtl.ML_TAIL _), ...})))  = BRANCH (true, []) (* why possible *)
      | cFlow (BASE(RTL(CALL _))) = BRANCH (true, [])
      | cFlow (BASE(RTL(RETURN _)))      = BRANCH (false, [])
      | cFlow (BASE(RTL(SAVE_CS label))) = BRANCH (true, [label])
      | cFlow (BASE(TAILCALL label))     = BRANCH (false, [])
      | cFlow _ = NOBRANCH


   (* map src registers using fs and destination using fd and return mapped instruction *)
   fun translate_to_real_reg(i,fs,fd) = 
     let 
       fun xspec (STOREI(oper, Rsrc, offset, Raddr)) = STOREI(oper, fs Rsrc, offset, fs Raddr)
         | xspec (LOADI(oper, Rdst, offset, Raddr)) = LOADI(oper, fd Rdst, offset, fs Raddr)
	 | xspec (STOREF(oper, Fsrc, offset, Raddr)) = STOREF(oper, fs Fsrc,offset, fs Raddr)
         | xspec (LOADF(oper, Fdst, offset, Raddr)) = LOADF(oper, fd Fdst,offset, fs Raddr)
	 | xspec (CBRANCHI(oper, llabel)) = CBRANCHI(oper, llabel)
	 | xspec (CBRANCHF(oper, llabel)) = CBRANCHF(oper, llabel)
	 | xspec (INTOP(oper, Rsrc1, REGop Rsrc2, Rdst)) = INTOP(oper, fs Rsrc1,REGop (fs  Rsrc2),fd Rdst)
	 | xspec (INTOP(oper, Rsrc1, src2, Rdst) ) = INTOP(oper, fs Rsrc1,src2, fd Rdst)
	 | xspec (FPOP(oper, Fsrc1, Fsrc2, Fdest)) = FPOP(oper, fs Fsrc1,fs Fsrc2,fd Fdest)
	 | xspec (FPMOVE(oper, Fsrc, Fdest)) = FPMOVE(oper, fs Fsrc, fd Fdest)
	 | xspec (TRAP oper) = TRAP oper
	 | xspec (IALIGN ia) = IALIGN ia
	 | xspec NOP = NOP
	 | xspec (SETHI (value, Rdest)) = SETHI(value, fd Rdest)
	 | xspec (CMP (Rsrc1, op2 as (IMMop _))) = CMP(fs Rsrc1, op2)
	 | xspec (CMP (Rsrc1, REGop Rsrc2)) = CMP(fs Rsrc1, REGop(fs Rsrc2))
	 | xspec (FCMPD (Rsrc1, Rsrc2)) = FCMPD(fs Rsrc1, fs Rsrc2)
       fun xbase (MOVI(src,dest)) = MOVI(fs src, fd dest)
         | xbase (MOVF(src,dest)) = MOVF(fs src, fd dest)
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


   fun increase_stackptr sz = SPECIFIC(INTOP(ADD, Rsp, IMMop sz, Rsp))
   fun decrease_stackptr sz = SPECIFIC(INTOP(SUB, Rsp, IMMop sz, Rsp))
   fun std_entry_code() = []
   fun std_return_code(NONE) = []
     | std_return_code(SOME sra) = []
   fun push (src,actual_location) =
       case (src,actual_location) of
          (R _, ACTUAL8 offset) => SPECIFIC(STOREI(STD,  src, offset, Rsp))
        | (R _, ACTUAL4 offset) => SPECIFIC(STOREI(ST,   src, offset, Rsp))
	| (F _, ACTUAL8 offset) => SPECIFIC(STOREF(STDF, src, offset, Rsp))
	| _ => error "push"

   fun pop (dst,actual_location) = 
       case (dst,actual_location) of
          (R _,ACTUAL8 offset) => SPECIFIC(LOADI(LDD,  dst, offset, Rsp))
        | (R _,ACTUAL4 offset) => SPECIFIC(LOADI(LD,   dst, offset, Rsp))
	| (F _,ACTUAL8 offset) => SPECIFIC(LOADF(LDDF, dst, offset, Rsp))
	| _ => error "pop"


  fun assign2s (IN_REG r) = msReg r
    | assign2s (ON_STACK s) = "STACK:" ^ (msStackLocation s)
    | assign2s (HINT r) = "HINT(" ^ (msReg r) ^ ")"
    | assign2s UNKNOWN = "UNKNOWN"    

   val special_iregs = listunique[Rzero, Rexnptr, Rth, R 3, Rheap, Rhlimit, R 6, R 7,
				  Rat, Rat2, Rsp, Rexnarg, Rra]
   val special_fregs = listunique[Fat, Fat2]

   val general_iregs = listdiff(int_regs, listunion(special_iregs, map ireg exclude_intregs))
   val general_fregs = listdiff(fp_regs,special_fregs)

   val special_regs  = listunion(special_iregs, special_fregs) 
   val general_regs  = listdiff(physical_regs, special_regs)

 end
 
 open Machine

end
