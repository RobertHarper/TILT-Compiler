functor Decalpha (val exclude_intregs : int list
		  structure Rtl : RTL) : DECALPHA =
  struct
    structure Rtl = Rtl
    type data = Rtl.data

    val error = fn s => Util.error "decalpha.sml" s
    datatype register = R of int
                      | F of int
    type loclabel = Rtl.local_label
    datatype label    = I of loclabel  (* must be same as in DECALPHA *)
                      | MLE of string
                      | CE of string * register option


    datatype operand =
      REGop of register
    | IMMop of int

    datatype align = LONG (* 4 bytes *)
                   | QUAD (* 8 bytes *)
                   | OCTA (* 16 bytes *)

    val Rzero   = R 31
    val Rsp     = R 30
    val Rgp     = R 29
    val Rat     = R 28
    val Rat2    = R 23

    (* return address/exnarg never live at the
       same time *)
    val Rpv     = SOME(R 27)
    val Rra     = R 26   
    val Rexnarg = R 26  (* it's okay for exnarg and ra to be the same
			      since exnarg is active only when about to raise
			      an exception which means we are not returning *)

                             (* we'll put these in C callee-save *)
    val Rexnptr = R 9
    val Rhlimit = R 10
    val Rheap   = R 11


    val Fzero   = F 31
    val Fat     = F 30
    val Fat2    = F 29

    fun isFloat (F _) = true
      | isFloat _ = false

    fun isInt (R _) = true
      | isInt _ = false


    fun isPhysical (R i) = i<32
      | isPhysical (F i) = i<32

    datatype storei_instruction =
      STL | STQ 

    datatype storef_instruction =
      STT | STS

    datatype loadi_instruction =
      LDA | LDAH | LDL | LDQ | LDGP 

    datatype loadf_instruction =
      LDT | LDS

    datatype cbri_instruction =
      BEQ | BGE | BGT | BLE | BLT | BNE | BLBC | BLBS

    datatype cbrf_instruction =
      FBEQ | FBGE | FBGT | FBLE | FBLT | FBNE

    datatype int_instruction =
      ADDL | ADDLV | ADDQ | ADDQV | SUBL | SUBLV | SUBQ | SUBQV
    | MULL | MULLV | MULQ | MULQV | UMULH 
    | S4ADDL | S4ADDQ | S8ADDL | S8ADDQ
    | S4SUBL | S4SUBQ | S8SUBL | S8SUBQ
    | CMPEQ | CMPLE | CMPLT | CMPULE | CMPULT
    | AND | OR | XOR | EQV | ANDNOT | ORNOT | SRA | SRL | SLL | ZAP
    | CMOVEQ | CMOVNE | CMOVLT | CMOVLE | CMOVGT | CMOVGE | CMOVLBC | CMOVLBS

    datatype fp_instruction = 
      CPYS | CPYSN | CPYSE
    | CMPTEQ | CMPTLT | CMPTLE | ADDT | SUBT | MULT | DIVT    
    | FCMOVEQ | FCMOVNE | FCMOVLT | FCMOVLE | FCMOVGT | FCMOVGE

    datatype fpconv_instruction =
      CVTQT | CVTTQ | CVTTQM | CVTTQC | CVTLQ | CVTQL

    datatype jmp_instruction = 
      JMP | JSR | RET

  datatype call_type = DIRECT of label | INDIRECT of register

  datatype rtl_instruction =
    CALL of 
    {extern_call : bool,               (* Is this an extern call to C *)
     func: call_type,                  (* label or temp containing addr. *)
     args : register list,             (* integer, floating temps *)
     results : register list,          (* integer, floating temps *)
     argregs : register list option,   (* actual registers *)
     resregs : register list option,   (*   "         "    *)
     destroys: register list option,   (*   "         "    *)
     tailcall : bool}

    | RETURN of {results: register list}       (* formals *)

    | JMP of register * loclabel list
    | HANDLER_ENTRY
    | SAVE_CS of loclabel

    datatype stacklocation =
      CALLER_FRAME_ARG of int
    | THIS_FRAME_ARG of int
    | SPILLED_INT of int
    | SPILLED_FP of int
    | ACTUAL4 of int
    | ACTUAL8 of int
    | RETADD_POS


    datatype decalpha_specific_instruction =
      IALIGN of align
    | STOREI of storei_instruction * register * int * register
    | LOADI  of loadi_instruction * register * int * register
    | STOREF of storef_instruction * register * int * register
    | LOADF  of loadf_instruction * register * int * register
    | CBRANCHI of cbri_instruction * register * loclabel
    | CBRANCHF of cbrf_instruction * register * loclabel
    | INTOP  of int_instruction * register * operand * register
    | FPOP   of fp_instruction * register * register * register
    | FPCONV of fpconv_instruction * register * register
    | TRAPB				(* Trap barrier *)
   type specific_instruction = decalpha_specific_instruction

   datatype base_instruction = 
      MOVI     of register * register
    | MOVF     of register * register
    | PUSH     of register * stacklocation
    | POP      of register * stacklocation
    | PUSH_RET of stacklocation option
    | POP_RET  of stacklocation option
    | RTL      of rtl_instruction
    | TAILCALL of label
    | BR       of label
    | BSR      of label * register option * {regs_modified : register list, regs_destroyed : register list,
					     args : register list}
    | JSR      of bool * register * int * loclabel list
    | RET      of bool * int
    | GC_CALLSITE of loclabel
    | ILABEL of loclabel
    | COMMENT of string
    | LADDR of register * label         (* dest, offset, label *)

  datatype instruction = 
    BASE     of base_instruction
  | SPECIFIC of specific_instruction


    fun in_imm_range i = (i >= 0) andalso (i <= 255)
    fun in_ea_disp_range i = (i >= ~32768) andalso (i <= 32767)

    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
  fun ms n = if n<0 then ("-"^(Int.toString (~n))) else Int.toString n

  fun msReg (R 30) = "$sp"
    | msReg (R 29) = "$gp"
    | msReg (R 28) = "$at"
    | msReg (R n) = "$" ^ (ms n)
    | msReg (F n) = "$f" ^ (ms n)
      
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
        | loop (s :: rest) = (str s) ^ (loop rest)
    in
      loop o explode
    end

  fun msLoclabel (Rtl.LOCAL_CODE v) = "LC" ^ (makeAsmLabel (Name.var2string v))
    | msLoclabel (Rtl.LOCAL_DATA v) = "LD" ^ (makeAsmLabel (Name.var2string v))
  fun msLabel (I label)         = msLoclabel label
    | msLabel (MLE label)        = (makeAsmLabel label)
    | msLabel (CE (label,_))     = (makeAsmLabel label)


  fun storei_to_ascii STL = "stl"
    | storei_to_ascii STQ = "stq"

  fun storef_to_ascii STS = "sts"
    | storef_to_ascii STT = "stt"

  fun loadi_to_ascii LDA  = "lda"
    | loadi_to_ascii LDAH = "ldah"
    | loadi_to_ascii LDL  = "ldl"
    | loadi_to_ascii LDQ  = "ldq"
    | loadi_to_ascii LDGP = "ldgp"

  fun loadf_to_ascii LDS  = "lds"
    | loadf_to_ascii LDT  = "ldt"

  fun cbri_to_ascii BEQ  = "beq"
    | cbri_to_ascii BGE  = "bge"
    | cbri_to_ascii BGT  = "bgt"
    | cbri_to_ascii BLE  = "ble"
    | cbri_to_ascii BLT  = "blt"
    | cbri_to_ascii BNE  = "bne"
    | cbri_to_ascii BLBC = "blbc"
    | cbri_to_ascii BLBS = "blbs"

  fun cbrf_to_ascii FBEQ = "fbeq"
    | cbrf_to_ascii FBGE = "fbge"
    | cbrf_to_ascii FBGT = "fbgt"
    | cbrf_to_ascii FBLE = "fble"
    | cbrf_to_ascii FBLT = "fblt"
    | cbrf_to_ascii FBNE = "fbne"

  fun int_to_ascii  ADDL   = "addl"
    | int_to_ascii  ADDLV  = "addlv"
    | int_to_ascii  ADDQ   = "addq"
    | int_to_ascii  ADDQV  = "addqv"
    | int_to_ascii  SUBL   = "subl"
    | int_to_ascii  SUBLV  = "sublv"
    | int_to_ascii  SUBQ   = "subq"
    | int_to_ascii  SUBQV  = "subqv"
    | int_to_ascii  MULL   = "mull"
    | int_to_ascii  MULLV  = "mullv"
    | int_to_ascii  MULQ   = "mulq"
    | int_to_ascii  MULQV  = "mulqv"
    | int_to_ascii  UMULH  = "umulh"
    | int_to_ascii  S4ADDL = "s4addl"
    | int_to_ascii  S4ADDQ = "s4addq"
    | int_to_ascii  S8ADDL = "s8addl"
    | int_to_ascii  S8ADDQ = "s8addq"
    | int_to_ascii  S4SUBL = "s4subl"
    | int_to_ascii  S4SUBQ = "s4subq"
    | int_to_ascii  S8SUBL = "s8subl"
    | int_to_ascii  S8SUBQ = "s8subq"
    | int_to_ascii  CMPEQ  = "cmpeq"
    | int_to_ascii  CMPLT  = "cmplt"
    | int_to_ascii  CMPLE  = "cmple"
    | int_to_ascii  CMPULE = "cmpule"
    | int_to_ascii  CMPULT = "cmpult"
    | int_to_ascii  AND    = "and"  
    | int_to_ascii  OR     = "or"  
    | int_to_ascii  XOR    = "xor"  
    | int_to_ascii  EQV    = "eqv"  
    | int_to_ascii  ANDNOT = "andnot"
    | int_to_ascii  ORNOT  = "ornot"
    | int_to_ascii  SRA    = "sra"
    | int_to_ascii  SRL    = "srl"
    | int_to_ascii  SLL    = "sll"
    | int_to_ascii  ZAP    = "zap"
    | int_to_ascii  CMOVEQ = "cmoveq"
    | int_to_ascii  CMOVNE = "cmovne"
    | int_to_ascii  CMOVLT = "cmovlt"
    | int_to_ascii  CMOVLE = "cmovle"
    | int_to_ascii  CMOVGT = "cmovgt"
    | int_to_ascii  CMOVGE = "cmovge"
    | int_to_ascii  CMOVLBC= "cmovlbc"
    | int_to_ascii  CMOVLBS= "cmovlbs"

  fun fp_to_ascii CPYS     = "cpys"
    | fp_to_ascii CPYSN    = "cpysn"
    | fp_to_ascii CPYSE    = "cpyse"
    | fp_to_ascii CMPTEQ   = "cmpteq"
    | fp_to_ascii CMPTLE   = "cmptle"
    | fp_to_ascii CMPTLT   = "cmptlt"
    | fp_to_ascii ADDT     = "addt"
    | fp_to_ascii SUBT     = "subt"
    | fp_to_ascii MULT     = "mult"
    | fp_to_ascii DIVT     = "divt"
    | fp_to_ascii FCMOVEQ  = "fcmoveq"
    | fp_to_ascii FCMOVNE  = "fcmovne"
    | fp_to_ascii FCMOVLT  = "fcmovlt"
    | fp_to_ascii FCMOVLE  = "fcmovle"
    | fp_to_ascii FCMOVGT  = "fcmovgt"
    | fp_to_ascii FCMOVGE  = "fcmovge"

  fun fpconv_to_ascii CVTQT  = "cvtqt"
    | fpconv_to_ascii CVTTQ  = "cvttq"
    | fpconv_to_ascii CVTTQM = "cvttqm"
    | fpconv_to_ascii CVTTQC = "cvttqc"
    | fpconv_to_ascii CVTLQ  = "cvtlq"
    | fpconv_to_ascii CVTQL  = "cvtql"


  fun reglist_to_ascii [] = ""
    | reglist_to_ascii [r] = (msReg r)
    | reglist_to_ascii (r::rs) = (msReg r) ^ "," ^ (reglist_to_ascii rs)

  fun rtl_to_ascii (CALL {func=DIRECT func, tailcall=true, 
			  args, results,...}) =
           "(tail)CALL " ^ (msLabel func) ^ " (" ^
	   (reglist_to_ascii args) ^ " ; " ^ (reglist_to_ascii results)
           ^ ")"
    | rtl_to_ascii (CALL {func=INDIRECT Raddr, tailcall=true, ...}) = 
           "(tail)CALL via " ^ msReg Raddr
    | rtl_to_ascii (CALL {func=INDIRECT Raddr, ...}) = 
           "CALL via " ^ msReg Raddr
    | rtl_to_ascii (CALL {func=DIRECT func, args,results,...}) = 
           "CALL " ^ (msLabel func) ^ " (" ^
	   (reglist_to_ascii args) ^ " ; " ^ (reglist_to_ascii results)
           ^ ")"
    | rtl_to_ascii (JMP (Raddr,_)) = "JMP " ^ (msReg Raddr)
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

  fun sloc2int (ACTUAL4 x) = x
    | sloc2int (ACTUAL8 x) = x
    | sloc2int arg = error ("decalpha.sml: Attempt to concretize stacklocation: " ^ (msStackLocation arg))
    
  val comma  	    	       = ", "
  val tab                      = "\t"
  val newline                  = "\n"
  fun msDisp(rd, 0)	       = "(" ^  (msReg rd) ^ ")"
    | msDisp(rd, disp) 	       = (ms disp) ^ "(" ^ (msReg rd) ^ ")"
  fun msOperand (REGop r) = msReg r
    | msOperand (IMMop n) = (ms n)

  fun msInstr' (IALIGN x) =
         let val i = 
	        case x
		of LONG => 2
		 | QUAD => 3
		 | OCTA => 4
	 in tab^".align "^Int.toString i
         end
    | msInstr' (STOREI (instr, Rsrc, disp, Raddr)) =
                                (tab ^ (storei_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msDisp(Raddr, disp)))
    | msInstr' (STOREF (instr, Rsrc, disp, Raddr)) =
                                (tab ^ (storef_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msDisp(Raddr, disp)))
    | msInstr' (LOADI (instr, Rdest, disp, Raddr)) =
                                (tab ^ (loadi_to_ascii instr) ^ tab ^
				 (msReg Rdest) ^ comma ^ (msDisp(Raddr, disp)))
    | msInstr' (LOADF (instr, Rdest, disp, Raddr)) =
                                (tab ^ (loadf_to_ascii instr) ^ tab ^
				 (msReg Rdest) ^ comma ^ (msDisp(Raddr, disp)))
    | msInstr' (CBRANCHI (instr, Rtest, label)) =
                                (tab ^ (cbri_to_ascii instr) ^ tab ^
				 (msReg Rtest) ^ comma ^ (msLabel (I label)))
    | msInstr' (CBRANCHF (instr, Rtest, label)) =
                                (tab ^ (cbrf_to_ascii instr) ^ tab ^
				 (msReg Rtest) ^ comma ^ (msLabel (I label)))
    | msInstr' (INTOP(instr, Rsrc1, op2, Rdest)) =
                                (tab ^ (int_to_ascii instr) ^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msOperand op2) ^ 
				 comma ^ (msReg Rdest))
    | msInstr' (FPOP(instr, Rsrc1, Rsrc2, Rdest)) =
                                (tab ^ (fp_to_ascii instr)^ tab ^
				 (msReg Rsrc1) ^ comma ^ (msReg Rsrc2) ^ 
				 comma ^ (msReg Rdest))
    | msInstr' (FPCONV(instr, Rsrc, Rdest)) =
                                (tab ^ (fpconv_to_ascii instr) ^ tab ^
				 (msReg Rsrc) ^ comma ^ (msReg Rdest))
    | msInstr' (TRAPB) =        "\ttrapb"


  fun msInstr_base (BSR (label, NONE, _)) = (tab ^ "bsr" ^ tab ^
				           (msReg Rra) ^ comma ^ (msLabel label))
    | msInstr_base (BSR (label, SOME sra, _)) = (tab ^ "bsr" ^ tab ^
					     (msReg sra) ^ comma ^ (msLabel label))
    | msInstr_base (TAILCALL label) = ("\tTAILCALL\t" ^ (msLabel label))
    | msInstr_base (BR label) = (tab ^ "br" ^ tab ^
				           (msReg Rzero) ^ comma ^ (msLabel label))
    | msInstr_base (ILABEL loclabel) = (msLoclabel loclabel) ^ ":"
    | msInstr_base (COMMENT str) = (tab ^ "# " ^ str)
    | msInstr_base (JSR(link, Raddr, hint, _)) =
                                (tab ^ "jsr" ^ tab 
				 ^ (if link then (msReg Rra) else (msReg Rzero))
				 ^ comma ^ (msDisp(Raddr,0)) ^
				 comma ^ (ms hint))
    | msInstr_base (RET(link, hint)) =
                                (tab ^ "ret" ^ tab 
				 ^ (if link then (msReg Rra) else (msReg Rzero))
				 ^ comma ^ (msDisp(Rra,0)) ^
				 comma ^ (ms hint))
    | msInstr_base (RTL instr) =  (tab ^ (rtl_to_ascii instr))
    | msInstr_base (MOVI (Rsrc,Rdest)) =
                                ("\tmov\t" ^ (msReg Rsrc) ^ comma ^
				 msReg Rdest)
    | msInstr_base (MOVF (Rsrc,Rdest)) =
                                ("\tfmov\t" ^ (msReg Rsrc) ^ comma ^
				 msReg Rdest)
    | msInstr_base (PUSH (Rsrc, sloc)) = 
                                ("\tPUSH\t" ^ (msReg Rsrc) ^ comma ^
				 (msStackLocation sloc))
    | msInstr_base (POP (Rdest, sloc)) = 
                                ("\tPOP\t" ^ (msReg Rdest) ^ comma ^
				 (msStackLocation sloc))
    | msInstr_base (PUSH_RET NONE) = "PUSH_RET none"
    | msInstr_base (POP_RET NONE) = "POP_RET none"
    | msInstr_base (PUSH_RET (SOME(ACTUAL8 offset))) = ("\tstq\t$26, " ^ (ms offset) ^ "($sp)\t# push_ret")
    | msInstr_base (POP_RET (SOME(ACTUAL8 offset))) =  ("\tldq\t$26, " ^ (ms offset) ^ "($sp)\t# pop_ret")
    | msInstr_base (PUSH_RET (SOME sloc)) = ("\tPUSH_RET\t" ^ (msStackLocation sloc))
    | msInstr_base (POP_RET (SOME sloc)) =  ("\tPOP_RET\t" ^ (msStackLocation sloc))
    | msInstr_base (GC_CALLSITE label) = ("\tGC CALLING SITE\t" ^
                                      (msLabel (I label)))
    | msInstr_base (LADDR (Rdest, label)) = 
                                ("\tlda\t" ^ (msReg Rdest) ^ comma ^ 
				 (msLabel label))

  fun msInstr (SPECIFIC i) = msInstr' i
    | msInstr (BASE i) = msInstr_base i

  fun msInstruction cmt instr =
    ((msInstr instr) ^ (if (cmt <> "") then ("\t# " ^ cmt) else "") ^ "\n")

  fun wms arg = "0x" ^ (W.toHexString arg)


  val msRtlLocalLabel = msLoclabel
  fun msRtlLabel (Rtl.LOCAL_LABEL ll) = msRtlLocalLabel  ll
    | msRtlLabel (Rtl.ML_EXTERN_LABEL s) = msLabel (MLE s)
    | msRtlLabel (Rtl.C_EXTERN_LABEL s) = msLabel (CE (s,NONE))


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
	    (str (chr ch)) ^ (charLoop chs)
	  else if (ch = 10) then (* newline *)
	    "\\n" ^ (charLoop chs)
	  else
	      (octal ch) ^ (charLoop chs)
    in
	charLoop (map ord (explode string))
    end

  fun single s = [(1, "\t" ^ s ^ "\n")]
  fun msData (Rtl.COMMENT com) =  single ("\t# " ^ com)
    | msData (Rtl.STRING (s)) = single
        (if (s = "") then
	  "# .ascii \"\" (zero length string)"
	else
	  (".ascii \"" ^ (fixupString s) ^ "\""))
    | msData (Rtl.INT32 (w))  = single (".long " ^ (wms w))
    | msData (Rtl.INT_FLOATSIZE (w)) = single (".quad " ^ (wms w))
    | msData (Rtl.FLOAT (f))  = single (".t_floating " ^ (fixupFloat f))
    | msData (Rtl.DATA (label)) = single (".long " ^ (msRtlLabel label))
    | msData (Rtl.ARRAYI (count, w)) = single (".long " ^ (wms w) ^ " : " ^ (Int.toString count))
    | msData (Rtl.ARRAYF (count, f)) = single (".t_floating " ^ (fixupFloat f) ^ 
					       " : " ^ (Int.toString count))
    | msData (Rtl.ARRAYP (count, Rtl.PTR label)) = single (".long " ^ (msRtlLabel label) ^
							   (" : ") ^ (Int.toString count))
    | msData (Rtl.ARRAYP (count, Rtl.TAG w)) = single (".long " ^ (wms w) ^ " : " ^ 
						       (Int.toString count))
    | msData (Rtl.ALIGN (Rtl.LONG)) = single (".align 2")
    | msData (Rtl.ALIGN (Rtl.QUAD)) = single (".align 3")
    | msData (Rtl.ALIGN (Rtl.ODDLONG)) = [(1, "\t.align 3\t\t# ODDLONG\n"),
					  (1,"\t.long 0\n")]
    | msData (Rtl.ALIGN (Rtl.OCTA)) = single (".align 4\n")
    | msData (Rtl.ALIGN (Rtl.ODDOCTA)) = [(1, "\t.align 4\t\t# ODDOCTA\n"),
					  (1,"\t.quad 0\n\t.long 0\n")]
    | msData (Rtl.DLABEL (label))   = 
	   [(1,case label of
		 Rtl.LOCAL_LABEL _ => ((msRtlLabel label) ^ ":\n")
	       | _ => ("\t.globl " ^ (msRtlLabel label) ^ "\n" ^
		       (msRtlLabel label) ^ ":\n"))]

   fun freshCodeLabel () = Rtl.LOCAL_CODE (Name.fresh_var())
   fun freshDataLabel () = Rtl.LOCAL_DATA (Name.fresh_var())
   fun freshIreg  () = let val v = Name.fresh_var() in R (Name.var2int v) end
   fun freshFreg  () = let val v = Name.fresh_var() in F (Name.var2int v) end


   fun ireg n = R n
   fun freg n = F n

   fun regNum (R n) = n
     | regNum (F n) = n

   fun eqRegs (R n) (R n') = n = n'
     | eqRegs (F n) (F n') = n = n'
     | eqRegs _ _ = false

   fun regLE (R n) (R n') = n <= n'
     | regLE (F n) (F n') = n <= n'
     | regLE _ _ = false


   fun member (a,[]) = false
     | member (a,b::rest) = (eqRegs a b) orelse member(a,rest)
   fun listdiff ([],b) = []
     | listdiff (a::rest,b) = if (member(a,b)) then listdiff(rest,b) else a::(listdiff(rest,b))
   fun listintersect ([],b) = []
     | listintersect (a::rest,b) = if (member(a,b)) then a::listdiff(rest,b) else (listdiff(rest,b))
   fun listunion (a,b) = a @ b

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


   val real_Rpv = (case Rpv of
		NONE => error "no Rpv for Alpha"
	      | SOME x => x)
    fun defUse (SPECIFIC(STOREI (_, Rsrc, _, Raddr)))   = ([], [Rsrc, Raddr])
      | defUse (SPECIFIC(LOADI (_, Rdest, _, Raddr)))   = ([Rdest], [Raddr])
      | defUse (SPECIFIC(STOREF (_, Rsrc, _, Raddr)))   = ([], [Raddr,Rsrc])
      | defUse (SPECIFIC(LOADF (_, Rdest, _, Raddr)))   = ([Rdest], [Raddr])
      | defUse (SPECIFIC(CBRANCHI (_, Rsrc, _)))        = ([], [Rsrc])
      | defUse (SPECIFIC(CBRANCHF (_, Rsrc, _)))        = ([], [Rsrc])
      | defUse (BASE(BR _))                             = ([], [])
      | defUse (BASE(BSR (_,NONE,{regs_modified,regs_destroyed,args}))) = (Rra::regs_destroyed, args)
      | defUse (BASE(BSR (_, SOME sra, {regs_modified,regs_destroyed,args}))) = (sra::regs_destroyed, args)
      | defUse (SPECIFIC(INTOP (opcode, Rsrc1, sv, Rdest))) =
	let
	    val temp1 = (case opcode of
			     CMOVEQ  => [Rdest]
			   | CMOVNE  => [Rdest]
			   | CMOVLT  => [Rdest]
			   | CMOVLE  => [Rdest]
			   | CMOVGT  => [Rdest]
			   | CMOVGE  => [Rdest]
			   | CMOVLBC => [Rdest]
			   | CMOVLBS => [Rdest]
			   | _       => [])
	    val temp2 = (case sv of 
		              REGop r => r::temp1
			    | _       => temp1)
	in
	    ([Rdest],Rsrc1::temp2)
	end
      | defUse (SPECIFIC(FPOP (opcode, Fsrc1, Fsrc2, Fdest))) = 
 	let
	    val temp1 = (case opcode of
			     FCMOVEQ  => [Fdest]
			   | FCMOVNE  => [Fdest]
			   | FCMOVLT  => [Fdest]
			   | FCMOVLE  => [Fdest]
			   | FCMOVGT  => [Fdest]
			   | FCMOVGE  => [Fdest]
			   | _        => [])
	in
	    ([Fdest],Fsrc1::Fsrc2::temp1)
	end
      | defUse (SPECIFIC(FPCONV (_, Fsrc, Fdest)))      = ([Fdest], [Fsrc])
      | defUse (BASE (JSR (false, Raddr, _, _)))       = ([], [Raddr])
      | defUse (BASE (JSR (true, Raddr, _, _)))       = ([Rra], [Raddr])
      | defUse (BASE (RET (false, _)))              = ([], [Rra])
      | defUse (BASE (RET (true, _)))               = ([Rra], [Rra])
      | defUse (SPECIFIC TRAPB)                         = ([], [])
      | defUse (BASE (LADDR (Rdest,_)))             = ([Rdest], [])
      | defUse (BASE (RTL (JMP (Raddr, _))))        = ([], [Raddr])
      | defUse (BASE (RTL (CALL {func=DIRECT (CE (_,SOME sra)),args,
				 results, ...})))   = (results, real_Rpv :: sra :: args)
      | defUse (BASE (RTL (CALL {func=DIRECT (CE (_,NONE)), args,
				 results, ...})))   = (results, real_Rpv :: args)
      | defUse (BASE (RTL (CALL {func=INDIRECT reg,
				 args, results, ...})))  = (results, real_Rpv :: reg :: args)
      | defUse (BASE (RTL (RETURN{results})))      = ([], Rra :: results)
      | defUse (BASE (RTL _))                      = ([], [])
      | defUse (BASE(MOVI (Rsrc,Rdest)))           = ([Rdest],[Rsrc])
      | defUse (BASE(MOVF (Rsrc,Rdest)))           = ([Rdest],[Rsrc])
      | defUse (BASE(PUSH (Rsrc, _)))              = ([], [Rsrc, Rsp])
      | defUse (BASE(POP (Rdest, _)))              = ([Rdest], [Rsp])
      | defUse (BASE(PUSH_RET (_)))                = ([], [Rra, Rsp])
      | defUse (BASE(POP_RET (_)))                 = ([Rra], [Rsp])
      | defUse (BASE(TAILCALL _))             = ([], [])
      | defUse (BASE(GC_CALLSITE _))               = ([], [])
      | defUse (SPECIFIC (IALIGN _))               = ([], [])
      | defUse (BASE(ILABEL _))                    = ([], [])
      | defUse (BASE(COMMENT _))                   = ([], [])


    (* SOME (fallthrough possible, other local labels this instr jumps to)
        if this instruction ends basic block; NONE otherwise *)
    fun cFlow (BASE (BR (I label)))    = SOME (false, [label])
      | cFlow (BASE (BR (CE _)))       = SOME (false, [])
      | cFlow (BASE (BR (MLE _)))      = SOME (false, [])
      | cFlow (BASE (BSR (I label,_,_))) = SOME (true, [label])
      | cFlow (BASE (BSR (CE _,_,_)))    = SOME (true, [])
      | cFlow (BASE (BSR (MLE _,_,_)))   = SOME (true, [])
      | cFlow (SPECIFIC (CBRANCHI(_, _, label))) = SOME (true, [label])
      | cFlow (SPECIFIC (CBRANCHF(_, _, label))) = SOME (true, [label])
      | cFlow (BASE (JSR(_,_,_,labels)))         = SOME (false, labels)
      | cFlow (BASE (RET(_,_)))  = SOME (false, [])
      | cFlow (BASE(RTL(CALL {tailcall=false, ...}))) = SOME (true, [])
      | cFlow (BASE(RTL(CALL {tailcall=true, ...})))  = SOME (true, []) 
(*      | cFlow (BASE(RTL(CALL {tailcall=true, ...})))  = SOME (false, []) *)
      | cFlow (BASE(RTL(RETURN _)))          = SOME (false, [])
      | cFlow (BASE(RTL(SAVE_CS label))) = SOME (true, [label])
      | cFlow (BASE(TAILCALL label))         = SOME (false, [])
      | cFlow _ = NONE

   fun eqLLabs (Rtl.LOCAL_CODE v) (Rtl.LOCAL_CODE v') = Name.eq_var(v,v')
     | eqLLabs (Rtl.LOCAL_DATA v) (Rtl.LOCAL_DATA v') = Name.eq_var(v,v')
     | eqLLabs _ _ = false
   fun eqLabs (I l) (I l') = eqLLabs l l'
     | eqLabs (MLE s) (MLE s') = (s = s')
     | eqLabs (CE (s,_)) (CE (s',_)) = (s = s')
     | eqLabs _ _ = false

   (* map src registers using fs and destination using fd and return mapped instruction *)
   fun translate_to_real_reg(i,fs,fd) = 
     let 
       fun xspec (STOREI(oper, Rsrc, offset, Raddr)) = STOREI(oper, fs Rsrc, offset, fs Raddr)
         | xspec (LOADI(oper, Rdst, offset, Raddr)) = LOADI(oper, fd Rdst, offset, fs Raddr)
	 | xspec (STOREF(oper, Fsrc, offset, Raddr)) = STOREF(oper, fs Fsrc,offset, fs Raddr)
         | xspec (LOADF(oper, Fdst, offset, Raddr)) = LOADF(oper, fd Fdst,offset, fs Raddr)
	 | xspec (CBRANCHI(oper, Rsrc, llabel)) = CBRANCHI(oper, fs Rsrc,llabel)
	 | xspec (CBRANCHF(oper, Fsrc, llabel)) = CBRANCHF(oper, fs Fsrc,llabel)
	 | xspec (INTOP(oper, Rsrc1, REGop Rsrc2, Rdst)) = INTOP(oper, fs Rsrc1,REGop (fs  Rsrc2),fd Rdst)
	 | xspec (INTOP(oper, Rsrc1, src2, Rdst) ) = INTOP(oper, fs Rsrc1,src2, fd Rdst)
	 | xspec (FPOP(oper, Fsrc1, Fsrc2, Fdest)) = FPOP(oper, fs Fsrc1,fs Fsrc2,fd Fdest)
         | xspec (FPCONV(oper, Fsrc, Fdest)) = FPCONV(oper, fs Fsrc,fd Fdest)
	 | xspec TRAPB = TRAPB
	 | xspec (IALIGN ia) = IALIGN ia
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
         | xbase (JSR(link,Raddr,hint,labels)) = JSR(link,fs Raddr,hint,labels)
         | xbase (RET(link,hint)) = RET(link,hint)
         | xbase (GC_CALLSITE l) = GC_CALLSITE l
         | xbase (ILABEL l) = ILABEL l
	 | xbase (COMMENT l) = COMMENT l
	 | xbase (LADDR (Rdst, label)) = LADDR(fd Rdst,label)
      in 
	case i of
	  SPECIFIC ii => SPECIFIC(xspec ii)
	| BASE ii => BASE(xbase ii)
      end
   fun extern_decl s = ""


   val pv = (case Rpv of
		 NONE => error "no Rpv for Alpha"
	       | SOME x => x)

   fun increase_stackptr sz = SPECIFIC(LOADI(LDA, Rsp, sz, Rsp))
   fun decrease_stackptr (sz : int) = SPECIFIC(LOADI(LDA, Rsp, ~sz, Rsp))
   fun std_entry_code() = [SPECIFIC(LOADI(LDGP, Rgp, 0, pv))]
   fun std_return_code(NONE) = [SPECIFIC(LOADI(LDGP, Rgp, 0, Rra))]
     | std_return_code(SOME sra) = [SPECIFIC(LOADI(LDGP, Rgp, 0, sra))]
   fun push (src,actual_location) =
       case (src,actual_location)
       of (R _, ACTUAL8 offset) => SPECIFIC(STOREI(STQ, src, offset, Rsp))
        | (R _, ACTUAL4 offset) => SPECIFIC(STOREI(STL, src, offset, Rsp))
	| (F _, ACTUAL8 offset) => SPECIFIC(STOREF(STT, src, offset, Rsp))
	| _ => error "push"

   fun pop (dst,actual_location) = 
       case (dst,actual_location)
       of (R _,ACTUAL8 offset) => SPECIFIC(LOADI(LDQ, dst, offset, Rsp))
        | (R _,ACTUAL4 offset) => SPECIFIC(LOADI(LDL, dst, offset, Rsp))
	| (F _,ACTUAL8 offset) => SPECIFIC(LOADF(LDT, dst, offset, Rsp))
	| _ => error "allocateBlock: pop"


   (* Where a given pseudoregister is physically located *)
   datatype assign = IN_REG of register
                   | ON_STACK of stacklocation
                   | HINT of register
		   | UNKNOWN

   fun eqAssigns (IN_REG a) (IN_REG b) = eqRegs a b
     | eqAssigns (ON_STACK a) (ON_STACK b) = (a = b)
     | eqAssigns _ _ = false

   (* information on a procedure *)

   datatype procsig = 
     PROCSIG of {arg_ra_pos : (assign list) option,
		 res_ra_pos : assign list option,
		 allocated  : bool,
		 regs_destroyed  : register list ref,
		 regs_modified  : register list ref,
		 blocklabels: loclabel list,
                 framesize  : int option,
		 ra_offset : int option,
		 callee_saved: register list,
		 args   : register list,  (* formals *)
		 res    : register list}  (* formals *)

  fun assign2s (IN_REG r) = msReg r
    | assign2s (ON_STACK s) = "STACK:" ^ (msStackLocation s)
    | assign2s (HINT r) = "HINT(" ^ (msReg r) ^ ")"
    | assign2s UNKNOWN = "UNKNOWN"    


   val pv = (case Rpv of
                 NONE => error "no Rpv for Alpha"
               | SOME x => x)

   fun increase_stackptr sz = SPECIFIC(LOADI(LDA, Rsp, sz, Rsp))
   fun decrease_stackptr (sz : int) = SPECIFIC(LOADI(LDA, Rsp, ~sz, Rsp))
   fun std_entry_code() = [SPECIFIC(LOADI(LDGP, Rgp, 0, pv))]
   fun std_return_code(NONE) = [SPECIFIC(LOADI(LDGP, Rgp, 0, Rra))]
     | std_return_code(SOME sra) = [SPECIFIC(LOADI(LDGP, Rgp, 0, sra))]
   fun push (src,actual_location) =
       case (src,actual_location)
       of (R _, ACTUAL8 offset) => SPECIFIC(STOREI(STQ, src, offset, Rsp))
        | (R _, ACTUAL4 offset) => SPECIFIC(STOREI(STL, src, offset, Rsp))
        | (F _, ACTUAL8 offset) => SPECIFIC(STOREF(STT, src, offset, Rsp))
        | _ => error "push"

   fun pop (dst,actual_location) = 
       case (dst,actual_location)
       of (R _,ACTUAL8 offset) => SPECIFIC(LOADI(LDQ, dst, offset, Rsp))
        | (R _,ACTUAL4 offset) => SPECIFIC(LOADI(LDL, dst, offset, Rsp))
        | (F _,ACTUAL8 offset) => SPECIFIC(LOADF(LDT, dst, offset, Rsp))
        | _ => error "allocateBlock: pop"


   val special_iregs = [Rzero, Rgp, Rat, Rsp, Rheap, Rhlimit, Rat2, Rexnptr, Rexnarg]
   val special_fregs = [Fat, Fat2, Fzero]

   val general_iregs = listdiff(listdiff(int_regs,special_iregs),
				(map ireg exclude_intregs))
   val general_fregs = listdiff(fp_regs,special_fregs)

   val special_regs  = listunion(special_iregs, special_fregs) 
   val general_regs  = listdiff(physical_regs, special_regs)

end
