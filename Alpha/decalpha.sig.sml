(*$import MACHINE *)
signature DECALPHA =
sig

  type register = Core.register
  type label = Rtl.label
  type align = Rtl.align

  val Rzero   : register   (* Integer zero *)
  val Rgp     : register   (* Alpha's $gp, pointer to global offset table *)
  val Rra     : register   (* The link register which holds the return address *)

  val Fzero   : register   (* Floating-point zero *)


  datatype operand = 
    REGop of register
  | IMMop of int
  
  val in_imm_range : int -> bool
  val in_ea_disp_range : int -> bool

  datatype storei_instruction = STL | STQ 
  datatype storef_instruction = STT | STS
  datatype loadi_instruction  = LDA | LDAH | LDL | LDQ | LDGP 
  datatype loadf_instruction  = LDT | LDS
  datatype cbri_instruction   = BEQ | BGE | BGT | BLE | BLT | BNE | BLBC | BLBS
  datatype cbrf_instruction   = FBEQ | FBGE | FBGT | FBLE | FBLT | FBNE
  datatype fpconv_instruction = CVTQT | CVTTQ | CVTTQM | CVTTQC | CVTLQ | CVTQL
  datatype int_instruction    =
    ADDL | ADDLV | ADDQ | ADDQV | SUBL | SUBLV | SUBQ | SUBQV
  | MULL | MULLV | MULQ | MULQV | UMULH 
  | S4ADDL | S4ADDQ | S8ADDL | S8ADDQ
  | S4SUBL | S4SUBQ | S8SUBL | S8SUBQ
  | CMPEQ | CMPLE | CMPLT | CMPULE | CMPULT
  | AND | OR | XOR | EQV | ANDNOT | ORNOT | SRA | SRL | SLL | ZAP
  | CMOVEQ | CMOVNE | CMOVLT | CMOVLE | CMOVGT | CMOVGE | CMOVLBC | CMOVLBS
  datatype fp_instruction    = 
    CPYS | CPYSN | CPYSE
  | CMPTEQ | CMPTLT | CMPTLE | ADDT | SUBT | MULT | DIVT    
  | FCMOVEQ | FCMOVNE | FCMOVLT | FCMOVLE | FCMOVGT | FCMOVGE


  datatype specific_instruction =
    IALIGN of align
  | STOREI of storei_instruction * register * int * register
  | LOADI  of loadi_instruction * register * int * register
  | STOREF of storef_instruction * register * int * register
  | LOADF  of loadf_instruction * register * int * register
  | CBRANCHI of cbri_instruction * register * label
  | CBRANCHF of cbrf_instruction * register * label
  | INTOP  of int_instruction * register * operand * register
  | FPOP   of fp_instruction * register * register * register
  | FPCONV of fpconv_instruction * register * register
  | TRAPB			      (* Trap barrier *)

  structure Machine : MACHINE where type specific_instruction = specific_instruction

end
