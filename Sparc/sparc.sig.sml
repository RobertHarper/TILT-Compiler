(*$import MACHINE *)
signature SPARC =
sig

  structure Machine : MACHINE

  type register = Machine.register
  type label = Rtl.label
  type align = Rtl.align

  val Rzero   : register   (* Integer zero *)
  val Rra     : register   (* The link register which holds the return address - 8 *)

  datatype operand = 
    REGop of register
  | IMMop of int
  
  val in_imm_range : int -> bool
  val in_ea_disp_range : int -> bool

  datatype storei_instruction = ST | STD
  datatype storef_instruction = STF | STDF
  datatype loadi_instruction  = LD | LDD
  datatype loadf_instruction  = LDF | LDDF
  (* BCC = branch on carry-clear = BGEU;  BCS = branch on carry-set = BLU *)
  datatype cbri_instruction   = BE | BNE | BG | BGE | BL | BLE | BGU | BLEU | BCC | BCS 
  datatype cbrf_instruction   = FBE | FBNE | FBG | FBGE | FBL | FBLE 
  datatype trap_instruction   = TVS
  datatype fpconv_instruction = FITOS | FITOD | FSTOI | FDTOI
  datatype int_instruction    =
    ADD | ADDCC | SUB | SUBCC
  | SMUL | SMULCC | UMUL | UMULCC 
  | SDIV | SDIVCC | UDIV | UDIVCC 
  | AND | OR | XOR | ANDCC | ORCC | XORCC 
  | SRA | SRL | SLL

  datatype fp_instruction    = 
    FADDD | FSUBD | FMULD | FDIVD 

  datatype fpmove_instruction = 
    FABSD | FNEGD | FMOV


  datatype specific_instruction =
    IALIGN of align
  | NOP  (* stylized for easier reading *)
  | SETHI  of int * register
  | CMP    of register * register
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
  | FPCONV of fpconv_instruction * register * register
  | TRAP of trap_instruction

  sharing type Machine.specific_instruction = specific_instruction

end
