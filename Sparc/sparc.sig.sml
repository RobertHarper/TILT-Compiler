(*$import MACHINE Core *)
signature SPARC =
sig

  type register = Core.register
  type label = Rtl.label
  type align = Rtl.align

  val Rzero   : register   (* Integer zero *)
  val Rra     : register   (* The link register which holds the return address - 8 *)

  datatype operand = 
    REGop of register
  | IMMop of int
  
  val in_imm_range : int -> bool
  val in_ea_disp_range : int -> bool
  val maxsp_disp : int

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
  | WRY    of register  (* the Y register is for 64-bit integer mult/div *)
  | RDY    of register
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

  structure Machine : MACHINE where type specific_instruction = specific_instruction

end
