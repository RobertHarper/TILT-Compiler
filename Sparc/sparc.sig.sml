(*$import MACHINE Core *)
signature SPARC =
sig

  type register = Core.register
  type label = Rtl.label

  val Rzero   : register   (* Integer zero *)
  val Rra     : register   (* The link register which holds the return address - 8 *)

  val in_imm_range : int -> bool
  val in_ea_disp_range : int -> bool
  val threadScratch_disp : int
  val writelistAlloc_disp : int
  val writelistLimit_disp : int
  val heapLimit_disp : int
  val globalOffset_disp : int
  val stackletOffset_disp : int
  val arrayOffset_disp : int

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

  datatype imm = INT of int             (* Must fit in 13 bits sign-extended *)
               | LOWINT of Word32.word  (* The low 10 bits of the word *)
               | HIGHINT of Word32.word (* The high 22 bits of the word *)
               | LOWLABEL of label * Word32.word  (* The low 10 bits of the label plus offset *)
               | HIGHLABEL of label * Word32.word (* The high 22 bits of the label plus offset *)

  datatype operand = 
    REGop of register
  | IMMop of imm

  datatype specific_instruction =
    NOP  (* stylized for easier reading *)
  (* For sethi, the imm must be of the HIGH flavor *)
  | SETHI  of imm * register
  | WRY    of register  (* the Y register is for 64-bit integer mult/div *)
  | RDY    of register
  | CMP    of register * operand
  | FCMPD  of register * register
  (* For the load/store instructions, imm must be of the LOW flavors *)
  | STOREI of storei_instruction * register * imm * register
  | LOADI  of loadi_instruction * register * imm * register
  | STOREF of storef_instruction * register * imm * register
  | LOADF  of loadf_instruction * register * imm * register
  | CBRANCHI of cbri_instruction * label
  | CBRANCHF of cbrf_instruction * label
  | INTOP  of int_instruction * register * operand * register
  | FPOP   of fp_instruction * register * register * register
  | FPMOVE  of fpmove_instruction * register * register
  | TRAP of trap_instruction

  structure Machine : MACHINE where type specific_instruction = specific_instruction

end
