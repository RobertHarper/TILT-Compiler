(*
 * Common opcodes for various architectures
 *)

signature SSA_BASE_EXP =
sig

  val intTy : SSAExp.ty
  val uintTy : SSAExp.ty

  (*
   * Integer arithmetic in the natural size of the architecture
   *)
  val ADD   : SSAExp.exp
  val SUB   : SSAExp.exp
  val SMUL  : SSAExp.exp
  val SDIV  : SSAExp.exp
  val UMUL  : SSAExp.exp
  val UDIV  : SSAExp.exp
  val ANDB  : SSAExp.exp
  val ORB   : SSAExp.exp
  val XORB  : SSAExp.exp
  val NAND  : SSAExp.exp
  val NOR   : SSAExp.exp
  val XNOR  : SSAExp.exp
  val NOTB  : SSAExp.exp
  val NOT   : SSAExp.exp
  val SRA   : SSAExp.exp
  val SRL   : SSAExp.exp
  val SLL   : SSAExp.exp

  (*
   * Integer arithmetic with overflow trapping 
   * in the natural size of the architecture
   *)
  val ADDT  : SSAExp.exp
  val SUBT  : SSAExp.exp
  val SMULT : SSAExp.exp
  val SDIVT : SSAExp.exp
  val UMULT : SSAExp.exp
  val UDIVT : SSAExp.exp

  (*
   * Signed Integer comparison
   *)
  val LT  : SSAExp.exp
  val GT  : SSAExp.exp
  val GE  : SSAExp.exp
  val LE  : SSAExp.exp
  val LTU : SSAExp.exp
  val GTU : SSAExp.exp
  val LEU : SSAExp.exp
  val GEU : SSAExp.exp
  val EQ  : SSAExp.exp
  val NE  : SSAExp.exp

  (*
   * Load instructions using base+offset addressing mode
   *)
  val LOAD8   : SSAExp.exp (* sign extend *)
  val LOADU8  : SSAExp.exp
  val LOAD16  : SSAExp.exp (* sign extend *)
  val LOADU16 : SSAExp.exp
  val LOAD32  : SSAExp.exp (* sign extend *)
  val LOADU32 : SSAExp.exp
  val LOAD64  : SSAExp.exp
  val LOADF   : SSAExp.exp
  val LOADD   : SSAExp.exp

  (*
   * Store instructions using base+offset addressing mode
   *)
  val STORE8  : SSAExp.exp
  val STORE16 : SSAExp.exp
  val STORE32 : SSAExp.exp
  val STORE64 : SSAExp.exp
  val STOREF  : SSAExp.exp
  val STORED  : SSAExp.exp

  (*
   * Single precision floating point arithmetic 
   *)
  val ADDF    : SSAExp.exp
  val SUBF    : SSAExp.exp
  val MULF    : SSAExp.exp
  val DIVF    : SSAExp.exp
  val NEGF    : SSAExp.exp
  val ABSF    : SSAExp.exp
  val SQRTF   : SSAExp.exp

  (*
   * Double precision floating point arithmetic 
   *)
  val ADDD    : SSAExp.exp
  val SUBD    : SSAExp.exp
  val MULD    : SSAExp.exp
  val DIVD    : SSAExp.exp
  val NEGD    : SSAExp.exp
  val ABSD    : SSAExp.exp
  val SQRTD   : SSAExp.exp
 
  (*
   * Type conversions
   *)
  val CVTI2D  : SSAExp.exp
  val CVTD2I  : SSAExp.exp
  val CVTI2F  : SSAExp.exp
  val CVTF2I  : SSAExp.exp
  val CVTF2D  : SSAExp.exp
  val CVTD2F  : SSAExp.exp

  (*
   * Jumps
   *)
  val JMP     : SSAExp.exp (* arity 1 *)
  val CALL    : SSAExp.exp (* arity 1 *)
  val IDXJMP  : SSAExp.exp (* arity 2 *)
  val IDXCALL : SSAExp.exp (* arity 2 *)

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:44  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:47  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:17  pscheng
# *** empty log message ***
#
 *)
