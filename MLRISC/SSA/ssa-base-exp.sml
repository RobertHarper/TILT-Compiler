functor SSABaseExpFn(val intTy : SSAExp.ty
                     val uintTy : SSAExp.ty
                    ) : SSA_BASE_EXP =
struct


  structure E = SSAExp

  val intTy = intTy
  val uintTy = uintTy 

  (*
   * Common opcodes 
   *)
  val ID0   = E.ID 0
  val ID1   = E.ID 1
  val ADD   = E.BINOP(E.ADD,intTy,ID0,ID1)
  val SUB   = E.BINOP(E.SUB,intTy,ID0,ID1)
  val SMUL  = E.BINOP(E.MUL,intTy,ID0,ID1)
  val SDIV  = E.BINOP(E.DIV,intTy,ID0,ID1)
  val UMUL  = E.BINOP(E.MUL,uintTy,ID0,ID1)
  val UDIV  = E.BINOP(E.DIV,uintTy,ID0,ID1)
  val ANDB  = E.BINOP(E.ANDB,intTy,ID0,ID1)
  val ORB   = E.BINOP(E.ORB,intTy,ID0,ID1)
  val XORB  = E.BINOP(E.XORB,intTy,ID0,ID1)
  val NAND  = E.UNARY(E.NOT,intTy,ANDB)
  val NOR   = E.UNARY(E.NOT,intTy,ORB)
  val XNOR  = E.UNARY(E.NOT,intTy,XORB)
  val SRA   = E.BINOP(E.SRA,intTy,ID0,ID1)
  val SRL   = E.BINOP(E.SRL,intTy,ID0,ID1)
  val SLL   = E.BINOP(E.SLL,intTy,ID0,ID1)
  val NOT   = E.UNARY(E.NOT,intTy,ID0)
  val NOTB  = E.UNARY(E.NOTB,intTy,ID0)

  val ADDT  = E.BINOP(E.ADDT,intTy,ID0,ID1)
  val SUBT  = E.BINOP(E.SUBT,intTy,ID0,ID1)
  val SMULT = E.BINOP(E.MULT,intTy,ID0,ID1)
  val SDIVT = E.BINOP(E.DIVT,intTy,ID0,ID1)
  val UMULT = E.BINOP(E.MULT,uintTy,ID0,ID1)
  val UDIVT = E.BINOP(E.DIVT,uintTy,ID0,ID1)

  val ADDR    = E.BINOP(E.ADD,intTy,ID0,ID1)

  val LT      = E.BINOP(E.CMP E.LT,intTy,ID0,ID1)
  val GT      = E.BINOP(E.CMP E.GT,intTy,ID0,ID1)
  val LE      = E.BINOP(E.CMP E.LE,intTy,ID0,ID1)
  val GE      = E.BINOP(E.CMP E.GE,intTy,ID0,ID1)
  val LTU     = E.BINOP(E.CMP E.LTU,intTy,ID0,ID1)
  val GTU     = E.BINOP(E.CMP E.GTU,intTy,ID0,ID1)
  val LEU     = E.BINOP(E.CMP E.LEU,intTy,ID0,ID1)
  val GEU     = E.BINOP(E.CMP E.GEU,intTy,ID0,ID1)
  val EQ      = E.BINOP(E.CMP E.EQ,intTy,ID0,ID1)
  val NE      = E.BINOP(E.CMP E.NE,intTy,ID0,ID1)

  val LOAD8   = E.LOAD(E.I8,ADDR)
  val LOADU8  = E.LOAD(E.U8,ADDR)
  val LOAD16  = E.LOAD(E.I16,ADDR)
  val LOADU16 = E.LOAD(E.U16,ADDR)
  val LOAD32  = E.LOAD(E.I32,ADDR)
  val LOADU32 = E.LOAD(E.U32,ADDR)
  val LOAD64  = E.LOAD(E.I32,ADDR)
  val LOADF   = E.LOAD(E.F,ADDR)
  val LOADD   = E.LOAD(E.D,ADDR)

  val STORE8  = E.STORE(E.I8,ID0,ADDR)
  val STORE16 = E.STORE(E.I16,ID0,ADDR)
  val STORE32 = E.STORE(E.I32,ID0,ADDR)
  val STORE64 = E.STORE(E.I64,ID0,ADDR)
  val STOREF  = E.STORE(E.F,ID0,ADDR)
  val STORED  = E.STORE(E.D,ID0,ADDR)

  val ADDF    = E.BINOP(E.ADD,E.F,ID0,ID1)
  val SUBF    = E.BINOP(E.SUB,E.F,ID0,ID1)
  val MULF    = E.BINOP(E.MUL,E.F,ID0,ID1)
  val DIVF    = E.BINOP(E.DIV,E.F,ID0,ID1)
  val NEGF    = E.UNARY(E.NEG,E.F,ID0)
  val ABSF    = E.UNARY(E.ABS,E.F,ID0)
  val SQRTF   = E.UNARY(E.SQRT,E.F,ID0)

  val ADDD    = E.BINOP(E.ADD,E.D,ID0,ID1)
  val SUBD    = E.BINOP(E.SUB,E.D,ID0,ID1)
  val MULD    = E.BINOP(E.MUL,E.D,ID0,ID1)
  val DIVD    = E.BINOP(E.DIV,E.D,ID0,ID1)
  val NEGD    = E.UNARY(E.NEG,E.D,ID0)
  val ABSD    = E.UNARY(E.ABS,E.D,ID0)
  val SQRTD   = E.UNARY(E.SQRT,E.D,ID0)

  val CVTI2D  = E.UNARY(E.CVT intTy,E.D,ID0)
  val CVTD2I  = E.UNARY(E.CVT E.D,intTy,ID0)
  val CVTI2F  = E.UNARY(E.CVT intTy,E.F,ID0)
  val CVTF2I  = E.UNARY(E.CVT E.F,intTy,ID0)
  val CVTF2D  = E.UNARY(E.CVT E.F,E.D,ID0)
  val CVTD2F  = E.UNARY(E.CVT E.D,E.F,ID0)

  val JMP     = E.JMP(ID0) 
  val IDXJMP  = E.JMP(ADDR) 
  val CALL    = E.CALL(ID0)
  val IDXCALL = E.CALL(ADDR)

end

structure SSABaseExp32 = SSABaseExpFn(val intTy = SSAExp.I32
                                      val uintTy = SSAExp.U32)
