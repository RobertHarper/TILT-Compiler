signature SPARCINSTR = 
sig
   structure Constant : CONSTANT
   structure Region   : REGION
   structure C : SPARCCELLS
(*
   structure A : ANNOTATIONS
*)

   datatype load = LDSB | LDSH | LDUB | LDUH | LD | LDD

   datatype store = STB | STH | ST | STD

   datatype fload = LDF | LDDF | LDFSR

   datatype fstore = STF | STDF | STFSR

   datatype ea = Direct of int
               | FDirect of int
               | Displace of {base: int, disp: int}

   datatype operand =
      REG of int
    | IMMED of int
    | LAB of LabelExp.labexp 
    | LO of LabelExp.labexp 
    | HI of LabelExp.labexp 
    | CONST of Constant.const

   datatype arith = ADD | SUB | UMUL | SMUL | UDIV | SDIV
                  | AND | ANDN | OR | ORN | XOR | XNOR 
                  | TADD | TADDTV

   datatype shift = SLL | SRL | SRA

   datatype farith1 = FiTOd | FdTOi | FiTOs | FsTOi
                    | FsTOd | FdTOs 
                    | FMOVs | FNEGs | FABSs 
                    | FMOVd | FNEGd | FABSd (* composite instructions *) 
                    | FSQRTs | FSQRTd 

   datatype farith2 = FADDd | FSUBd | FMULd | FDIVd 
                    | FADDs | FSUBs | FMULs | FDIVs 
                    | FsMULd 

   datatype fcmp = FCMPd | FCMPEd 
                 | FCMPs | FCMPEs

   datatype branch = BA | BN | BNE | BE | BG | BLE | BGE | BL | BGU 
                   | BLEU | BCC | BCS | BPOS | BNEG | BVC | BVS

   datatype fbranch = FBA | FBN | FBU | FBG | FBUG | FBL | FBUL 
                    | FBLG | FBNE | FBE | FBUE | FBGE | FBUGE 
                    | FBLE | FBULE | FBO

   (* Naming Conventions:
    *              r -- source register/base register for loads,jumps
    *              i -- operand
    *              d -- destinate register/data register for stores
    *              b -- branch/trap condition
    *              a -- annul bit for delay slot
    *              cc -- condition code bit
    *)

   datatype instruction =
      LOAD of  { l:load, r:int, i:operand, d:int, mem:Region.region }
   |  STORE of { s:store, d:int, r:int, i:operand, mem:Region.region }
   |  FLOAD of { l:fload, r:int, i:operand, d:int, mem:Region.region }
   |  FSTORE of{ s:fstore, d:int, r:int, i:operand, mem:Region.region }
   |  SETHI of { d:int, i:int }
   |  ARITH of { a:arith, cc:bool, r:int, i:operand, d:int }
   |  SHIFT of { s:shift, r:int, i:operand, d:int }
   |  Bicc of  { b:branch, a:bool, label:Label.label, nop:bool }
   |  FBfcc of { b:fbranch, a:bool, label:Label.label, nop:bool }
   |  JMP  of  { r:int, i:operand, labs : Label.label list, nop:bool  }
   |  JMPL of  { r:int, i:operand, d:int, defs:C.cellset, uses:C.cellset, 
                 nop:bool }
   |  CALL of  { defs:C.cellset, uses:C.cellset, label:Label.label, nop:bool }  
   |  Ticc of  { t:branch, r:int, i:operand}
   |  FPop1 of { a:farith1, r:int, d:int }
   |  FPop2 of { a:farith2, r1:int, r2:int, d:int }
   |  FCMP of  { cmp:fcmp, r1:int, r2:int, nop:bool }
   |  COPY of  { dst:int list, src:int list, impl:instruction list option ref,
                 tmp:ea option}
   |  FCOPY of { dst:int list, src:int list, impl:instruction list option ref,
                 tmp:ea option}
   |  SAVE of {r:int, i:operand, d:int}
   |  RESTORE of {r:int, i:operand, d:int}
   |  RDY of {d:int}
   |  WRY of {r:int,i:operand}
   |  RET of {leaf:bool,nop:bool} 
(*
   |  ANNOTATION of instruction * A.annotation
*)

   (* Reverse the conditions for branching *)
   val revCond  : branch -> branch
   val revFcond : fbranch -> fbranch

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:42  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:42  pscheng
# *** empty log message ***
#
 * Revision 1.2  1998/08/12 13:36:21  leunga
 *   Fixed the 2.0 + 2.0 == nan bug by treating FCMP as instrs with delay slots
 *
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
