functor SparcInstr(structure Const : CONSTANT
                   structure Region : REGION
(*
                   structure Annotations : ANNOTATIONS
*)
                  ) : SPARCINSTR = 
struct
   structure Constant = Const
   structure Region   = Region
   structure C = SparcCells
(*
   structure A = Annotations
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
   |  JMP  of  { r:int, i:operand, labs : Label.label list, nop:bool }
   |  JMPL of  { d:int, r:int, i:operand, defs:C.cellset, uses:C.cellset, 
                 nop:bool }
   |  CALL of  { defs:C.cellset, uses:C.cellset, label:Label.label, nop:bool }
   |  Ticc of  { t:branch, r:int, i:operand }
   |  FPop1 of { a:farith1, r:int, d:int }
   |  FPop2 of { a:farith2, r1:int, r2:int, d:int }
   |  FCMP of  { cmp:fcmp, r1:int, r2:int, nop:bool }
   |  COPY of  { dst:int list, src:int list, impl:instruction list option ref,
                 tmp:ea option}
   |  FCOPY of { dst:int list, src:int list, impl:instruction list option ref,
                 tmp:ea option}
   |  SAVE of { r:int, i:operand, d:int}
   |  RESTORE of {r:int, i:operand, d:int}
   |  RDY of {d:int}
   |  WRY of {r:int,i:operand}
   |  RET of {leaf:bool,nop:bool}
(*
   |  ANNOTATION of instruction * A.annotation
*)

   
   fun revCond BA = BN
     | revCond BN = BA
     | revCond BNE = BE
     | revCond BE  = BNE
     | revCond BG  = BLE
     | revCond BLE = BG 
     | revCond BGE = BL
     | revCond BL  = BGE
     | revCond BGU = BLEU
     | revCond BLEU = BGU
     | revCond BCC  = BCS
     | revCond BCS  = BCC
     | revCond BPOS = BNEG 
     | revCond BNEG = BPOS
     | revCond BVC  = BVS
     | revCond BVS  = BVC

   fun revFcond FBA   = FBN
     | revFcond FBN   = FBA
     | revFcond FBU   = FBO
     | revFcond FBG   = FBULE
     | revFcond FBUG  = FBLE
     | revFcond FBL   = FBUGE
     | revFcond FBUL  = FBGE
     | revFcond FBLG  = FBUE
     | revFcond FBNE  = FBE
     | revFcond FBE   = FBNE
     | revFcond FBUE  = FBLG
     | revFcond FBGE  = FBUL
     | revFcond FBUGE = FBL
     | revFcond FBLE  = FBUG
     | revFcond FBULE = FBG
     | revFcond FBO   = FBU
end

