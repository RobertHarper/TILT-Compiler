(* mltree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

functor MLTreeF(structure Const : CONSTANT
		structure P : PSEUDO_OPS
		structure R : REGION
		structure B : BLOCK_NAMES) : MLTREE =
struct
  structure Constant = Const
  structure PseudoOp = P
  structure Region = R
  structure BNames = B

  datatype cond = LT | LTU | LE | LEU | EQ | NEQ | GE | GEU | GT | GTU
  datatype fcond =
    == | ?<> | ? | <=> | > | >= | ?> | ?>= | < | <= | ?< | ?<= | <> | ?= 
  datatype order = LR | RL

  datatype stm =
      MV     of int * rexp			(* REG(dest) := src *)
    | FMV    of int * fexp
    | CCMV   of int * ccexp

    | COPY   of int list * int list
    | FCOPY  of int list * int list

    | JMP    of rexp * Label.label list
    | CALL   of rexp * mlrisc list * mlrisc list
    | RET

    | STORE8  of rexp * rexp * Region.region	(* address, data *)
    | STORE32 of rexp * rexp * Region.region
    | STORED  of rexp * fexp * Region.region
    | STORECC of rexp * ccexp * Region.region

    | BCC    of cond * ccexp * Label.label 
    | FBCC   of fcond * ccexp * Label.label

  and rexp = 
      REG    of int
    | LI     of int
    | LI32   of Word32.word
    | LABEL  of LabelExp.labexp
    | CONST  of Constant.const

    | ADD    of rexp * rexp
    | SUB    of rexp * rexp * order
    | MULU   of rexp * rexp
    | DIVU   of rexp * rexp * order

    | ADDT   of rexp * rexp 
    | MULT   of rexp * rexp
    | SUBT   of rexp * rexp * order
    | DIVT   of rexp * rexp * order

    | LOAD8  of rexp * Region.region
    | LOAD32 of rexp * Region.region

    | ANDB   of rexp * rexp
    | ORB    of rexp * rexp
    | XORB   of rexp * rexp

    | SRA   of rexp * rexp * order		(* value, shift *)
    | SRL   of rexp * rexp * order
    | SLL   of rexp * rexp * order

    | SEQ of stm * rexp

  and fexp =
      FREG   of int
    | LOADD  of rexp  * Region.region

    | FADDD  of fexp * fexp
    | FMULD  of fexp * fexp
    | FSUBD  of fexp * fexp * order
    | FDIVD  of fexp * fexp * order
    | FABSD  of fexp 
    | FNEGD  of fexp

    | CVTI2D of rexp

    | FSEQ   of stm * fexp

  and ccexp =
      CC     of int
    | LOADCC of rexp * Region.region
    | CMP    of cond * rexp * rexp * order
    | FCMP   of fcond * fexp * fexp * order

  and mlrisc = CCR of ccexp | GPR of rexp | FPR of fexp

  datatype mltree = 
      BEGINCLUSTER
    | PSEUDO_OP of PseudoOp.pseudo_op
    | DEFINELABEL of Label.label
    | ENTRYLABEL of Label.label
    | CODE of stm list
    | BLOCK_NAME of BNames.name
    | ORDERED of mltree list
    | ESCAPEBLOCK of mlrisc list 
    | ENDCLUSTER of int Intmap.intmap
end (* MLTree *)

