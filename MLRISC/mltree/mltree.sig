(* mltree.sig
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)
signature MLTREE = sig
  structure Constant : CONSTANT
  structure PseudoOp : PSEUDO_OPS
  structure Region   : REGION
  structure BNames   : BLOCK_NAMES

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
    | LOADD  of rexp * Region.region

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

end (* MLTREE *)


(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:25  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:25  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:26  pscheng
# *** empty log message ***
#
 * Revision 1.2  1998/07/25 03:08:20  george
 *   added to support block names in MLRISC
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
