(* labelExp.sml -- expressions involving labels
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)
signature LABELEXP = sig
  datatype labexp = 
      LABEL of Label.label
    | CONST of int
    | PLUS of labexp * labexp
    | MINUS of labexp * labexp
    | MULT of labexp * labexp
    | DIV of labexp * labexp
    | LSHIFT of labexp * word
    | RSHIFT of labexp * word
    | AND of labexp * word
    | OR of labexp * word

  val valueOf : labexp -> int
  val toString : labexp -> string
end

structure LabelExp = struct
  datatype labexp = 
      LABEL of Label.label
    | CONST of int
    | PLUS of labexp * labexp
    | MINUS of labexp * labexp
    | MULT of labexp * labexp
    | DIV of labexp * labexp
    | LSHIFT of labexp * word
    | RSHIFT of labexp * word
    | AND of labexp * word
    | OR of labexp * word

  val itow = Word.fromInt
  val wtoi = Word.toIntX

  fun valueOf(LABEL lab) = Label.addrOf lab
    | valueOf(CONST i) = i
    | valueOf(PLUS(lexp1, lexp2)) = valueOf(lexp1) + valueOf(lexp2)
    | valueOf(MINUS(lexp1, lexp2)) = valueOf(lexp1) - valueOf(lexp2)
    | valueOf(MULT(lexp1, lexp2)) = valueOf(lexp1) * valueOf(lexp2)
    | valueOf(DIV(lexp1, lexp2)) = valueOf(lexp1) div valueOf(lexp2)
    | valueOf(LSHIFT(lexp, cnt)) = wtoi(Word.<<(wValueOf lexp, cnt))
    | valueOf(RSHIFT(lexp, cnt)) = wtoi(Word.>>(wValueOf lexp, cnt))
    | valueOf(AND(lexp, mask)) = wtoi(Word.andb(wValueOf lexp, mask))
    | valueOf(OR(lexp, mask)) = wtoi(Word.orb(wValueOf lexp, mask))

  and wValueOf lexp = itow(valueOf lexp)

  (* This module should be parameterised, in order to generate
   * target label expressions for assembly code purposes.
   *)
  fun parenthesize str = "(" ^ str ^ ")"

  fun pToString(lexp as LABEL _) = toString lexp
    | pToString(lexp as CONST _) = toString lexp
    | pToString lexp = parenthesize(toString lexp)

  and toString(LABEL lab) = Label.nameOf lab 
    | toString(CONST i) = Int.toString i
    | toString(PLUS(lexp1, lexp2)) =  pToString lexp1 ^ "+" ^ pToString lexp2
    | toString(MINUS(lexp1, lexp2)) = pToString lexp1 ^ "-" ^ pToString lexp2
    | toString(MULT(lexp1, lexp2)) = pToString lexp1 ^ "*" ^ pToString lexp2
    | toString(DIV(lexp1, lexp2)) = pToString lexp1 ^ "/" ^ pToString lexp2
    | toString(LSHIFT(lexp, cnt)) = pToString lexp ^ "<<" ^ Word.toString cnt
    | toString(RSHIFT(lexp, cnt)) = pToString lexp ^ ">>" ^ Word.toString cnt
    | toString(AND(lexp, mask)) = pToString lexp ^ "&" ^ Word.toString mask
    | toString(OR(lexp, mask)) = pToString lexp ^ "|" ^ Word.toString mask
end



