(*$import INTSYN *)
(* Names of Constants and Variables *)
(* Author: Frank Pfenning *)
(* Modified: Jeff Polakow *)

signature FIXITY =
sig

  datatype associativity = Left | Right | None
  datatype precedence = Strength of int

  val maxPrec : precedence
  val minPrec : precedence

  val less : precedence * precedence -> bool
  val leq : precedence * precedence -> bool
  val compare : precedence * precedence -> order

  val inc : precedence -> precedence
  val dec : precedence -> precedence

  datatype fixity =
      Nonfix
    | Infix of precedence * associativity
    | Prefix of precedence
    | Postfix of precedence

  val prec : fixity -> precedence
  val toString : fixity -> string

end;  (* signature FIXITY *)

signature NAMES =
sig

  structure IntSyn : INTSYN

  exception Error of string

  structure Fixity : FIXITY

  (* Constant names and fixities *)
  val reset : unit -> unit

  val installFixity : string * Fixity.fixity -> unit
  val getFixity : int (*IntSyn.cid*) -> Fixity.fixity
  val fixityLookup : string -> Fixity.fixity

  val installName : string * int (*IntSyn.cid*) -> unit
  val nameLookup : string -> int (*IntSyn.cid*) option
  val constName : int (*IntSyn.cid*) -> string	(* will mark if shadowed *)

  (* Name preferences for anonymous variables: a, EPref, UPref *)
  val installNamePref : string * (string * string option) -> unit

  (* EVar and BVar name choices *)
  val varReset : unit -> unit
  val getFVarType : string -> IntSyn.Exp (* create, if undefined *)
  val getEVar : string -> IntSyn.Exp (* create, if undefined *)
  val getEVarOpt : string -> IntSyn.Exp option (* NONE, if undefined or not EVar *)
  val evarName : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Exp -> string (* create, if undefined *)
  val bvarName : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * int -> string (* must be defined *)

  val decName  : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Dec -> IntSyn.Dec (* status unknown, like decEName *)
  val decEName : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Dec -> IntSyn.Dec (* assign existential name *)
  val decUName : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Dec -> IntSyn.Dec (* assign universal name *)
  val decLUName: IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) * IntSyn.Dec -> IntSyn.Dec (* assign local universal name *)

  val ctxName  : IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) -> IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) (* assign global existential names *)
  val ctxLUName: IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) -> IntSyn.Dec IntSyn.Ctx(*IntSyn.dctx*) (* assign local universal names *)

  val nameConDec : IntSyn.ConDec -> IntSyn.ConDec

  (* Skolem constants *)
  val skonstName : string -> string

  (* Named EVars, used for queries *)
  val namedEVars : unit -> (IntSyn.Exp * string) list
  (* Uninstantiated named EVars with constraints *)
  val evarCnstr : unit -> (IntSyn.Exp * string) list
end;  (* signature NAMES *)
