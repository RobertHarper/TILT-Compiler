(* cells.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * CELLS - describes storage units on the machine, such as
 *         dedicated and general registers, memory ...
 *
 *	 This file acts as a bridge between MLRISC and the machine 
 *	 code.
 *
 *)
signature CELLS = sig
  type register = int
  type regmap = register Intmap.intmap
  eqtype cellclass
  exception Cells
  val GP   : cellclass  (* general purpose register *)
  val FP   : cellclass  (* floating point register *)
  val CC   : cellclass  (* conditional code register *)
  val MEM  : cellclass  (* memory *)
  val CTRL : cellclass  (* control dependence *)

  val stackptrR : int			(* stack pointer register *)
  val asmTmpR : int			(* assembly temporary *)
  val fasmTmp : int			(* floating point temporary *)

  val newCell : cellclass -> unit -> register (* generate a new name *)
  val numCell : cellclass -> unit -> int (* number of names in class *)
  val maxCell : unit -> int		 (* max id of name *)
  val cellToString : register * cellclass -> string

  val newReg : unit -> register		(* newClass GP *)
  val newFreg : unit -> register	(* newClass FP *)
  val newCCreg : unit -> register	(* newClass CC *)

  val firstPseudo : register
  val zero : cellclass -> register option 
       (* name of the register that contains zero *)

  val resetRegs : unit -> regmap (* reset any local state *)

  type cellset
  val cellset2string : cellset -> string
  val empty	     : cellset
  val addCell        : cellclass -> register * cellset -> cellset
  val cellsetToRegs  : regmap * cellset -> register list

  val addReg  : register * cellset -> cellset (* addCell GP *)
  val addFreg : register * cellset -> cellset (* addCell FP *)
end


(*
 * $Log$
# Revision 1.1  99/02/17  21:16:31  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:10  pscheng
# *** empty log message ***
#
 * Revision 1.4  1998/10/06 14:07:45  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.3  1998/05/25 15:11:02  george
 *   Fixed RCS keywords
 *
 *)
