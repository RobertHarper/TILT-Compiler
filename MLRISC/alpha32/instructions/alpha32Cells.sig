(* alpha32cells.sig
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
signature ALPHA32CELLS = sig

  exception Cells

  type register = int
  type regmap = int Intmap.intmap
  datatype cellclass = GP | FP | CC | CR | MEM | CTRL

  val stackptrR : int			(* stack pointer register *)
  val asmTmpR : int			(* assembly temporary *)
  val fasmTmp : int			(* floating point temporary *)


  val newCell : cellclass -> unit -> int (* generate a new name *)
  val numCell : cellclass -> unit -> int (* number of names in class *)
  val maxCell : unit -> int		 (* max id of name *)
  val cellToString : int * cellclass -> string

  val newReg : unit -> int		(* newClass GP *)
  val newFreg : unit -> int		(* newClass FP *)
  val newCCreg : unit -> int		(* newClass CC *)

  val firstPseudo : int
  val zero : cellclass -> int option 
       (* name of the register that contains zero *)

  val resetRegs : unit -> regmap (* reset any local state *)

  type cellset
  val cellset2string : cellset -> string
  val empty	     : cellset
  val addCell        : cellclass -> int * cellset -> cellset
  val cellsetToRegs  : regmap * cellset -> int list

  val addReg  : int * cellset -> cellset (* addCell GP *)
  val addFreg : int * cellset -> cellset (* addCell FP *)
end


(*
 * $Log$
# Revision 1.1  99/02/17  21:15:11  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:43  pscheng
# *** empty log message ***
#
 * Revision 1.2  1998/10/06 14:07:28  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.1  1998/05/19 15:48:49  george
 *   Version 110.5
 *
 *)
