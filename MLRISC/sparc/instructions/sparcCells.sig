(* sparcCells.sig
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
signature SPARCCELLS = sig

  exception Cells

  type register = int
  type regmap = register Intmap.intmap
  datatype cellclass = GP | FP | CC | MEM | CTRL 
                     | Y    (* multiply register *)
                     | PSR  (* processor status register *)
                     | FSR  (* floating point status register *)

  val stackptrR : register		(* stack pointer register *)
  val asmTmpR : register		(* assembly temporary *)
  val fasmTmp : register		(* floating point temporary *)
  val y       : register                     
  val psr     : register
  val fsr     : register
  val linkReg : register

  val newCell : cellclass -> unit -> register (* generate a new name *)
  val numCell : cellclass -> unit -> int (* number of names in class *)
  val maxCell : unit -> int		 (* max id of name *)
  val cellToString : register * cellclass -> string

  val newReg : unit -> register 	(* newClass GP *)
  val newFreg : unit -> register	(* newClass FP *)
  val newCCreg : unit -> register	(* newClass CC *)

  val firstPseudo : register
  val zero : cellclass -> register option 
       (* name of the register that contains zero *)

  val resetRegs : unit -> regmap (* reset any local state *)

  type cellset = register list * register list
  val cellset2string : cellset -> string
  val empty	     : cellset
  val addCell        : cellclass -> register * cellset -> cellset
  val cellsetToRegs  : regmap * cellset -> register list

  val addReg  : register * cellset -> cellset (* addCell GP *)
  val addFreg : register * cellset -> cellset (* addCell FP *)
end
