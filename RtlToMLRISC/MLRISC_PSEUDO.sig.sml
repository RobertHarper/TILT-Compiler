
(* =========================================================================
 * MLRISC_PSEUDO.sig.sml
 * ========================================================================= *)

signature MLRISC_PSEUDO = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register.
   *)
  type id

  datatype pseudo_op =
    ModuleHeader
  | ModuleTrailer
  | TextHeader
  | TextTrailer
  | DataHeader
  | DataTrailer
  | TableHeader
  | TableTrailer
  | ProcedureHeader of Label.label
  | ProcedureTrailer of Label.label
  | Export of Label.label
  | CallSite of id list ref * (id list -> unit)
  | Align of int * int
  | Comment of string
  | Integer of Word32.word
  | IntegerFloatSize of Word32.word
  | Float of string
  | Label of Label.label
  | IntegerArray of int * Word32.word
  | FloatArray of int * string
  | LabelArray of int * Label.label
  | String of string

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a label string with special characters escaped.
   * string -> the label string to escape
   * <- the escaped label string
   *)
  val fixLabel: string -> string

  (*
   * Interfaces to MLRISC.
   *)
  val toString:	    pseudo_op -> string
  val emitValue:    {pOp: pseudo_op, loc: int, emit: Word8.word -> unit}
		      -> unit
  val sizeOf:	    pseudo_op * int -> int
  val adjustLabels: pseudo_op * int -> unit

end

