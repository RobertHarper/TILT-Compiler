(* =========================================================================
 * REGISTER_TRACE_MAP.sig.sml
 * ========================================================================= *)

signature REGISTER_TRACE_MAP = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a source register of a given type.
   *)
  type var

  (*
   * Identifies a target register of a given type.
   *)
  type id

  (*
   * Denotes the representation of a given source register.
   *)
  type rep

  (*
   * Denotes the traceability status of a given target register.
   *)
  type trace

  (*
   * Identifies a source register and its representation.
   *)
  type register = var * rep

  (*
   * Designates a location on the stack.
   *)
  type stacklocation

  (*
   * A set of register ids.
   *)
  type idSet

  (*
   * Maps source registers to target register ids and retains traceability
   * information.
   *)
  type map

  (* -- exceptions --------------------------------------------------------- *)

  (*
   * Raised when malformed source code is encountered.
   *)
  exception InvalidSource of string

  (*
   * Raised when no representation is found for a given target register id in
   * the traceability map.
   *)
  exception NotMapped of id

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a new, empty map.
   * <- the new map
   *)
  val map: unit -> map

  (*
   * Return the target id that a given source register is mapped to in a
   * given map.
   * lookup   -> the lookup function to use for registers in polymorphic values
   * map     <-> the map to look the register up in
   * register -> the register to look up
   * <- the target id of register in map
   *)
  val lookup: (register -> id) -> map -> register -> id

  (*
   * Assign a given target register id a given trace value.
   * map  <-> the map to assign the id in
   * is	   -> the id of the register to assign
   * trace -> the trace value to assign to id
   *)
  val assign: map -> id * trace -> unit

  (*
   * Return the trace value mapping function of a given map.
   * spill -> the spill function to use for registers in polymorphic values
   * map   -> the map to return the trace value mapping function of
   * <- a function mapping target register ids to trace values
   *)
  val trace: (id -> stacklocation) -> map -> id -> trace

  (*
   * Return the polymorphic spill set for a given map and a given set of
   * register ids.
   * map -> the map to trace the register ids in
   * ids -> the register ids to return the polymorphic spill set of
   * <- the ids that must be spilled for polymorphic values
   *)
  val polySpills: map -> idSet -> idSet

  (*
   * Remove all the source-based register mappings from a given map.
   * map <-> the map to reset
   *)
  val resetSource: map -> unit

  (*
   * Remove all the target-based register mappings from a given map.
   * map <-> the map to reset
   *)
  val resetTarget: map -> unit

  (*
   * Translate the representation of a global value to a trace value.
   * represent -> the representation to translate
   * <- the equivalent trace value
   *)
  val traceGlobalRepresent: rep -> trace

end

