(*$import TopLevel *)


(* =========================================================================
 * REGISTER_SPILL_MAP.sig.sml
 * ========================================================================= *)

signature REGISTER_SPILL_MAP = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register.
   *)
  type id

  (*
   * Designates a location on the stack.
   *)
  type offset

  (*
   * Maps register ids to spill/reload offsets.
   *)
  type map

  (* -- exceptions --------------------------------------------------------- *)

  (*
   * Raised when no spill/reload location is found for a given register id.
   *)
  exception NotMapped of id

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a new, empty map.
   * <- the new map
   *)
  val map: unit -> map

  (*
   * Return the spill and reload offsets of a given register in a given map.
   * map <-> the map to look the register up in
   * id	  -> the register to look up
   * <- the offsets of id in map
   *)
  val lookup:	    map -> id -> offset * offset
  val lookupSpill:  map -> id -> offset
  val lookupReload: map -> id -> offset

  val test:	  map -> id -> (offset * offset) option
  val testSpill:  map -> id -> offset option
  val testReload: map -> id -> offset option

  (*
   * Map a given register to given spill and reload offsets in a given map.
   * map    <-> the map to map the register in
   * id	     -> the register to map
   * offsets -> the offsets to map id to in map
   *)
  val insert: map -> id * (offset * offset) -> unit

  (*
   * Defer mapping a given set of registers by specifying a deferral function
   * in a given map.
   * map      <-> the map to defer mapping the registers in
   * predicate -> a predicate describing the register set
   * deferral  -> the function to call to produce the spill and reload offsets
   *)
  val defer: map -> (id -> bool) * (id -> offset * offset) -> unit

  (*
   * Remove all the register spill/reload mappings in a given map.
   * map <-> the map to reset
   *)
  val reset: map -> unit

end

