(*$import *)


(* =========================================================================
 * REGISTER_MAP.sig.sml
 * ========================================================================= *)

signature REGISTER_MAP = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Identifies a register.
   *)
  type id

  (*
   * Maps register ids to arbitrary values.
   *)
  type 'a map

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a new, empty map based on a given default value function.
   * default -> the default value function (called once when undefined)
   * <- the new map
   *)
  val map: (id -> 'a) -> 'a map

  (*
   * Return the value that a given register is mapped to in a given map.
   * map -> the map to look the register up in
   * id	 -> the register to look up
   * <- the value of id in map
   *)
  val lookup: 'a map -> id -> 'a
  val test:   'a map -> id -> 'a option

  (*
   * Map a given register to a given value in a given map.
   * map  <-> the map to map the register in
   * id	   -> the register to map
   * value -> the value to map id to in map
   *)
  val insert: 'a map -> id * 'a -> unit

  (*
   * Remove all the register mappings from a given map.
   * map <-> the map to reset
   *)
  val reset: 'a map -> unit

end

