
(* =========================================================================
 * REGISTER_ALLOCATION.sig.sml
 * ========================================================================= *)

signature REGISTER_ALLOCATION = sig

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
   * An MLRISC cluster type.
   *)
  type cluster

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Set the spill and reload lookup functions for the next cluster to have
   * registers allocated to it.
   * lookupSpill, lookupReload -> the spill and reload lookup functions
   *)
  val setLookup: (id -> offset) * (id -> offset) -> unit

  (*
   * Allocate physical registers to the pseudo-registers of a given cluster
   * and return a new cluster representing the allocation.
   * cluster -> the cluster to allocate registers for
   * <- the allocated cluster
   *)
  val allocateCluster: cluster -> cluster

end

