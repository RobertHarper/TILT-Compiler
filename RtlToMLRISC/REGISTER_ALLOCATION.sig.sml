
(* =========================================================================
 * REGISTER_ALLOCATION.sig.sml
 * ========================================================================= *)

signature REGISTER_ALLOCATION = sig

  (* -- types -------------------------------------------------------------- *)

  (*
   * Maps register ids to spill/reload offsets.
   *)
  type spillMap

  (*
   * An MLRISC cluster type.
   *)
  type cluster

  (* -- values ------------------------------------------------------------- *)

  (*
   * The spill/reload map for pseudo-registers.
   *)
  val spillMap: spillMap

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Allocate physical registers to the pseudo-registers of a given cluster
   * and return a new cluster representing the allocation.
   * cluster -> the cluster to allocate registers for
   * <- the allocated cluster
   *)
  val allocateCluster: cluster -> cluster

end

