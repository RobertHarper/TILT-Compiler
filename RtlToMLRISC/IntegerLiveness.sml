
(* =========================================================================
 * IntegerLiveness.sml
 * ========================================================================= *)

functor IntegerLiveness(
	  structure BasicBlock:	     BASIC_BLOCK
	  structure IntegerDataFlow: REGISTER_DATA_FLOW
	  structure IntSet:	     ORD_SET where type Key.ord_key = int
	  structure MLTreeExtra:     MLTREE_EXTRA

	  sharing type MLTreeExtra.MLTree.mltree =
		       BasicBlock.mltree =
		       IntegerDataFlow.mltree
	      and type IntSet.set =
		       IntegerDataFlow.set
	) :> REGISTER_LIVENESS
	       where type id	 = int
		 and type mltree = MLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type id = int

  type mltree = MLTree.mltree

  type block = mltree list

  (* -- list functions ----------------------------------------------------- *)

  (*
   * Return whether or not a given predicate is true for any element of a list
   * and its index.
   * f	   -> the predicate to test over the list
   * index -> the index of the first element
   * list  -> the list to test f over
   * <- true if any element of list satisifies f
   *)
  fun existsIndex _ (_, nil) =
	false
    | existsIndex f (index, head::tail) =
	if f(index, head) then true else existsIndex f (index+1, tail)

  (* -- functions ---------------------------------------------------------- *)

  local
    (*
     * The is a naive adaptation of the iterative live-variable analysis
     * algorithm given in Aho, Sethi, and Ullman, page 631.  A better
     * implementation might use bit vectors to represent the register sets.
     *)

    (*
     * Return a list of (use, define, successors) block triples for a given
     * list of basic blocks.
     *)
    local
      fun triples1(block, successors) = (IntegerDataFlow.use_ block,
					 IntegerDataFlow.define block,
					 successors)
    in
      fun triples blocks =
	    ListPair.map triples1 (blocks, BasicBlock.successors blocks)
    end

    (*
     * Return the in and out sets of a given block triple according to a given
     * in set lookup function.
     *)
    fun liveness1 lookup (use, define, successors) =
	  let
	    val out = foldr IntSet.union IntSet.empty (map lookup successors)
	    val in_ = IntSet.union(use, IntSet.difference(out, define))
	  in
	    (in_, out)
	  end

    (*
     * Return whether or not the size of a given in set has increased.
     *)
    fun changed lookup (index, (in_, _)) =
	  IntSet.numItems in_>IntSet.numItems(lookup index)

    (*
     * Return the liveness of a given set of block triples according to a
     * given in set lookup function.
     *)
    fun liveness' triples lookup =
	  let
	    val sets = map (liveness1 lookup) triples
	  in
	    if existsIndex (changed lookup) (0, sets) then
	      let
		val vector = Vector.fromList sets

		fun lookup' index = #1(Vector.sub(vector, index))
	      in
		liveness' triples lookup'
	      end
	    else
	      sets
	  end

    (*
     * Return a list of the registers that are in the intersection of an in
     * and an out set.
     *)
    fun result sets = IntSet.listItems(IntSet.intersection sets)
  in
    fun liveness blocks =
	  map result (liveness' (triples blocks) (fn _ => IntSet.empty))
  end

end

