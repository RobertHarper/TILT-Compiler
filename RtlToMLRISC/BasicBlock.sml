(* =========================================================================
 * BasicBlock.sml
 * ========================================================================= *)

functor BasicBlock(
	  structure MLRISCPseudo: MLRISC_PSEUDO
	  structure MLTreeExtra:  MLTREE_EXTRA

	  sharing type MLRISCPseudo.pseudo_op =
		       MLTreeExtra.MLTree.PseudoOp.pseudo_op
	) :> BASIC_BLOCK
	       where type mltree = MLTreeExtra.MLTree.mltree
	  = struct

  (* -- structures --------------------------------------------------------- *)

  structure IntMap = IntBinaryMap
  structure IntSet = DenseIntSet

  structure MLTree = MLTreeExtra.MLTree

  (* -- types -------------------------------------------------------------- *)

  type mltree = MLTree.mltree

  type block = mltree list

  (* -- exceptions --------------------------------------------------------- *)

  exception InvalidSource of string

  exception UndefinedLabel of string

  (* -- list functions ----------------------------------------------------- *)

  (*
   * Return the result of mapping a given function over the elements of a
   * given list and their indices.
   * f	   -> the function to map over the list
   * index -> the index of the first element
   * list  -> the list to map f over
   * <- the result of mapping f over list
   *)
  fun mapIndex _ (_, nil) =
	nil
    | mapIndex f (index, head::tail) =
	f(index, head)::mapIndex f (index+1, tail)

  (*
   * Apply a given function to the elements of a given list and their indices.
   * f	   -> the function to apply to the list
   * index -> the index of the first element
   * list  -> the list to apply f to
   *)
  fun appIndex _ (_, nil) =
	()
    | appIndex f (index, head::tail) =
	(f(index, head); appIndex f (index+1, tail))

  (* -- functions ---------------------------------------------------------- *)

  local
    (*
     * Add the branch targets in a given statement to a given set of labels.
     *)
    fun targetsStatement(MLTree.JMP(_, labels'), labels) =
	  IntSet.addList(labels, map Label.id labels')
      | targetsStatement(MLTree.BCC(_, _, label), labels) =
	  IntSet.add(labels, Label.id label)
      | targetsStatement(MLTree.FBCC(_, _, label), labels) =
	  IntSet.add(labels, Label.id label)
      | targetsStatement(_, labels) =
	  labels

    (*
     * Add the branch targets in a given statement/directive to a given set of
     * labels.
     *)
    fun targetsTree(MLTree.CODE code, labels) =
	  foldr targetsStatement labels code
      | targetsTree(_, labels) =
	  labels

    (*
     * Return a predicate which identifies the branch target labels in a given
     * list of statement/directives.
     *)
    fun targetLabels mltrees =
	  let
	    val set = foldr targetsTree IntSet.empty mltrees
	  in
	    fn label => IntSet.member(set, Label.id label)
	  end
  in
    fun keepTargetLabels mltrees =
	  let
	    val labels = targetLabels mltrees

	    fun keep(MLTree.DEFINELABEL label) = labels label
	      | keep _			       = true
	  in
	    List.filter keep mltrees
	  end
  end

  local
    fun code1 statement = MLTree.CODE[statement]

    (*
     * Divide a given statement/directive from a given sequence of basic
     * blocks.
     *)
    fun divide(mltree, blocks) = [mltree]::blocks

    (*
     * Merge a given statement/directive into a given sequence of basic blocks,
     * if appropriate.
     *)
    fun merge(mltree, nil) =
	  [[mltree]]
      | merge(mltree, blocks as ((MLTree.PSEUDO_OP(
				    MLRISCPseudo.CallSite _)::_)::_)) =
	  divide(mltree, blocks)
      | merge(mltree, blocks as ((MLTree.DEFINELABEL _::_)::_)) =
	  divide(mltree, blocks)
      | merge(MLTree.CODE code, (MLTree.CODE code'::tail')::tail) =
	  (MLTree.CODE(code@code')::tail')::tail
      | merge(mltree, block::tail) =
	  (mltree::block)::tail

    (*
     * Merge or divide a given statement with a given sequence of basic
     * blocks, as appropriate.
     *)
    fun partitionStatement(statement as MLTree.JMP _, blocks) =
	  divide(code1 statement, blocks)
      | partitionStatement(statement as MLTree.CALL _, blocks) =
	  divide(code1 statement, blocks)
      | partitionStatement(statement as MLTree.BCC _, blocks) =
	  divide(code1 statement, blocks)
      | partitionStatement(statement as MLTree.FBCC _, blocks) =
	  divide(code1 statement, blocks)
      | partitionStatement(statement, blocks) =
	  merge(code1 statement, blocks)

    (*
     * Merge or divide a given statement/directive with a given sequence of
     * basic blocks, as appropriate.
     *)
    fun partitionTree(MLTree.CODE code, blocks) =
	  foldr partitionStatement blocks code
      | partitionTree(MLTree.BEGINCLUSTER, _) =
	  raise InvalidSource "basic blocks cannot cross clusters"
      | partitionTree(MLTree.ENDCLUSTER _, _) =
	  raise InvalidSource "basic blocks cannot cross clusters"
      | partitionTree operands =
	  merge operands
  in
    val partition = foldr partitionTree []
  end

  local
    (*
     * Return a function mapping labels to basic block indices for a given list
     * of basic blocks.
     *)
    local
      (*
       * Return a map of the label indices heading a given basic block
       * sequence, starting with a given index.
       *)
      fun mapLabels(nil, _) =
	    IntMap.empty
	| mapLabels(block::tail, index) =
	    let
	      val map = mapLabels(tail, index+1)
	    in
	      case block of
		MLTree.DEFINELABEL label::_ =>
		  IntMap.insert(map, Label.id label, index)
	      | _ =>
		  map
	    end
    in
      fun indexLabels blocks =
	    let
	      val map = mapLabels(blocks, 0)
	    in
	      fn label =>
		case IntMap.find(map, Label.id label) of
		  SOME index => index
		| NONE	     => raise UndefinedLabel(Label.nameOf label)
	    end
    end

    (*
     * Return the successor list of a given basic block based on a given label
     * index function.
     *)
    local
      (*
       * Adjust a given successor list according to a given statement.
       *)
      fun successorsStatement index (MLTree.JMP(_, labels), _) =
	    map index labels
	| successorsStatement _ (MLTree.RET, _) =
	    []
	| successorsStatement index (MLTree.BCC(_, _, label), list) =
	    index label::list
	| successorsStatement index (MLTree.FBCC(_, _, label), list) =
	    index label::list
	| successorsStatement _ (_, list) =
	    list

      (*
       * Adjust a given successor list according to a given
       * statement/directive.
       *)
      fun successorsTree index (MLTree.CODE code, list) =
	    foldr (successorsStatement index) list code
	| successorsTree _ (_, list) =
	    list
    in
      fun successorsBlock index = foldr (successorsTree index)
    end
  in
    fun successors blocks =
	  let
	    val successorsBlock' = successorsBlock(indexLabels blocks)

	    fun successors1(index, block) = successorsBlock' [index+1] block
	  in
	    mapIndex successors1 (0, blocks)
	  end

    fun predecessors blocks =
	  let
	    val array = Array.array(length blocks, []: int list)

	    fun insert source target =
		  Array.update(array, target, source::Array.sub(array, target))

	    fun insert'(index, successors) = app (insert index) successors
	  in
	    appIndex insert' (0, successors blocks);
	    Array.foldr op:: nil array
	  end
  end

end

