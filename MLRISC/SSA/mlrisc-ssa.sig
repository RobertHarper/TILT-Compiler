(* 
 * SSA graph.
 * The node id's in this graph are indexed by ssa variables.
 * If a node has multiple definitions r1,...,rn, then each of the ri
 * refers to the same ssa op. 
 *)
signature SSA =
sig

   structure I   : INSTRUCTIONS
   structure C   : CELLS
   structure CFG : CONTROL_FLOW_GRAPH
   structure Dom : DOMINATOR_TREE
   structure SP  : SSA_PROPERTIES
      sharing CFG.I = SP.I = I
      sharing I.C = SP.C = C 

   type value = int             (* value id *)
   type pos = int               (* position within a block *)
   type block = Graph.node_id   (* block id *)
   type exp = SSAExp.exp        (* SSA expression *)
   type const = SP.const
   type dom = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree

   (*
    * An SSA op can be either a phi node, a normal instruction,
    * a source node or a sink node.
    * For source/sink instructions, also keep around the original register
    * names.
    *)
   datatype ssa_op = 
      PHI of    {preds:block list,t':C.register,t:value,s:value list,b:block}
   |  OP  of    {e:exp,i:I.instruction,s:value list,t:value list,b:block,p:pos}
   |  SOURCE of {t:value list, t':C.register list, b:block}
   |  SINK of   {s:value list, s':C.register list, b:block}

   type info 
   type ssa = (ssa_op,value,info) Graph.graph

   val newSSA    : CFG.cfg * (CFG.cfg -> dom) -> ssa  
                                              (* create an empty  SSA graph *)
   val newVar    : ssa -> C.cellclass -> value  (* generate new variable *)
   val newRenamedVar : int Array.array option ->
                       ssa -> value -> value    (* generate renamed variable *)
   val immed     : ssa -> int -> value          (* create a new immed value *)
   val label     : ssa -> Label.label -> value  (* create a new label operand*)
   val operand   : ssa -> I.operand -> value    (* create a new operand *)

   val dom       : ssa -> dom                 (* extracts the dominator *)
   val cfg       : ssa -> CFG.cfg             (* extracts the CFG *)
   val maxVar    : ssa -> int                 (* maximum number of ssa names *)
   val operands  : ssa -> int                 (* number of operands *)
   val const     : ssa -> value -> const      (* lookup const values *)
   val defSite   : ssa -> value -> Graph.node_id (* lookup definition site *)
   val cellClass : ssa -> value -> C.cellclass   (* lookup cell class *)
   val updateCellClass : ssa -> value * C.cellclass -> unit

       (* linearize the SSA *)
   val nodes     : ssa -> { source : ssa_op Graph.node list,
                            phis   : ssa_op Graph.node list,
                            ops    : ssa_op Graph.node list,
                            sink   : ssa_op Graph.node list
                          } Array.array
   (*
    * Replace all use of one value with another.  Return true iff
    * this operation is successful. 
    * Note: The definition of "from" must dominate all uses of "to", as
    * required by the SSA form.
    *)  
   val replaceAllUses : ssa -> {from:value, to:value} -> bool

   (*
    *  Replace the value of 'from' by a copy of value of 'to'
    *)
   val replaceByCopy : ssa -> {from:value, to:value} -> bool
 
   (*
    * Replace the definition of value by const.  Return true iff
    * this operation is successful.
    *)
   val foldConstant : ssa -> {value:value, const:value} -> bool

   (*
    * Move an instruction from one block to another
    *) 
   val move : ssa -> {i:ssa_op Graph.node, block:block} -> unit

   (*
    * Set the target of a conditional branch as true or false.
    * This removes the branch and eliminates all unreachable code.
    *)
   val setBranch : ssa -> {jmp:ssa_op Graph.node, cond:bool} -> unit

   (* 
    * Signal that an SSA has been changed
    *)
   val changed : ssa -> unit

   (*  
    * Graph viewing and pretty printing 
    *)
   val show_op    : ssa -> ssa_op -> string
   val show_val   : ssa -> value -> string
   val viewAsSSA  : ssa -> GraphLayout.layout
   val viewAsCFG  : ssa -> GraphLayout.layout

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:43  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:14  pscheng
# *** empty log message ***
#
 *)
