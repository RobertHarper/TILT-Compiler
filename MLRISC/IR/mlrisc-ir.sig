(*
 * MLRISC Internal IR
 * This is for performing whole program analysis.
 *)

signature MLRISC_IR =
sig

   structure I    : INSTRUCTIONS
   structure CFG  : CONTROL_FLOW_GRAPH
   structure Dom  : DOMINATOR_TREE
   structure CDG  : CONTROL_DEPENDENCE_GRAPH
   structure Loop : LOOP_STRUCTURE
   structure Util : CFG_UTIL
      sharing Util.CFG = CFG
      sharing CFG.I = I 
      sharing Loop.Dom = CDG.Dom = Dom
  
   type cfg  = CFG.cfg  
   type IR   = CFG.cfg  (* The IR looks just like a CFG! *)
   type dom  = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree
   type pdom = (CFG.block,CFG.edge_info,CFG.info) Dom.postdominator_tree
   type cdg  = (CFG.block,CFG.edge_info,CFG.info) CDG.cdg
   type loop = (CFG.block,CFG.edge_info,CFG.info) Loop.loop_structure
 
   (*
    *  Extract various views from an IR.
    *  These are computed by need.
    *) 
   val dom   : IR -> dom
   val pdom  : IR -> pdom
   val cdg   : IR -> cdg
   val loop  : IR -> loop

   (*
    *  Signal that the IR has been changed
    *) 
   val changed : IR -> unit  

   (*
    *  View as a picture  
    *) 
   val view : string -> IR -> unit       (* view some aspect of the IR *)
   val viewSubgraph : IR -> cfg -> unit  (* view a subgraph of the IR *)

   (*
    *  This function allows the client to design a new view and extend
    *  the functionality of the IR
    *) 
   val memo : (IR -> 'facet) -> IR -> 'facet

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:37  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:05  pscheng
# *** empty log message ***
#
 *)
