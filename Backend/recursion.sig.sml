(*$import Prelude Rtl *)

signature RECURSION =
sig

  val debug: bool ref

  (* Given an Rtl module, return:
     - a mapping taking a function name to the functions it calls;
     - the list of strongly-connected components in the call graph;
     - the list of components (considering the call graph as
       undirected) when only considering the tailcall edges. *)     

  val procGroups : Rtl.module -> 
                   {callee_map: Rtl.label -> Rtl.label list,
		    rtl_scc: Rtl.label list list,
		    rtl_tailcall_cc: Rtl.label list list}



end
