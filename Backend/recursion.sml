(*$import Pprtl GRAPH PRINTUTILS RECURSION Util *)

functor Recursion(structure Printutils : PRINTUTILS)
    :> RECURSION =
struct

  structure Graph = Labelgraph
  open Printutils

  val error = fn s => Util.error "recursion.sml" s
  val debug = ref false

  fun print_rtl_label ll = print (Pprtl.label2s ll)
  fun print_rtl_var v = print (Pprtl.var2s v)

  fun print_node n = if (! debug) 
		       then print_rtl_label n
		     else ()


  (* Given an Rtl module, return:
     - a mapping taking a function name to the functions it calls;
     - the list of strongly-connected components in the call graph;
     - the list of connected (viewing the call graph as undirected)
       components only considering the tailcall edges. *)     
  fun isLocal(Rtl.LOCAL_CODE _) = true
    | isLocal(Rtl.LOCAL_DATA _) = true
    | isLocal _ = false

  fun procGroups module =
    let
      val callgraph     = Graph.empty (Rtl.fresh_code_label "dead")
      val tailcallgraph = Graph.empty (Rtl.fresh_code_label "dead")

      (* Add edge to the callgraph if the instruction is a call *)
      fun addCall proc (Rtl.CALL{func=Rtl.LABEL' l,
				 ...}) =
	     (if (! debug) then
		(emitString "Recording call from ";
		 print_rtl_label proc;
		 emitString " to ";
		 print_rtl_label l;
		 emitString "\n") else ();
		if isLocal l
		    then Graph.insert_edge(callgraph, (proc, l))
		else ())
        | addCall _ _ = ()

      (* Add edge to the tailcall graph if the instruction is 
         a tailcall *)
      fun addTailCall proc (Rtl.CALL{func=Rtl.LABEL' l,
				     call_type=ML_TAIL, ...}) =
	     if isLocal l
		 then (Graph.insert_edge(tailcallgraph, (proc, l));
		       Graph.insert_edge(tailcallgraph, (l, proc)))
	     else ()
	| addTailCall _ _ = ()

      fun procLoop f [] = ()
        | procLoop f ((Rtl.PROC{name, code, ...}) :: rest) = 
	  (Array.app (f name) code; procLoop f rest)

      (* Extract all the names of the Rtl functions *)
      fun procNames [] = []
        | procNames ((Rtl.PROC{name, ...}) :: rest) = 
	  name :: (procNames rest)

      (* Create the callgraph and the tail-callgraph *)
      fun makeGraphs (Rtl.MODULE{procs,...}) =
	let 
	  val procnames = procNames procs
	in
	  app (fn proc => Graph.insert_node(callgraph, proc)) procnames;
	  app (fn proc => Graph.insert_node(tailcallgraph, proc)) procnames;
	  procLoop addCall procs; 
	  procLoop addTailCall procs
	end
	
      val ghash   = Graph.hash callgraph
      val gunhash = Graph.unhash callgraph

      (* Create the graphs *)
      val _ = makeGraphs module;

      val rtl_scc = map (map gunhash) 
	(Graph.sc_components print_node callgraph)

      val rtl_tailcall_cc = map (map gunhash)
	(Graph.sc_components print_node tailcallgraph)
    in

      if (! debug) then
	(emitString "\nCallgraph:\n";
	 app (fn (n : Rtl.label) =>
	      let
		val edges = map gunhash (Graph.edges callgraph (ghash n))
	      in
		print_rtl_label n;
		emitString " : ";
		print_list print_rtl_label edges
	      end) 
	     (Graph.nodes callgraph))
      else ();

      if (! debug) then
	(emitString "Connected components: \n";
	 app (print_list print_rtl_label) rtl_scc)
      else ();

      (* Return info *)
      {callee_map = fn n => map gunhash
       (Graph.edges callgraph (ghash n)),
       rtl_scc    = rtl_scc,
       rtl_tailcall_cc = rtl_tailcall_cc}
    end

end












