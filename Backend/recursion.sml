functor Recursion(structure Pprtl : PPRTL
		  structure Graph : DIRECTEDGRAPH
		  structure Printutils : PRINTUTILS
		    sharing type Graph.node = Pprtl.Rtl.var) : RECURSION =
struct

  structure Rtl = Pprtl.Rtl
  open Printutils

  val error = fn s => Util.error "recursion.sml" s
  val debug = ref false

  fun print_rtl_locallabel ll = print (Pprtl.local_label2s ll)
  fun print_rtl_var v = print (Pprtl.var2s v)

  fun print_node n = if (! debug) 
		       then print_rtl_var n
		     else ()

  fun extractVarFromCodeLabel (Rtl.LOCAL_CODE v) = v
    | extractVarFromCodeLabel _ = error "extractvarfromcodelabel: locallabel not a code_label"

  (* Given an Rtl module, return:
     - a mapping taking a function name to the functions it calls;
     - the list of strongly-connected components in the call graph;
     - the list of connected (viewing the call graph as undirected)
       components only considering the tailcall edges. *)     
  fun procGroups module =
    let
      val callgraph     = Graph.empty (Name.fresh_var ())
      val tailcallgraph = Graph.empty (Name.fresh_var ())

      (* Add edge to the callgraph if the instruction is a call *)
      fun addCall proc (Rtl.CALL{func=Rtl.LABEL' (Rtl.LOCAL_LABEL l), 
				 ...}) =
	     (if (! debug) then
		(emitString "Recording call from ";
		 print_rtl_locallabel proc;
		 emitString " to ";
		 print_rtl_locallabel l;
		 emitString "\n") else ();
	      Graph.insert_edge(callgraph, (extractVarFromCodeLabel proc, 
					    extractVarFromCodeLabel l)))
        | addCall _ _ = ()

      (* Add edge to the tailcall graph if the instruction is 
         a tailcall *)
      fun addTailCall proc (Rtl.CALL{func=Rtl.LABEL' (Rtl.LOCAL_LABEL l), 
				     tailcall=true, ...}) =
	     (Graph.insert_edge(tailcallgraph, (extractVarFromCodeLabel proc, 
						extractVarFromCodeLabel l));
	      Graph.insert_edge(tailcallgraph, (extractVarFromCodeLabel l, 
						extractVarFromCodeLabel proc)))
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
	  app (fn proc => Graph.insert_node(callgraph, extractVarFromCodeLabel proc)) procnames;
	  app (fn proc => Graph.insert_node(tailcallgraph, extractVarFromCodeLabel proc)) procnames;
	  procLoop addCall procs; 
	  procLoop addTailCall procs
	end
	
      val ghash   = Graph.hash callgraph
      val gunhash = Rtl.LOCAL_CODE o (Graph.unhash callgraph)

      (* Create the graphs *)
      val _ = makeGraphs module;

      val rtl_scc = map (map gunhash) 
	(Graph.sc_components print_node callgraph)

      val rtl_tailcall_cc = map (map gunhash)
	(Graph.sc_components print_node tailcallgraph)
    in

      if (! debug) then
	(emitString "\nCallgraph:\n";
	 app (fn (n : Rtl.var) =>
	      let
		val edges = map gunhash (Graph.edges callgraph (ghash n))
	      in
		print_rtl_var n;
		emitString " : ";
		print_list print_rtl_locallabel edges
	      end) 
	     (Graph.nodes callgraph))
      else ();

      if (! debug) then
	(emitString "Connected components: \n";
	 app (print_list print_rtl_locallabel) rtl_scc)
      else ();

      (* Return info *)
      {callee_map = fn n => map gunhash
       (Graph.edges callgraph (ghash (extractVarFromCodeLabel n))),
       rtl_scc    = rtl_scc,
       rtl_tailcall_cc = rtl_tailcall_cc}
    end

end












