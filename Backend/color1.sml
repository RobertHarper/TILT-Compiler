(* Original implementation.  Does not try cycling through temporary regs. *)

functor Color1(structure Ifgraph : IFGRAPH
	       structure Trackstorage : TRACKSTORAGE
	       structure MU : MACHINEUTILS
	       structure Printutils : PRINTUTILS

	       sharing MU = Trackstorage.Machineutils
	                  = Printutils.Machineutils
	                  
	       sharing MU.Machine = Trackstorage.Machine 
	                          = Printutils.Machine

	       sharing type Ifgraph.node = MU.Machine.register) : COLOR =
struct
  structure Ifgraph = Ifgraph
  structure Trackstorage = Trackstorage
  structure MU = MU

  open MU MU.Machine Printutils

  val debug = ref false

   (* Color the interference graph *)

   fun simplify_graph_time f = Stats.timer("simplify graph",f)
   fun select_colors_time f = Stats.timer("select colors",f)


   fun reset_times () = Stats.reset_stats()
   fun print_times () = Stats.print_stats()

   (* returns a mapping from all pseudo-registers used in a procedure
      to their locations at run-time.   This mapping excludes
      physical registers.*)


   val num_iregs = length general_iregs
   val num_fregs = length general_fregs


   fun color(igraph,info, stack_resident : stacklocation Regmap.map, getBias) =

    let val stackOffset = Trackstorage.stackOffset info
        exception TrySpill

       fun rlookup [] _ = NONE
         | rlookup ((reg, bias) :: rest) reg' =
	   if (eqRegs reg reg') then (SOME bias) else (rlookup rest reg')
	     

       fun getBias' pseudoreg physicalreg =
	      case rlookup (getBias pseudoreg) physicalreg of
		 NONE => 0
	       | SOME bias => bias

	(* make a local copy of the interference graph 
	   for use in the simplify phase *)

        val g = Ifgraph.copy igraph
	val degree = Ifgraph.degree g
	val delete_node = Ifgraph.delete_node g

       (* first delete all stack-resident variables from graph.*)

        val _ = Regmap.appi (fn (key,_) =>
			      delete_node key
			      handle x => 
				(print ("error deleting stack resident var"^MU.Machine.msReg key);
				 raise x)) stack_resident
	          

       (* invariant for simplify: physical registers must not 
	  be returned by the nodes function on interference graphs.*)
 
       fun simplify node_stack =
	  case (Ifgraph.nodes_excluding_physical g)
	  of nil => node_stack
	   | l as (h :: _)=>
	       let fun loop [] = []
		     | loop (node :: nodes) =
		          if isPhysical node then loop nodes
			  else
		             let val count = degree node
			         val remove =
				   case node
				   of R _ => count < num_iregs
				    | F _ => count < num_fregs
			     in if remove 
			        then node :: loop nodes
			        else loop nodes
			     end
		    (* get low-degree nodes which can be removed *)
	            val next_nodes = loop l

		    (* if there aren't any, then spill pseudo-randomly *)

		    val next_nodes =
			  case next_nodes 
			  of [] => [h]
			   | _ => next_nodes 
	   in app delete_node next_nodes;
	      simplify  (next_nodes @ node_stack)
	   end

           val simplify = simplify_graph_time simplify 

       fun select mapping [] = mapping
	 | select mapping (node :: nodes) =
	   let
	     val _ = if (!debug) then
	       (emitString "select: "; print_list print_reg [node]) else ()

	     val neighbors = Ifgraph.edges igraph node
	     val neighborcolors =
	       let
		 fun loop [] = []
		   | loop (nbr :: nbrs) =
		       if isPhysical nbr then nbr :: loop nbrs
		       else case Regmap.find (mapping, nbr) of
			       SOME (IN_REG r) => r :: loop nbrs
			     | _ => loop nbrs
	       in
		 listToSet(loop neighbors)
	       end

	     fun bestReg pseudoreg [] bias' reg' = reg'
               | bestReg pseudoreg (reg::rest) bias' reg' =
		 let
		   val bias = getBias' pseudoreg reg

		   val _ = if (! debug) then
		     (emitString "conisdering ";
		      print_reg reg;
		      emitString " for variable ";
		      print_reg pseudoreg;
		      emitString "(bias = ";
		      print_int bias;
		      emitString ") best previously was ";
		      print_reg reg';
		      emitString " (bias =";
                      print_int bias';
		      emitString ")\n") else ()

		 in
		   if (bias > bias') then
		     bestReg pseudoreg rest bias reg
		   else if (bias = bias') andalso 
		           (regNum reg < regNum reg') then
			   bestReg pseudoreg rest bias reg
			else
			  bestReg pseudoreg rest bias' reg'
		 end

	     val regs_available = 
	       (case node of 
		  (R _) => 
		    Regset.listItems
		    (Regset.difference(listToSet general_iregs, neighborcolors))
		| (F _) =>
		    Regset.listItems
		    (Regset.difference(listToSet general_fregs, neighborcolors)))

	     val chosen_reg = 
	       if (length regs_available = 0) then
		 NONE
	       else
		 SOME (bestReg node regs_available ~1000000 node)

	   in
	     case chosen_reg of
	       SOME r => 
		 (if (! debug) then 
		    (emitString "Allocating "; print_reg r;
		     emitString " for pseudoregister "; print_reg node;
		     emitString "\n") else ();
		  Trackstorage.noteUsed info r;
		  select (Regmap.insert(mapping, node, IN_REG r)) nodes)
	     | NONE =>
		 (if (! debug) then
		    (emitString "Allocating pseudoregister "; print_reg node;
		     emitString " on stack\n") else ();
		  select (Regmap.insert(mapping, node,
				       ON_STACK (stackOffset node))) nodes)
           end (* select *)

           val select = select_colors_time select 

	val node_stack = simplify []

        (* color the local registers *)

	val t = Regmap.map ON_STACK stack_resident
        val mapping = select t node_stack

     in mapping
     end
end (* struct *)
