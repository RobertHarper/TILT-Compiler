(* Original implementation.  Does not try cycling through temporary regs. *)

functor Color1(structure Ifgraph : IFGRAPH
	       structure Trackstorage : TRACKSTORAGE
	       structure MU : MACHINEUTILS
	       structure Printutils : PRINTUTILS

	       sharing MU = Trackstorage.Machineutils
	                  = Printutils.Machineutils
	                  
	       sharing MU.Machine = Trackstorage.Machine 
	                          = Printutils.Machine

               sharing Ifgraph.Regset = MU.Regset
	       sharing type Ifgraph.node = MU.Machine.register) : COLOR = 
struct
  structure Ifgraph = Ifgraph
  structure Trackstorage = Trackstorage
  structure MU = MU

  open MU MU.Machine Printutils

  val debug = ref false

   (* Color the interference graph *)

   fun simplify_graph_time f = Stats.subtimer("simplify graph",f)
   fun select_colors_time f = Stats.subtimer("select colors",f)


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
       let val nodes = Ifgraph.nodes_excluding_physical g
       in  if (Regset.isEmpty nodes) then node_stack
	   else
	       let 
		   fun folder (node,acc as (good,bad)) =
		          if isPhysical node then acc
			  else
		             let val count = degree node
				 val isInt = (case node of R _ => true | F _ => false)
			         val isGood = count < (if isInt then num_iregs else num_fregs)
			         val isBad = count > (if isInt then 4*num_iregs else 4*num_fregs)
			     in  (if isGood then node::good else good,
				  if isBad then node::bad else bad)
			     end
		    (* get low-degree nodes which can be removed *)
	            val (good_nodes, bad_nodes) = Regset.foldl folder ([],[]) nodes

		    (* if there aren't any, then spill pseudo-randomly *)

		    val random_nodes = 
			  (case (good_nodes,bad_nodes) of
			   ([],[]) => [valOf(Regset.find (fn _ => true) nodes)]
			   | _ => [])

		val _ = (print "(GOOD,BAD,RANDOM) = ";
			print (Int.toString (length good_nodes)); print ", ";
			print (Int.toString (length bad_nodes)); print ", ";
			print (Int.toString (length random_nodes)); print "\n")

	   in app delete_node good_nodes;
	      app delete_node bad_nodes;
	      app delete_node random_nodes;
	      simplify  (random_nodes @ bad_nodes @ good_nodes @ node_stack)
	   end
       end
       val simplify = simplify_graph_time simplify 

       fun select mapping [] = mapping
	 | select mapping (node :: nodes) =
	   let
	     val _ = if (!debug) then
	       (emitString "select: "; print_list print_reg [node]) else ()

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

	     val neighbors = Ifgraph.edges igraph node

	     val chosen_reg = 
	       if (Regset.numItems neighbors > 128) then NONE
	       else 
	         let 
		   fun folder (nbr,acc) =
		       if isPhysical nbr then Regset.add(acc,nbr)
		       else case Regmap.find (mapping, nbr) of
			       SOME (IN_REG r) => Regset.add(acc,r)
			     | _ => acc
		   val neighborcolors = Regset.foldl folder Regset.empty neighbors

	           val regs_available = 
		       (case node of 
			  (R _) => 
			    Regset.listItems
			    (Regset.difference(listToSet general_iregs, neighborcolors))
			| (F _) =>
			    Regset.listItems
			    (Regset.difference(listToSet general_fregs, neighborcolors)))

		 in
	           if (length regs_available = 0) then NONE
	           else SOME (bestReg node regs_available ~1000000 node)
                 end

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
				       ON_STACK (stackOffset (Regset.listItems neighbors,
								node)))) nodes)
          end (* select *)

        val select = select_colors_time select 

	val node_stack = simplify []

        (* color the local registers *)

	val t = Regmap.map ON_STACK stack_resident
        val mapping = select t node_stack

     in mapping
     end
end (* struct *)
