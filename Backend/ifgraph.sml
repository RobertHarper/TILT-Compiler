(*$import Prelude TopLevel Word List Int MONO_HASH_TABLE Core MACHINE IFGRAPH Util HashTableFn SplaySetFn *)
(* interference graph *)

functor Ifgraph (structure Machine : MACHINE) :> IFGRAPH =
struct

    val error = fn s => Util.error "ifgraph.sml" s

    open Core
    open Machine

    fun canInterfere (R a) (R b) = a <> b
      | canInterfere (F a) (F b) = a <> b
      | canInterfere _ _ = false

    fun eqReg (R r1, R r2) = r1 = r2
      | eqReg (F r1, F r2) = r1 = r2
      | eqReg _ = false

     structure HashKey =
       struct
         type hash_key = register
	 val highbit = Word.<<(0w1, Word.fromInt(Word.wordSize - 1))
         fun hashVal (R v) = Word.fromInt v
           | hashVal (F v) = Word.orb(highbit, Word.fromInt v)
	 val sameKey = eqReg
       end
     structure A : MONO_HASH_TABLE = HashTableFn(HashKey)

     (* graph representation:
          size is the total number of nodes
	    all is the list of all nodes
	  graph is a hash table with one entry for each of the nodes
	    each entry contains a set of neighbors
	Invariant: A graph contains no physical nodes. *)

     type node = register
     datatype graph = GRAPH of {size : int ref,
				all : Regset.set ref,
				graph : Regset.set A.hash_table}

     (* set utilities *)

     fun single v = Regset.singleton v
     fun member (a,s) = Regset.member(s,a)
     fun delete (a,s) = Regset.delete(s,a)
     fun insert (a,s) = Regset.add(s,a)
     fun inserts (alist,s) = Regset.addList(s,alist)
     fun hash_member(k,ht) = (case A.find ht k of
				  NONE => false
				| SOME _ => true)

     (* graph operations *)
     val hint = 64
     exception AdjList
     fun empty () = GRAPH {size = ref 0,
			   all = ref(Regset.empty),
			   graph = A.mkTable(hint,AdjList)}

     fun nodes (GRAPH {all,...}) = !all
     fun nodes_excluding_physical (GRAPH {all,...}) = !all

     fun edges (GRAPH {graph,...}) n =
	 case A.find graph n of
	     NONE => error "node is not in graph"
	   | SOME l => l

     val degree = fn g => Regset.numItems o (edges g)

     fun insert_node (GRAPH {all,graph,size}) n =
	   if ((isPhysical n) orelse (hash_member(n,graph))) 
	       then ()
	   else (A.insert graph (n,Regset.empty);
		 all := Regset.add(!all,n);
		 size := (!size) + 1)


     (* remove the node entry from graph and remove the node from its neighbors's entries *)
     fun delete_from_graph graph node = 
	 let val neighbors = A.remove graph node handle AdjList => 
			      (print ("delete_node: AdjList:"^msReg node^"\n");
			       raise AdjList)
	     fun find k = (case A.find graph k of
			       SOME value => value
			     | _ => error "delete_from_graph: node does not exist")
	     fun apper n = if (isPhysical n)
			      then ()
		           else A.insert graph (n,delete(node,find n))
         in Regset.app apper neighbors
	 end

     fun delete_node (GRAPH {all,graph,size}) n =
	 (delete_from_graph graph n;
	  size := !size - 1;
	  all := delete(n,!all))

     fun copy (GRAPH {all,size,graph}) = 
	 GRAPH{graph = A.copy graph,
	       size = ref (!size),
	       all = ref (!all)}

     (* add an interference edge between 2 registers:
              . two registers interfere only if they're the same type
              . don't keep edge lists for physical registers
              . don't add an edge from a register to itself
      *)

     (* assumes a and b are different and are of the same type
        does not assume either are in the graph already *)
     fun add_edges_direct graph a nodes =
	     if isPhysical a 
		 then ()
	     else let val l = A.lookup graph a 
		      val nodes = List.filter (canInterfere a) nodes
		      val l' = inserts(nodes,l)
		  in  A.insert graph (a,l')
		  end
     fun add_edge_direct graph a b = add_edges_direct graph a [b]

     fun insert_edge (g as GRAPH{all,size,graph}) (a,b) =
	 (add_edge_direct graph a b;
	  add_edge_direct graph b a)


     fun insert_edges (g as GRAPH{all,size,graph}) ([node],nodeSet) =
	 let fun folder (r,acc) = r::acc
	     val nodeList = Regset.foldl folder [] nodeSet
	 in  add_edges_direct graph node nodeList;
	     app (fn b => add_edge_direct graph b node) nodeList
	 end
       | insert_edges (g as GRAPH{all,size,graph}) (nodeList,nodeSet) =
	 let
	     fun folder (r as R _, (iRegs, fRegs)) = (r::iRegs,fRegs)
	       | folder (r as F _, (iRegs, fRegs)) = (iRegs,r::fRegs)
	     val (inodeList1,fnodeList1) = List.partition isInt nodeList
	     val (inodeList2,fnodeList2) = Regset.foldl folder ([],[]) nodeSet
	 in  if (null inodeList2)
		 then () 
	     else (app (fn n => add_edges_direct graph n inodeList1) inodeList2;
		   app (fn n => add_edges_direct graph n inodeList2) inodeList1);

	     if (null fnodeList2)
		 then ()
	     else (app (fn n => add_edges_direct graph n fnodeList1) fnodeList2;
		   app (fn n => add_edges_direct graph n fnodeList2) fnodeList1)
	 end


       fun print_stats (GRAPH{all,size,graph}) =
	   let val edges = ref 0
	   in A.appi (fn (_,n) =>
		         (edges := !edges + (Regset.numItems n))) graph;
	      
	      print "IG stat:  (V,E,avg deg) = (";
	      print (Int.toString (!size)); print ", ";
	      print (Int.toString (!edges)); print ", ";
	      (if ((!size) = 0)
		 then print "undef"
	       else print (Int.toString ((!edges) div (!size))));
	      print ")\n"
	   end
end

