(*$import MACHINE IFGRAPH Util HashTableFn SplaySetFn *)
(* interference graph *)

(*

functor Ifgraph (structure Machine : MACHINE) :> IFGRAPH =

struct

    val badness_threshold = ref 128

    val error = fn s => Util.error "ifgraph.sml" s

    open Core
    open Machine

    fun eqReg (r1,r2) = eqRegs r1 r2


     structure HashKey =
       struct
         type hash_key = register
         fun hashVal (R v) = Word.fromInt v
           | hashVal (F v) = Word.fromInt v
	 val sameKey = eqReg
       end
     structure A : MONO_HASH_TABLE = HashTableFn(HashKey)

     (* graph representation:
          size is the total number of nodes
	    all is the list of all nodes
          bad is the set of nodes that interferes with all other nodes
	  graph is a hash table with one entry for each of the non-bad nodes
	    each entry contains a set of non-bad neighbors *)

     type node = register
     datatype graph = GRAPH of {size : int ref,
				bad : Regset.set ref,
				all : Regset.set ref,
				graph : Regset.set A.hash_table}

     (* set utilities *)

     fun single v = Regset.singleton v
     fun member (a,s) = Regset.member(s,a)
     fun delete (a,s) = Regset.delete(s,a)
     fun insert (a,s) = Regset.add(s,a)
     fun hash_member(k,ht) = (case A.find ht k of
				  NONE => false
				| SOME _ => true)

     (* graph operations *)
     val hint = 64
     exception AdjList
     fun empty () = GRAPH {size = ref 0,
			   bad = ref(Regset.empty),
			   all = ref(Regset.empty),
			   graph = A.mkTable(hint,AdjList)}

     fun nodes (GRAPH {all,...}) = !all
     fun nodes_excluding_physical (GRAPH {all,...}) = !all
     
     val num_general_iregs = length general_iregs
     val num_general_fregs = length general_fregs
     val general_iregs = Regset.addList(Regset.empty,general_iregs)
     val general_fregs = Regset.addList(Regset.empty,general_fregs)

     fun edges (GRAPH {bad,graph,all,...}) n =
	 case A.find graph n of
	     NONE => if (member(n,!bad)) 
			 then Regset.union(!all, (case n of
					 R _ => general_iregs
				       | F _ => general_fregs))
		     else Regset.empty
	   | SOME l => Regset.union(l,!bad)

     fun degree (GRAPH {all,bad,graph,size}) n =
	 case A.find graph n of
	     NONE => if (member(n,!bad)) 
			 then (!size-1+(case n of
					 R _ => num_general_iregs
				       | F _ => num_general_fregs))
		     else 0
	   | SOME neighbors => (Regset.numItems neighbors) + (Regset.numItems (!bad))

     fun insert_node (GRAPH {all,bad,graph,size}) n =
	  (if ((isPhysical n)
	       orelse (member(n,!bad))
	       orelse (hash_member(n,graph))) then ()
	   else (A.insert graph (n,Regset.empty);
		 all := Regset.add(!all,n);
		 size := (!size) + 1))


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

     fun delete_node (GRAPH {all,bad,graph,size}) n =
	 let val remove = A.remove graph
	     val insert = A.insert graph
	     fun find k = (case A.find graph k of
			       SOME value => value
			     | _ => error "delete_node: node does not exist")
	 in (if (member(n,!bad))
		   then bad := delete(n,!bad)
	     else delete_from_graph graph n);
	      size := !size - 1;
	      all := delete(n,!all)
         end

     (* add an interference edge between 2 registers:
              . two registers interfere only if they're the same type
              . don't keep edge lists for physical registers
              . don't add an edge from a register to itself
      *)

     fun copy (GRAPH {all,size,bad,graph}) = 
	 GRAPH{graph = A.copy graph,
	       size = ref (!size),
	       all = ref (!all),
	       bad = ref (!bad)}

     (* assumes a and b are different, 
                        are not bad, and are of the same type;
        does not assume either are in the graph already *)
     fun insert_edge_fast (g as GRAPH{all,size,bad,graph}) (a,b) =
	 (let 
	      val graph_insert = A.insert graph
	      fun add (a,b) = 
		  if isPhysical a 
		      then ()
		  else let val l = A.lookup graph a 
		      handle AdjList => 
			  (insert_node g a;
			   A.lookup graph a) 
			   val l' = insert(b,l)
		       in  graph_insert (a,l')
		       end
	      fun promote a = 
		  if isPhysical a 
		      then ()
		  else let val l = A.lookup graph a 
		       in if (Regset.numItems l > !badness_threshold) then
			   (delete_from_graph graph a;
			    bad := Regset.add(!bad,a);
			   print "XXX something is becoming bad\n")
			  else ()
		       end
		   
	  in  add(a,b); add(b,a); promote a; promote b
	  end
	  handle AdjList => 
	      (if member(a,!bad) then ()
	       else error "insert_edge: node not found!"))
	    
     fun insert_edge (g as GRAPH{all,size,bad,graph}) (a,b) =
       let 
(*
	  val _ = (print "insert_edge "; print (msReg a); print "  ";
			print (msReg b); print "\n")
*)
       in  if eqReg(a,b) orelse member(b,!bad) orelse member(a,!bad) 
	       then ()
	   else insert_edge_fast g (a,b)
       end

     fun insert_edges (g as GRAPH{all,size,bad,graph}) (a,nodes) =
       let 
	   val canInterfere = 
	       (case a of
		    R _ => (fn b as R _ => not(eqReg(a,b) orelse (member(b,!bad))) | _ => false)
		  | F _ => (fn b as F _ => not(eqReg(a,b) orelse (member(b,!bad))) | _ => false))
	   val nodes = Regset.listItems nodes
	   val nodes = List.filter canInterfere nodes
       in  if member(a,!bad)
	       then () 
	   else app (fn b => insert_edge_fast g (a,b)) nodes
       end


       fun print_stats (GRAPH{all,size,bad,graph}) =
	   let val numBad = Regset.numItems (!bad)
	       val edges = ref (numBad * (!size - 1))
	   in A.appi (fn (_,n) =>
		         (edges := !edges + numBad + (Regset.numItems n))) graph;
	      
	      print "IG stat:  (V,E,avg deg) = (";
	      print (Int.toString (!size)); print ", ";
	      print (Int.toString (!edges)); print ", ";
	      (if ((!size) = 0)
		 then print "undef"
	       else print (Int.toString ((!edges) div (!size))));
	      print ")\n";
	      print "       bad = "; print (Int.toString numBad); print "\n"
	   end
end

*)


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
         fun hashVal (R v) = Word.fromInt v
           | hashVal (F v) = Word.fromInt v
	 val sameKey = eqReg
       end
     structure A : MONO_HASH_TABLE = HashTableFn(HashKey)

     (* graph representation:
          size is the total number of nodes
	    all is the list of all nodes
	  graph is a hash table with one entry for each of the nodes
	    each entry contains a set of neighbors *)

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
     
     val num_general_iregs = length general_iregs
     val num_general_fregs = length general_fregs
     val general_iregs = Regset.addList(Regset.empty,general_iregs)
     val general_fregs = Regset.addList(Regset.empty,general_fregs)

     fun edges (GRAPH {graph,...}) n =
	 case A.find graph n of
	     NONE => error "node is not in graph"
	   | SOME l => l

     fun degree (GRAPH {graph,...}) n =
	 case A.find graph n of
	     NONE => error "node is not in graph"
	   | SOME neighbors => (Regset.numItems neighbors) 

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

     (* add an interference edge between 2 registers:
              . two registers interfere only if they're the same type
              . don't keep edge lists for physical registers
              . don't add an edge from a register to itself
      *)

     fun copy (GRAPH {all,size,graph}) = 
	 GRAPH{graph = A.copy graph,
	       size = ref (!size),
	       all = ref (!all)}

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
	 let fun split [] (iRegs,fRegs) = (iRegs,fRegs)
	       | split (r::rest) (iRegs,fRegs) = split rest (case r of
								 R _ => (r::iRegs,fRegs)
							       | F _ => (iRegs,r::fRegs))
	     fun folder (r as R _, (iRegs, fRegs)) = (r::iRegs,fRegs)
	       | folder (r as F _, (iRegs, fRegs)) = (iRegs,r::fRegs)
	     val (inodeList1,fnodeList1) = split nodeList ([],[])
	     val (inodeList2,fnodeList2) = Regset.foldl folder ([],[]) nodeSet
(*
	     val _ = (print "potential edges from insert_edges: ";
		      print (Int.toString ((length inodeList1) * (length inodeList2) +
					   (length fnodeList1) * (length fnodeList2)));
		      print "\n")
*)
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

