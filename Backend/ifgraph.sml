(*$import MACHINE IFGRAPH Util HashTableFn SplaySetFn *)
(* interference graph *)

(*
functor IfgraphOldReliable (structure Machine : MACHINE) : IFGRAPH =
struct

    val error = fn s => Util.error "ifgraph.sml" s

    open Machine

    fun eqReg (R v, R v') = v = v'
      | eqReg (F v, F v') = v = v'
      | eqReg _ = false

    structure SetKey : ORD_KEY = 
	struct
	    type ord_key = Machine.register
	    fun compare(R _, F _) = LESS
	      | compare(F _, R _) = GREATER
	      | compare(R i1, R i2) = Int.compare(i1,i2)
	      | compare(F i1, F i2) = Int.compare(i1,i2)
	end
    structure RegSet = SplaySetFn(SetKey) 

     structure HashKey =
       struct
         type hash_key = Machine.register
         fun hashVal (Machine.R v) = Word.fromInt v
           | hashVal (Machine.F v) = Word.fromInt v
	 val sameKey = eqReg
       end
     structure A : MONO_HASH_TABLE = HashTableFn(HashKey)

     (* graph representation:
           Graphs are represented using adjacency lists.
	   We keep the length of the list also.*)

     type node = Machine.register
     datatype graph = GRAPH of (int * RegSet.set) A.hash_table

     (* set utilities *)

     fun single v = RegSet.singleton v

     fun delete (a,orig as (degree,s)) =
	 ((degree-1,RegSet.delete(s,a))
	  handle LibBase.NotFound => orig)

     fun member (a,s) = RegSet.member(s,a)

     fun insert (a,orig as (degree,s)) =
	  if member(a,s) then orig else (degree+1,RegSet.add(s,a))
	 

     (* graph operations *)

     val hint = 64
     exception AdjList

     fun empty () = GRAPH (A.mkTable(hint,AdjList))

     fun nodes (GRAPH g) = map #1 (A.listItemsi g)

     fun nodes_excluding_physical (GRAPH g) = 
	  let fun scan ((h,_) :: t) =
	           if isPhysical h then scan t
		   else h :: scan t
                | scan nil = nil
         in scan (A.listItemsi g)
	 end
     
     fun edges (GRAPH g) n =
	 case A.find g n of
	     NONE => []
	   | SOME (_,l) => RegSet.listItems l

     fun degree (GRAPH g) n =
	 case A.find g n of
	     NONE => 0
	   | SOME (degree,_) => degree

     fun insert_node (GRAPH g) =
	 let val peek = A.find g
             val insert = A.insert g
	 in fn n =>
	      if not (isPhysical n) then
	         (case peek n of
		      NONE => insert (n,(0,RegSet.empty))
		   | SOME _ => ())
	      else ()
	 end

     fun delete_node (GRAPH g) =
	 let val remove = A.remove g
	     val insert = A.insert g
	     fun find k = (case A.find g k of
			       SOME value => value
			     | _ => error "delete_node: node does not exist")
	 in fn n =>
	     let val (_,l) = remove n handle
		             AdjList => (print ("delete_node: AdjList:"^msReg n^"\n");
					 raise AdjList)
		 fun upd x = if isPhysical x then ()
		             else insert (x,delete(n,find x))
	     in RegSet.app upd l
	     end
         end

     (* add an interference edge between 2 registers:
              . two registers interfere only if they're the same type
              . don't keep edge lists for physical registers
              . don't add an edge from a register to itself
      *)

     fun copy (GRAPH g) = GRAPH (A.copy g)


     fun insert_edge (GRAPH g) =
       let val peek = A.find g
	   val graph_insert = A.insert g
	   val insert_node = insert_node (GRAPH g)
	   fun add (a,b) = 
	       if not (isPhysical a) then
		   let val l = case peek a
		               of NONE => (1,single b)
			        | SOME l => insert(b,l)
		   in graph_insert (a,l)
		   end
	       else ()
       in fn (a,b) =>
	    if eqReg(a,b) then insert_node a
	    else case (a,b)
		 of (R _,R _) => (add(a,b); add(b,a))
		  | (F _,F _) => (add(a,b); add(b,a))
		  | _ => ()
       end

       fun print_stats (GRAPH g) =
	   let val count = ref 0
	       val edges = ref 0 
	   in A.appi (fn (k,(i,l)) =>
		         (count := !count + 1;
			  edges := !edges + i)) g;
	      print "IG stat:  (V,E,avg deg) = (";
	      print (Int.toString (!count)); print ", ";
	      print (Int.toString (!edges)); print ", ";
	      (if ((!count) = 0)
		 then print "undef"
	       else print (Int.toString ((!edges) div (!count))));
	      print ")\n"
	   end
end

*)

functor Ifgraph (structure Machine : MACHINE) :> IFGRAPH where Machine = Machine =

struct

    structure Machine = Machine
    val badness_threshold = ref 128

    val error = fn s => Util.error "ifgraph.sml" s

    open Machine

    fun eqReg (r1,r2) = Machine.eqRegs r1 r2


     structure HashKey =
       struct
         type hash_key = Machine.register
         fun hashVal (Machine.R v) = Word.fromInt v
           | hashVal (Machine.F v) = Word.fromInt v
	 val sameKey = eqReg
       end
     structure A : MONO_HASH_TABLE = HashTableFn(HashKey)

     (* graph representation:
          size is the total number of nodes
	    all is the list of all nodes
          bad is the set of nodes that interferes with all other nodes
	  graph is a hash table with one entry for each of the non-bad nodes
	    each entry contains a set of non-bad neighbors *)

     type node = Machine.register
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
     
     val num_general_iregs = length Machine.general_iregs
     val num_general_fregs = length Machine.general_fregs
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
	  ((* print "insert_node "; print (msReg n); print "\n";*)
	    if ((isPhysical n)
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


     fun insert_edge (g as GRAPH{all,size,bad,graph}) (a,b) =
       let 
(*
	  val _ = (print "insert_edge "; print (msReg a); print "  ";
			print (msReg b); print "\n")
*)
	   val _ = insert_node g a
	   val _ = insert_node g b
	   val graph_insert = A.insert graph
	   fun add (a,b) = 
	       if isPhysical a 
		   then ()
	       else 
		   let val l = A.lookup graph a 
				handle AdjList => error "insert_edge: node not found!"
		       val l' = insert(b,l)
		   in  graph_insert (a,l')
		   end
	   fun promote a = 
	   if (isPhysical a) then () else
	   let val l = A.lookup graph a 
			handle AdjList => error "insert_edge: node not found!"
	   in if (Regset.numItems l > !badness_threshold) then
		       (delete_from_graph graph a;
			bad := Regset.add(!bad,a))
	      else ()
	   end
       in  if eqReg(a,b) orelse member(b,!bad) orelse member(a,!bad) then ()
	    else case (a,b)
		of (R _,R _) => (add(a,b); add(b,a); promote a; promote b)
	      | (F _,F _) => (add(a,b); add(b,a); promote a; promote b)
	      | _ => ()
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

