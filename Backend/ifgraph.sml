(* interference graph *)

functor Ifgraph (structure Machine : MACHINE) : IFGRAPH =
struct

    val error = fn s => Util.error "ifgraph.sml" s

    open Machine

    fun eqReg (R v, R v') = v = v'
      | eqReg (F v, F v') = v = v'
      | eqReg _ = false

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
     datatype graph = GRAPH of (int * node list) A.hash_table

     (* list utilities *)

     fun delete (a,(degree,l)) =
       let exception NotFound
	   fun remove (h::t) =
	        if eqReg (h,a) then t
		else h :: remove t
             | remove nil = raise NotFound
       in (degree-1,remove l)
	  handle NotFound => (degree,l)
       end

     fun member (a,nil) = false
       | member (a,h::t) = eqReg(a,h) orelse member(a,t)

     fun insert (a,(degree,l)) =
	  if member(a,l) then (degree,l) else (degree+1,a :: l)
	 

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
	     NONE => nil
	   | SOME (_,l) => l

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
		      NONE => insert (n,(0,nil))
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
	     in app upd l
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
		               of NONE => (1,[b])
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

