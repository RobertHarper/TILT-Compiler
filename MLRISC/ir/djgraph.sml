(* 
 * The algorithm for dominance frontier and iterated dominance
 * frontier is due to Sreedhar, Gao and Lee.   The algorithm as cited
 * uses the DJgraph.  In order not to bother with constructing and 
 * maintaining the DJgraph, we'll just use a combination of the dominator tree
 * and the original cfg.  This alteration does not change the linear
 * complexity of the algorithm.  I'm also using a simple time stamp trick to 
 * avoid the data structure initialization step; this should make the
 * algorithm sublinear in N in practice, where N is the number of nodes 
 * in the CFG.
 *
 * --Allen
 *)

functor DJGraphFn (Dom : DOMINATOR_TREE) : DJ_GRAPH =
struct

   structure G       = Graph
   structure Dom     = Dom
   structure A       = Array

   type ('n,'e,'g) dj_graph = ('n,'e,'g) Dom.dominator_tree

   fun dj_graph (D as G.GRAPH dom) =
   let val G.GRAPH cfg = Dom.cfg D
       val methods     = Dom.methods D
       val L           = Dom.max_levels D
       val N           = #capacity dom ()
       val node_info   = #node_info dom
       val idom        = #immediately_dominates methods 
       fun levelOf x   = let val Dom.DOM{level,...} = node_info x in level end
       val in_DF       = A.array(N,0)  (* has appeared in the DF set? *)
       val stamp       = ref 0
       fun new_stamp() = let val s = !stamp + 1 in stamp := s; s end

       fun unmarked(marked,i,stamp) =
           let val s = A.sub(marked,i)
           in  if s = stamp then false else (A.update(marked,i,stamp); true)
           end

       (* 
        * Compute the dominance frontiers of a node
        * Dominance frontier of x: 
        *   The set of all nodes y such that x dominates a predecessor 
        *   of y but x doesn't strictly dominates y.
        *)
       fun DF x =
       let val stamp = new_stamp()
           val level_x = levelOf x
           fun walk(z, S) = 
               let fun scan((_,y,_)::es,S) =
                       if levelOf y <= level_x andalso
                           unmarked(in_DF,y,stamp) then scan(es,y::S)
                       else scan(es,S)
                     | scan([],S) = S
                   val S = scan(#out_edges cfg z,S)
                   fun walkList([],S) = S
                     | walkList((_,z,_)::es,S) = walkList(es,walk(z,S))
               in  walkList(#out_edges dom z,S)
               end
       in  walk(x,[])
       end

       val in_alpha  = A.array(N,0)  (* has appeared in N_alpha? *)
       val visited   = A.array(N,0)  (* has it been visited *)
       val piggybank = A.array(L,[]) (* nodes in the piggy bank *)

       (* 
        * This algorithm is described in POPL 95 
        *)
       fun IDFs xs =
       let val stamp = new_stamp()
           fun init([],l) = l
             | init(x::xs,l) = 
               let val l_x = levelOf x
               in  A.update(in_alpha,x,stamp);
                   A.update(piggybank,l_x,x::A.sub(piggybank,l_x));
                   init(xs,if l < l_x then l_x else l)
               end 
           fun visit(y,level_x,S) =
           let fun scan([],S) = S
                 | scan((_,z,_)::es,S) =
                   let val level_z = levelOf z
                   in  if level_z <= level_x andalso unmarked(in_DF,z,stamp) 
                       then (if A.sub(in_alpha,z) <> stamp 
                             then A.update(piggybank,level_z,
                                           z::A.sub(piggybank,level_z)) 
                             else ();
                             scan(es,z::S))
                       else scan(es,S)  
                   end
               fun visitSucc([],S) = S
                 | visitSucc((_,z,_)::es,S) = 
                   visitSucc(es,if unmarked(visited,z,stamp)
                                then visit(z,level_x,S) else S)
               val S = scan(#out_edges cfg y,S)
           in  visitSucc(#out_edges dom y,S) 
           end 

           fun visitAll(~1,S) = S
             | visitAll(l,S) =
               case A.sub(piggybank,l) of
                 [] => visitAll(l-1,S)
               | x::xs => (A.update(visited,x,stamp);
                           A.update(piggybank,l,xs);
                           visitAll(l,visit(x,levelOf x,S)))

           val L = init(xs,~1) 
       in  visitAll(L,[])
       end

       fun IDF x = IDFs [x]
   in
       { DF = DF, IDF = IDF, IDFs = IDFs }
   end

end

