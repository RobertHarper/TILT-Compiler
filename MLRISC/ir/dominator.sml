(* 
 * Computation of the dominator tree representation from the
 * control flow graph.   Can also compute the DJgraph. 
 * I'm using the old algorithm by Lengauer and Tarjan.
 *
 * Note: to deal with CFG with endless loops,
 * by default we assume instructions are postdominated by STOP. 
 *
 * The algorithm for dominance frontier and iterated dominance
 * frontier is due to Sreedhar, Gao and Lee.   The algorithm as cited
 * uses the DJgraph.  In order not to bother with constructing and 
 * maintaining the DJgraph, we'll just use a combination of the dominator tree
 * and the original cfg.
 *)

functor DominatorTreeFn (GraphImpl : GRAPH_IMPLEMENTATION
                        ) : DOMINATOR_TREE =
struct

   structure GI      = GraphImpl
   structure G       = Graph
   structure Rev     = ReversedGraphView
   structure A       = Array 
   structure NodeSet = BitSet

   exception Dominator
   
   datatype 'n dom_node = 
       DOM of { node : 'n, level : int, preorder : int, postorder : int }

   type dominator_methods =
        { dominates              : G.node_id * G.node_id -> bool,
          immediately_dominates  : G.node_id * G.node_id -> bool,
          strictly_dominates     : G.node_id * G.node_id -> bool,
          postdominates          : G.node_id * G.node_id -> bool,
          immediately_postdominates : G.node_id * G.node_id -> bool,
          strictly_postdominates : G.node_id * G.node_id -> bool,
          control_equivalent     : G.node_id * G.node_id -> bool,
          idom         : G.node_id -> G.node_id,
          idoms        : G.node_id -> G.node_id list,
          doms         : G.node_id -> G.node_id list,
          ipdom        : G.node_id -> G.node_id,
          ipdoms       : G.node_id -> G.node_id list,
          pdoms        : G.node_id -> G.node_id list,
          dom_lca      : Graph.node_id * Graph.node_id -> Graph.node_id,
          pdom_lca     : Graph.node_id * Graph.node_id -> Graph.node_id,
          dom_level    : Graph.node_id -> int,
          pdom_level   : Graph.node_id -> int,
          control_equivalent_partitions : unit -> Graph.node_id list list
        }

   type ('n,'e,'g) dom_info = 
       { cfg : ('n,'e,'g) G.graph, edge_label : string,
         methods : (unit -> dominator_methods) ref, 
         max_levels : int ref
       }
   type ('n,'e,'g) dominator_tree     =
       ('n dom_node,unit,('n,'e,'g) dom_info) G.graph
   type ('n,'e,'g) postdominator_tree = 
       ('n dom_node,unit,('n,'e,'g) dom_info) G.graph 

   fun graph_info (G.GRAPH dom) : ('n,'e,'g) dom_info = #graph_info dom 

   fun cfg dom        = #cfg(graph_info dom)
   fun max_levels dom = !(#max_levels(graph_info dom))
   fun methods dom    = (!(#methods(graph_info dom)))()

   (*
    * This is the main Lengauer/Tarjan algorithm
    *)
   fun tarjan_lengauer (name,edge_label) (CFG as (G.GRAPH cfg)) =
   let val N           = #order cfg ()
       val M           = #capacity cfg ()
       val r           = case #entries cfg () of
                           [r] => r
                         | _   => raise Dominator
       val in_edges    = #in_edges cfg
       val succ        = #succ cfg
       val dfnum       = A.array (M, ~1)
       val vertex      = A.array (N, ~1) 
       val parent      = A.array (M, ~1)  
       val bucket      = A.array (M, [])    : G.node_id list array
       val semi        = A.array (M, r)  
       val ancestor    = A.array (M, ~1) 
       val idom        = A.array (M, r) 
       val samedom     = A.array (M, ~1)
       val best        = A.array (M, ~1)
       val max_levels  = ref 0
       val dom_info    = { cfg = CFG, edge_label = edge_label,
                           methods = ref(fn _ => raise G.Graph "dom"),
                           max_levels = max_levels }
       val Dom as G.GRAPH domtree = GI.graph(name, dom_info, N)

       (* step 1 
        * Initialize semi dominators and parent map
        *)
       fun dfs(p,n,N) =
         if A.sub(dfnum,n) = ~1 then
            (A.update(dfnum,n,N);
             A.update(vertex,N,n);
             A.update(parent,n,p);
             dfsSucc(n,succ n,N+1)
            )
         else N
       and dfsSucc(p,[],N)    = N
         | dfsSucc(p,n::ns,N) = dfsSucc(p,ns,dfs(p,n,N))

       and dfsAll(n::ns,N) = dfsAll(ns,dfs(~1,n,N))
         | dfsAll([],N)    = ()
       val nonRoots = List.foldr 
                        (fn ((r',_),l) => if r <> r' then r'::l else l) []
                          (#nodes cfg ())
       val _ = dfsAll(nonRoots,dfs(~1,r,0))

       (*
       fun pr s = print (s ^ "\n")
       fun dumpArray title a = 
          pr(title ^ ": " ^
                      String.concat(A.foldr 
                         (fn (i,s) => Int.toString i::" "::s) [] a))

       val _ = pr("root = " ^ Int.toString r)
       val _ = dumpArray "vertex" vertex
       val _ = dumpArray "dfnum" dfnum
       val _ = dumpArray "parent" parent
       val _ = Msg.printMessages(fn _ => CFG.G.printGraph (!Msg.outStream) cfg)
       *)

       fun link(p,n) = (A.update(ancestor,n,p); A.update(best,n,n))

       fun ancestorWithLowestSemi v =
       let val a = A.sub(ancestor,v)
       in  if a <> ~1 andalso A.sub(ancestor,a) <> ~1 then
              let val b = ancestorWithLowestSemi a
              in  A.update(ancestor,v,A.sub(ancestor,a));
                  if A.sub(dfnum,A.sub(semi,b)) <
                     A.sub(dfnum,A.sub(semi,A.sub(best,v))) then
                     A.update(best,v,b)
                  else ()
              end
           else ();
           let val u = A.sub(best,v) 
           in  if u = ~1 then v else u
           end
       end

       (* steps 2 and 3
        * Compute vertex, bucket and semi maps 
        *)
       fun compute 0 = ()
         | compute i = 
           let val n = A.sub(vertex,i)
               val p = A.sub(parent,n)
               fun computeSemi ((v,n,_)::rest,s) =
                  if v = n then computeSemi(rest,s)
                  else
                  let val s' = if A.sub(dfnum,v) < A.sub(dfnum,n) then v
                               else A.sub(semi,ancestorWithLowestSemi(v))
                      val s  = if A.sub(dfnum,s') < 
                                  A.sub(dfnum,s) then s'
                               else s
                  in  computeSemi(rest,s) 
                  end
                | computeSemi ([], s) = s
           in  if p <> ~1 then
               let val s = computeSemi(in_edges n, p)
               in  A.update(semi,n,s);
                   A.update(bucket,s,n::A.sub(bucket,s));
                   link(p,n);
                   app (fn v => 
                      let val y = ancestorWithLowestSemi(v)
                      in  if A.sub(semi,y) = A.sub(semi,v) then
                             A.update(idom,v,p) else A.update(samedom,v,y)
                      end) (A.sub(bucket,p));
                   A.update(bucket,p,[])
               end else ();
               compute(i-1)
           end
       val _ = compute (N-1)

       (*
       val _ = dumpArray "semi" idom
       val _ = dumpArray "idom" idom
        *)

       (* step 4 update dominators *)
       fun updateIdoms i = 
         if i < N then
            let val n = A.sub(vertex, i)
            in  if A.sub(samedom, n) <> ~1 
                then A.update(idom, n, A.sub(idom, A.sub(samedom, n)))
                else ();
                updateIdoms (i+1)   
            end 
         else ()
       val _ = updateIdoms 1

       (*
       val _ = dumpArray "idom" idom
        *)

       (* Create the nodes of the dominator tree *)
       fun addNode ~1 = ()
         | addNode i  = let val v = A.sub(vertex,i)
                        in  #add_node domtree (v, 
                               DOM { node      = #node_info cfg v,
                                     level     = 0,
                                     preorder  = 0,
                                     postorder = 0
                                   });
                            addNode (i-1)
                        end

       val _ = addNode (N-1) 

       (* Create the edges of the dominator tree *)
       val _ = #forall_nodes domtree
                   (fn (v,_) =>
                     if v <> r then
                        let val w = A.sub(idom,v)
                        in  #add_edge domtree (w,v,()) end
                     else  ())

       (* Compute the level/preorder/postorder numbers *)
       fun computeNumbering(level,preorder,postorder,n) = 
       let val DOM { node, ... } = #node_info domtree n
           val (preorder',postorder',max_level) =
                   computeNumbering'(level+1,preorder+1,postorder,
                                     #succ domtree n)
       in  #add_node domtree (n, 
               DOM{node=node,level=level,
                   preorder=preorder,postorder=postorder'});
           (preorder',postorder'+1,max_level)
       end

       and computeNumbering'(level,preorder,postorder,[]) =
                (preorder,postorder,level)
         | computeNumbering'(level,preorder,postorder,n::ns) =
             let val (preorder',postorder',max1) = 
                   computeNumbering(level,preorder,postorder,n)
                 val (preorder',postorder',max2) =
                   computeNumbering'(level,preorder',postorder',ns)
             in  (preorder',postorder',Int.max(max1,max2))
             end

       val (_,_,max) = computeNumbering(0,0,0,r)
   in  
       max_levels := max+1;
       #set_entries domtree [r];
       (* Msg.printMessages(fn _ => G.printGraph (!Msg.outStream) domtree); *)
       Dom
   end

   (* The algorithm specialized to making dominators and postdominators *)

   fun dominator_trees (cfg as (G.GRAPH g)) = 
   let val Dom as G.GRAPH D = 
             tarjan_lengauer ("Dom","dom") cfg
       val PDom as G.GRAPH P = 
             tarjan_lengauer ("PDom","pdom") (Rev.rev_view cfg)

       fun immediately_dominates (i,j) =
           case #in_edges D j of
              (k,_,_)::_ => i = k 
           |  _          => false

       fun immediately_postdominates (i,j) =
           case #in_edges D j of
              (k,_,_)::_ => i = k 
           |  _          => false

       (* immediate dominator of n *)
       fun idom n = case #in_edges D n of
                       (n,_,_)::_ => n
                    |  _          => ~1

       fun idoms n = #succ D n

       fun subtree (succ,[],S) = S
         | subtree (succ,n::ns,S) = subtree(succ,succ n,
                                        subtree(succ,ns,n::S))

       fun dom_level i = 
           let val DOM{level,...} = #node_info D i in level end
       fun pdom_level i = 
           let val DOM{level,...} = #node_info P i in level end

           (* Least common ancestors *)
       fun dom_lca(a,b) =
           let val l_a = dom_level a 
               val l_b = dom_level b
               fun idom i = case #in_edges D i of (j,_,_)::_ => j
               fun up_a(a,l_a) = if l_a > l_b then up_a(idom a,l_a-1) else a
               fun up_b(b,l_b) = if l_b > l_a then up_b(idom b,l_b-1) else b
               val a = up_a(a,l_a)
               val b = up_b(b,l_b)
               fun up_both(a,b) = if a = b then a else up_both(idom a,idom b)
           in  up_both(a,b)
           end
       fun pdom_lca(a,b) =
           let val l_a = pdom_level a 
               val l_b = pdom_level b
               fun ipdom i = case #in_edges P i of (j,_,_)::_ => j
               fun up_a(a,l_a) = if l_a > l_b then up_a(ipdom a,l_a-1) else a
               fun up_b(b,l_b) = if l_b > l_a then up_b(ipdom b,l_b-1) else b
               val a = up_a(a,l_a)
               val b = up_b(b,l_b)
               fun up_both(a,b) = if a = b then a else up_both(ipdom a,ipdom b)
           in  up_both(a,b)
           end
           
       fun doms n = subtree(#succ D, [n], [])

       (* immediate postdominator of n *)
       fun ipdom n = case #in_edges P n of
                        (n,_,_)::_ => n
                     |  _          => ~1
       fun ipdoms n = #succ P n
       fun pdoms n  = subtree(#succ P, [n], [])

      (* is x and ancestor of y in D?
       * This is true iff PREORDER(x) <= PREORDER(y) and
       *                  POSTORDER(x) >= POSTORDER(y)
       *)
      fun dominates (x,y) =
      let val DOM { preorder = a, postorder = b, ... } = #node_info D x
          val DOM { preorder = c, postorder = d, ... } = #node_info D y
      in  a <= c andalso b >= d
      end

      fun strictly_dominates (x,y) = 
      let val DOM { preorder = a, postorder = b, ... } = #node_info D x 
          val DOM { preorder = c, postorder = d, ... } = #node_info D y 
      in  a < c andalso b > d
      end

      fun postdominates (x,y) =
      let val DOM { preorder = a, postorder = b, ... } = #node_info P x
          val DOM { preorder = c, postorder = d, ... } = #node_info P y
      in  a <= c andalso b >= d
      end

      fun strictly_postdominates (x,y) = 
      let val DOM { preorder = a, postorder = b, ... } = #node_info P x
          val DOM { preorder = c, postorder = d, ... } = #node_info P y
      in  a < c andalso b > d
      end

      fun control_equivalent (x,y) =
          dominates(x,y) andalso postdominates(y,x) orelse
          dominates(y,x) andalso postdominates(x,y)

      (* control equivalent partitions 
       * two nodes a and b are control equivalent iff
       *    a dominates b and b postdominates a (or vice versa) 
       * We use the following property of dominators to avoid wasteful work:
       *    If i dom j dom k and j not pdom i then
       *          k not pdom i
       * This algorithm runs in O(n)  
       *)
      fun control_equivalent_partitions() =
      let fun walkDom([],S) = S
            | walkDom(n::waiting,S) =
               let val (waiting,S,S') = 
                       findEquiv(n,#out_edges D n,waiting,S,[n])
               in  walkDom(waiting,S'::S)
               end
          and findEquiv(i,[],waiting,S,S') = (waiting,S,S')
            | findEquiv(i,(_,j,_)::es,waiting,S,S') =
                if postdominates(j,i) then
                   let val (waiting,S,S') = findEquiv(i,es,waiting,S,j::S')
                   in  findEquiv(i,#out_edges D j,waiting,S,S')
                   end
                else
                   findEquiv(i,es,j::waiting,S,S')

          val equivSets = walkDom(#entries D (),[])
      in
          equivSets
      end

         (* Methods for dominators/postdominator manipulation *)
      val methods = { dominates              = dominates,
                      strictly_dominates     = strictly_dominates,
                      postdominates          = postdominates,
                      strictly_postdominates = strictly_postdominates,
                      control_equivalent     = control_equivalent,
                      idom                   = idom,
                      idoms                  = idoms,
                      doms                   = doms,
                      ipdom                  = ipdom,
                      ipdoms                 = ipdoms,
                      pdoms                  = pdoms,
                      dom_level              = dom_level,
                      pdom_level             = pdom_level,
                      dom_lca                = dom_lca,
                      pdom_lca               = pdom_lca,
                      immediately_dominates  = immediately_dominates,
                      immediately_postdominates = immediately_postdominates,
                      control_equivalent_partitions = 
                          control_equivalent_partitions
                    }
      fun get_methods _ = methods
   in
      #methods (#graph_info D) := get_methods;
      #methods (#graph_info P) := get_methods;
      (Dom, PDom)
   end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:13  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:43  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:21  pscheng
# *** empty log message ***
#
 *)
