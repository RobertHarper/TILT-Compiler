(*
 * SCC based global value numbering algorithm (L Taylor Simpson's algorithm)
 * 
 * Note: there is a bug in Simpson's algorithm involving phi-nodes. 
 * His algorithm assigns the same value number to two structurally 
 * identical SCCs.  This is fine if the two SCC are in the same loop.
 * However, this causes problems if the SCCs appear in distinct loops,
 * or if one is nested within another in the loop structure.
 *  
 * Allen
 *)

functor SSAGlobalValueNumberingFn
    (CF : SSA_CONSTANT_FOLDING) : SSA_GLOBAL_VALUE_NUMBERING =
struct

   structure SSA  = CF.SSA
   structure SP   = SSA.SP
   structure CFG  = SSA.CFG
   structure Dom  = SSA.Dom
   structure I    = SSA.I
   structure E    = SSAExp
   structure G    = Graph
   structure A    = Array
   structure H    = HashTable

   val top = CF.top

   (*
    * SCC based value numbering/constant folding
    *)
   fun computeValueNumbers (SSA as G.GRAPH ssa) =
   let val CFG as G.GRAPH cfg = SSA.cfg SSA
       val Dom as G.GRAPH dom = SSA.dom SSA
       val nodes = SSA.nodes SSA
       val N = #capacity ssa ()   (* number of instructions *)
       val M = #capacity cfg ()   (* control flow graph *)
       val V = SSA.maxVar SSA     (* number of variables *)
       val show_op = SSA.show_op SSA
       val show_val = SSA.show_val SSA

         (* 
          * Table mapping variables -> value numbers 
          *)
       val VN = A.array(V,CF.top) (* value numbers *)
       val DomN = A.array(N,~1) (* dominator numbers *)
       val visited = BitSet.create M
       fun walk(b,n) =
           let val {phis,ops,sink,source} = A.sub(nodes,b)
               fun number([],n) = n
                 | number((i,_)::ops,n) = 
                      (A.update(DomN,i,n); number(ops,n+1))
               val n = number(source,n)
               val n = number(phis,n)
               val n = number(ops,n)
               val n = number(sink,n)
               fun walkSucc([],n) = n
                 | walkSucc((_,b',_)::es,n) = walkSucc(es,walk(b',n))
           in  walkSucc(#out_edges dom b,n) end

       val _ = walk(hd(#entries dom ()),0)

       exception NotFound
       val validTable       = CF.hashTable(V,NotFound)
       val optimisticTable  = CF.hashTable(V,NotFound)
       val validLookup      = H.lookup validTable
       val validInsert      = H.insert validTable
       val optimisticLookup = H.lookup optimisticTable
       val optimisticInsert = H.insert optimisticTable

       fun bad(E.PHI _,operands) = List.all (fn r => r = top) operands
         | bad(_,operands) = List.exists (fn r => r = top) operands

       fun check(e,operands) = 
          (if bad(e,operands) then
              print("Bad opcode "^E.toString (map Int.toString operands) e^" "^
                    String.concat(map (fn r => Int.toString r^" ") operands)
                    ^"\n")
           else (); 
           (e,operands))

        (* lookup value number; create new vn if not found *)
       val validSearch = CF.constantFolding SSA 
             (fn (e,operands,t) => 
                 validLookup(e,operands) handle _ =>
                     (validInsert((* check *)(e,operands),t); t))
                     
       val optimisticSearch = CF.constantFolding SSA 
             (fn (e,operands,t) =>
                 optimisticLookup(e,operands) handle _ =>
                    (optimisticInsert((* check *)(e,operands),t); t))
  
       fun dumpSCC ops = 
       let fun printVN(i,i') = 
           let fun pr(t) = 
               let val vn = A.sub(VN,t)
               in  if vn <> t then print(" VN="^Int.toString vn)
                   else ()
               end
           in  print("\t("^Int.toString(A.sub(DomN,i))^") "^show_op i');
               case i' of
                   SSA.OP{t=[t],...} => pr t
                |  SSA.PHI{t=t,...} => pr t
                |  _ => ();
               print "\n"
           end
       in  print "SCC=\n"; 
           app printVN ops
       end

         (* 
          * compute the fixpoint of an scc 
          *) 
       fun unique ts = app (fn t => A.update(VN,t,t)) ts

       fun isVolatile r = List.exists (fn r' => r' = r) SP.volatile

       fun initSource(t,t') = 
       let fun init(t::ts,t'::ts') = 
               (A.update(VN,t,
                 if isVolatile t' then CF.volatile else t); init(ts,ts'))
             | init _ = ()
       in  init(t,t') end

       fun processSCC (scc,()) =
       let fun init t = A.update(VN,t,top)
           fun initialize([],ops) = ops
             | initialize(i::is,ops) =
               let val i' = #node_info ssa i
               in  case i' of
                      SSA.SOURCE{t,t',...} => initSource(t,t')
                   |  SSA.SINK _ => ()
                   |  SSA.OP{t,e=E.COPY,...} => app init t
                   |  SSA.OP{t=[t],...} => init t
                   |  SSA.OP{t,...} => unique t
                   |  SSA.PHI{t,...} => init t;
                   initialize(is,(i,i')::ops)
               end

           val ops = initialize(scc,[])
           fun byDomN((i,_),(j,_)) = A.sub(DomN,i) < A.sub(DomN,j)
           val ops = Sorting.sort byDomN ops

           fun loop([],look,more) = more
             | loop((_,SSA.SOURCE _)::ops,look,more) = loop(ops,look,more)
             | loop((_,SSA.SINK _)::ops,look,more) = loop(ops,look,more)
             | loop((_,SSA.OP{t=[],...})::ops,look,more) = loop(ops,look,more)
             | loop((_,SSA.OP{e=E.COPY,t,s,...})::ops,look,more) = 
                  loop(ops,look,processCopy(t,s,more))
             | loop((_,SSA.PHI{t,s,b,...})::ops,look,more) = 
                  loop(ops,look,process(look,E.PHI b,t,s,more))
             | loop((_,SSA.OP{e,t=[t],s,...})::ops,look,more) = 
                  loop(ops,look,process(look,e,t,s,more))
             | loop((_,SSA.OP{e,t,s,...})::ops,look,more) = 
                  loop(ops,look,more)

           and compute_vn [] = []
             | compute_vn (r::rs) = 
                 (if r < 0 then r else A.sub(VN,r))::compute_vn rs

           and process(look,e,t,s,changed) =
               let val n = look(e,compute_vn s,t)
               in  if A.sub(VN,t) = n then changed
                   else (A.update(VN,t,n); true)
               end

           and processCopy(t,s,changed) =
               let val vn = map (fn r => A.sub(VN,r)) s
                   fun update(t::ts,vn::vns,changed) =
                       if A.sub(VN,t) = vn then update(ts,vns,changed)
                       else (A.update(VN,t,vn); update(ts,vns,true))
                     | update(_,_,changed) = changed
               in  update(t,vn,changed) end

       in  case ops of
             [i] => (loop(ops,validSearch,false); ())
           | _   => let fun iterate count =
                            if loop(ops,optimisticSearch,false) then
                               iterate(count+1)
                            else count
                        val count = iterate 1
                    in  (* dumpSCC ops;
                        print("["^Int.toString(length ops)^":"^
                                  Int.toString(count)^"]"); *)
                        loop(ops,validSearch,false); ()
                    end
       end

       (*
        * Initialize all value numbers
        *)
       fun initializeValueNumbers() =
       let val ENTRY = hd(#entries dom ())
           fun init s = 
               let val SSA.SOURCE{t,b,...} = #node_info ssa s  
               in  unique t; 
                   if b = ENTRY then app initEdge(#out_edges ssa s) else ()
               end
           and initEdge(_,_,r) = A.update(VN,r,r)
       in  app init (#entries ssa ());
           case I.C.zero I.C.GP of
             SOME zeroR => A.update(VN,zeroR,CF.zero) 
           | NONE => ()
       end
          
   in  initializeValueNumbers();
       GraphSCC.scc (ReversedGraphView.rev_view SSA) processSCC ();
       VN
   end

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:15:02  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:24  pscheng
# *** empty log message ***
#
 *)
