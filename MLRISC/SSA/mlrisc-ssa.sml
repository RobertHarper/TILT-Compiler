(*
 *  Basic SSA definitions, data structures, and transformations.
 *)

functor SSAFn
   (structure CFG : CONTROL_FLOW_GRAPH
    structure Dom : DOMINATOR_TREE
    structure SP  : SSA_PROPERTIES
    structure P   : INSN_PROPERTIES
    structure FormatInsn : FORMAT_INSTRUCTION
    structure GraphImpl : GRAPH_IMPLEMENTATION
      sharing CFG.I = SP.I = P.I = FormatInsn.I
   ) : SSA =
struct

   structure CFG = CFG
   structure Dom = Dom
   structure I   = CFG.I
   structure C   = I.C
   structure SP  = SP
   structure G   = Graph
   structure DA  = DynamicArray
   structure HA  = HashArray
   structure HT  = HashTable
   structure L   = GraphLayout
   structure E   = SSAExp
   structure A   = Array
   structure SU  = SSAPropsUtil

   fun error msg = MLRiscErrorMsg.impossible("MLRISC_SSA."^msg)

   type value  = int
   type pos    = int
   type block  = Graph.node_id
   type exp    = SSAExp.exp
   type const  = SP.const
   type dom    = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree

   datatype ssa_op = 
     PHI of    {preds:block list,t':C.register,t:value,s:value list,b:block}
   | OP  of    {e:exp,i:I.instruction,s:value list,t:value list,b:block,p:pos} 
   | SOURCE of {t:value list,t':C.register list,b:block}
   | SINK of   {s:value list,s':C.register list,b:block}

   datatype info = 
       INFO of 
       { cfg        : CFG.cfg,
         dom        : CFG.cfg -> dom,
         immTable   : value HA.array,
         labTable   : (Label.label,value) HT.table,
         opnTable   : (I.operand,value) HT.table,   
         constTable : const HA.array,
         defSite    : Graph.node_id DA.array,
         cellClass  : C.cellclass DA.array,
         nextImmed  : int ref,
         minPos     : int ref,
         maxPos     : int ref,
         nameTable  : (C.register*int) HA.array (* old name/subscript *)
       }

   type ssa = (ssa_op,value,info) Graph.graph

   exception NoConst
   exception NoLabel
   exception NoOperand
   exception NoDefSite
   exception NoName
 
   (* 
    *  Selectors
    *)
   fun getInfo (G.GRAPH ssa) = let val INFO info = #graph_info ssa in info end

   fun dom SSA = 
       let val {cfg,dom,...} = getInfo SSA
       in  dom cfg end

   fun cfg SSA = #cfg(getInfo SSA) 

   fun immed SSA = 
       let val {immTable,...} = getInfo SSA
       in  fn i => HA.sub(immTable,i) end 

   fun label SSA = 
       let val {labTable,constTable,nextImmed,...} = getInfo SSA
           val look = HT.lookup labTable
           val ins  = HT.insert labTable
           fun lookup lab = look lab handle NoLabel =>
               let val v = !nextImmed 
                   val _ = nextImmed := v - 1
               in  HA.update(constTable,v,SP.LABEL lab); ins(lab,v); v end
       in  lookup end

   fun operand SSA = 
       let val {opnTable,constTable,nextImmed,...} = getInfo SSA
           val look = HT.lookup opnTable
           val ins  = HT.insert opnTable
           fun lookup opn = look opn handle NoOperand =>
               let val v = !nextImmed 
                   val _ = nextImmed := v - 1
               in  HA.update(constTable,v,SP.OPERAND opn); ins(opn,v); v end
       in  lookup end

   fun const SSA = 
       let val {constTable,...} = getInfo SSA
       in  fn v => HA.sub(constTable,v) end

   fun maxVar(G.GRAPH ssa) = #capacity ssa ()

   fun operands SSA = 
       let val {nextImmed,...} = getInfo SSA
       in  ~(!nextImmed) - 1 end

   fun defSite SSA =
       let val {defSite,...} = getInfo SSA
       in  fn v => DA.sub(defSite,v) end

   fun cellClass SSA = 
       let val {cellClass,...} = getInfo SSA
       in  fn v => DA.sub(cellClass,v) end

   fun updateCellClass SSA = 
       let val {cellClass,...} = getInfo SSA 
       in  fn (v,c) => DA.update(cellClass,v,c) end

   fun newVar SSA = 
       let val {cellClass,nameTable,...} = getInfo SSA
       in  fn c => let val r = C.newReg()
                   in DA.update(cellClass,r,c); r end
       end

   fun newRenamedVar subscriptTable SSA = 
       let val {cellClass,nameTable,...} = getInfo SSA
           fun f r = 
           let val r' = C.newReg()
           in  DA.update(cellClass,r',DA.sub(cellClass,r)); r' end
           fun g subscriptTable r = 
           let val r' = C.newReg()
               val i = A.sub(subscriptTable,r)
           in  DA.update(cellClass,r',DA.sub(cellClass,r)); 
               A.update(subscriptTable,r,i+1);
               (* print("[ r"^Int.toString r'^" -> "^
                     Int.toString r^"."^Int.toString i^"] \n"); *)
               HA.update(nameTable,r',(r,i));
               r'
           end
       in  case subscriptTable of 
              SOME t => g t 
           |  NONE   => f
       end

   (*
    * Create a new SSA graph
    *)
   fun newSSA(CFG as G.GRAPH cfg, dom) =
   let val nextImmed  = ref ~1
       val constTable = HA.array'(37,fn _ => raise NoConst)
       fun newImmed i =   
           let val v = !nextImmed
               val _ = nextImmed := v - 1
           in  HA.update(constTable,v,SP.IMMED i); v end
       val immTable   = HA.array''(37,newImmed)
       val labTable   = HashTable.create{== = SU.eqLabel,hash=SU.hashLabel,
                                         size=37,exn=NoLabel}
       val opnTable   = HashTable.create{== = SP.eqOpn,hash=SP.hashOpn,
                                         size=37,exn=NoOperand}
       val ENTRY      = hd(#entries cfg ()) 
       val uninit     = C.newReg()
       val defSite    = DA.array(37,uninit)
       val cellClass  = DA.array(C.maxCell(),C.GP)
       val nameTable  = HA.array'(13,fn _ => raise NoName)
       val info       = INFO{cfg        = CFG,
                             dom        = dom,
                             immTable   = immTable,
                             labTable   = labTable,
                             opnTable   = opnTable,
                             constTable = constTable,
                             defSite    = defSite,
                             cellClass  = cellClass,
                             nextImmed  = nextImmed,
                             minPos     = ref 0,
                             maxPos     = ref 1000000,
                             nameTable  = nameTable
                            }
       val G.GRAPH ssa = GraphImpl.graph("SSA",info,30)
       val _           = #add_node ssa (uninit,SOURCE{b=ENTRY,t=[],t'=[]})
       val _           = #set_entries ssa [uninit]
       fun getDefs(OP{t,...}) = t
         | getDefs(PHI{t,...}) = [t]
         | getDefs(SOURCE{t,...}) = t
         | getDefs(SINK _) = []
       fun add_node(n,n') = 
           (DA.update(defSite,n,n);
            app (fn r => DA.update(defSite,r,n)) (getDefs n');
            #add_node ssa (n,n'))
       fun node_info n = #node_info ssa (DA.sub(defSite,n))
       fun remove_node n = #remove_node ssa (DA.sub(defSite,n))
       fun has_node n = #has_node ssa (DA.sub(defSite,n))
           
       val SSA = 
           G.GRAPH
           { name            = #name ssa,
             graph_info      = #graph_info ssa,
             new_id          = #new_id ssa,
             add_node        = add_node,
             add_edge        = #add_edge ssa,
             remove_node     = remove_node,
             set_in_edges    = #set_in_edges ssa,
             set_out_edges   = #set_out_edges ssa,
             set_entries     = #set_entries ssa,
             set_exits       = #set_exits ssa,
             garbage_collect = #garbage_collect ssa,
             nodes           = #nodes ssa,
             edges           = #edges ssa,
             order           = #order ssa,
             size            = #size ssa,
             capacity        = #capacity ssa,
             out_edges       = #out_edges ssa,
             in_edges        = #in_edges ssa,
             succ            = #succ ssa,
             pred            = #pred ssa,
             has_edge        = #has_edge ssa,
             has_node        = has_node,
             node_info       = node_info,
             entries         = #entries ssa,
             exits           = #exits ssa,
             entry_edges     = #entry_edges ssa,
             exit_edges      = #exit_edges ssa,
             forall_nodes    = #forall_nodes ssa,
             forall_edges    = #forall_edges ssa
           }
       val _   = app (fn i => (immed SSA i; ())) [0,1]
   in  SSA
   end

   (*
    * Signal that the SSA graph has changed
    *)
   fun changed SSA = ()

   (*
    * Extract the nodes in an SSA graph in a table form.
    * Convenient for scanning. 
    *)
   fun nodes (SSA as G.GRAPH ssa) =
   let val CFG as G.GRAPH cfg = cfg SSA
       val N = #capacity cfg ()
       val T = A.array(N,{source=[],phis=[],ops=[],sink=[]})
       fun ins(x as (_,n')) = 
           case n' of
             SOURCE{b,...} => 
               let val {source,phis,ops,sink} = A.sub(T,b)
               in  A.update(T,b,{source=x::source,phis=phis,ops=ops,sink=sink})
               end
           | PHI{b,...} => 
               let val {source,phis,ops,sink} = A.sub(T,b)
               in  A.update(T,b,{source=source,phis=x::phis,ops=ops,sink=sink})
               end
           | OP{b,...} =>
               let val {source,phis,ops,sink} = A.sub(T,b)
               in  A.update(T,b,{source=source,phis=phis,ops=x::ops,sink=sink})
               end
           | SINK{b,...} => 
               let val {source,phis,ops,sink} = A.sub(T,b)
               in  A.update(T,b,{source=source,phis=phis,ops=ops,sink=x::sink})
               end
       fun byPos((_,OP{p=p1,...}),(_,OP{p=p2,...})) = p1 < p2
         | byPos _ = error "byPos"
       fun sort {source,phis,ops,sink} =
            {source=source,phis=phis,ops=Sorting.sort byPos ops,sink=sink}
   in  #forall_nodes ssa ins; 
       A.modify sort T;
       T
   end

   (*
    * Graph viewing
    *)
   fun show_val SSA = 
   let val cellClass = cellClass SSA
       val const = const SSA
       val {nameTable,...} = getInfo SSA
       fun lookupName v = 
           let val c = cellClass v
           in  let val (r,i) = HA.sub(nameTable,v)
               in  C.cellToString(r,c)^"."^Int.toString i end
               handle NoName => C.cellToString(v,c)
           end 
       fun reg v =
            if v >= 0 then lookupName v
            else case const v of
                   SP.IMMED i => if i < 0 then "-"^Int.toString(~i)
                                 else Int.toString i 
                 | SP.LABEL l => Label.nameOf l
                 | SP.OPERAND _ => "v"^Int.toString(~v)
   in  reg end

   fun show_op SSA =
   let val show_val = show_val SSA
       val regmap   = CFG.regmap(cfg SSA)
       val asm      = FormatInsn.toString regmap
       val K        = 5
       fun block b = "b"^Int.toString b
       fun regs([b],[v])     = "["^block b^"]"^show_val v
         | regs(b::bs,v::vs) = "["^block b^"]"^show_val v^","^regs(bs,vs)
         | regs _ = ""
       fun regs' vs =
       let fun f(_,[])    = ""
             | f(0,vs)    = "\n   "^f(K,vs)
             | f(n,[v])   = show_val v
             | f(n,v::vs) = show_val v ^","^f(n-1,vs)
       in  f(K,vs) end
       fun show(OP{e,i,s,t,b,...}) = 
             asm i ^" ["^ 
             (case t of 
                 [] => ""
              |  _  => regs' t^" := "  
             )^E.toString (map show_val s) e^"]"
         | show(PHI{preds,t,s,b,...}) = show_val t^" := phi("^regs(preds,s)^")" 
         | show(SOURCE{t,b,...}) = "source["^block b^"]("^regs' t^")"
         | show(SINK{s,b,...}) = "sink["^block b^"]("^regs' s^")"
   in  show
   end

   fun ssaStyle SSA =
   let val show_op  = show_op SSA
       val show_val = show_val SSA
       fun graph _     = []
       fun node(_,bb)  = [L.LABEL(show_op bb)]
       fun edge(_,_,v) = [L.COLOR "red",L.LABEL(show_val v)]
   in  { graph = graph,
         node  = node,
         edge  = edge
       }
   end

   fun cfgStyle SSA =
   let val {graph,node,edge} = CFG.viewStyle(cfg SSA)
       val nodes = nodes SSA
       val show_op = show_op SSA
       fun node(b,b') = 
       let val {source,phis,ops,sink} = A.sub(nodes,b)
           val text = String.concat(map (fn (_,i) => show_op i^"\n") 
                                    (source @ phis @ ops @ sink))
       in  [L.LABEL(CFG.headerText b'^text)]
       end 
   in  { graph = graph,
         node  = node,
         edge  = edge
       }
   end   

   fun displayView(SSA as G.GRAPH ssa) =
   let val defSite = defSite SSA

       fun dst(PHI{t,...}) = [t]
         | dst(OP{t,...}) = t
         | dst(SOURCE{t,...}) = t
         | dst(SINK _) = []

       fun out_edges i = 
       let val defs = dst(#node_info ssa i)
       in  foldr (fn (d,l) => map (fn (_,j,r) => (i,j,r)) 
                      (#out_edges ssa d) @ l) [] defs
       end 

       fun in_edges i = map (fn (i,j,r) => (defSite i,j,r)) (#in_edges ssa i)

       fun forall_edges f = #forall_edges ssa (fn (i,j,e) => f(defSite i,j,e))

       val ssa' = G.GRAPH
       { name            = "SSACFG",
         graph_info      = #graph_info ssa,
         new_id          = G.unimplemented,
         add_node        = G.unimplemented,
         add_edge        = G.unimplemented,
         remove_node     = G.unimplemented,
         set_in_edges    = G.unimplemented,
         set_out_edges   = G.unimplemented,
         set_entries     = G.unimplemented,
         set_exits       = G.unimplemented,
         garbage_collect = G.unimplemented,
         nodes           = #nodes ssa,
         edges           = #edges ssa,
         order           = #order ssa,
         size            = #size ssa,
         capacity        = #capacity ssa,
         out_edges       = out_edges,
         in_edges        = in_edges,
         succ            = fn i => map (fn (_,j,_) => j) (out_edges i),
         pred            = fn j => map (fn (i,_,_) => i) (in_edges j),
         has_edge        = G.unimplemented,
         has_node        = #has_node ssa,
         node_info       = #node_info ssa,
         entries         = #entries ssa,
         exits           = #exits ssa,
         entry_edges     = #entry_edges ssa,
         exit_edges      = #exit_edges ssa,
         forall_nodes    = #forall_nodes ssa,
         forall_edges    = forall_edges 
       }
   in  Subgraph_P_View.subgraph_p_view
         (map #1 (#nodes ssa ()))
         (fn _ => true)
         (fn (i,j) => 
            case (#node_info ssa i, #node_info ssa j) of
               (SOURCE _,SINK _) => false
             | _ => true)
         ssa'
   end

   fun viewAsSSA SSA = L.makeLayout (ssaStyle SSA) (displayView SSA)
   fun viewAsCFG SSA = L.makeLayout (cfgStyle SSA) (cfg SSA)

   (* 
    * Function that replaces all uses of one value with another.
    * The instruction that defines the original value is still around
    * undeleted, but can be deleted subsequently with dead code elimination. 
    *)
   fun replaceAllUses(G.GRAPH ssa) = 
   let fun replace{from,to} =
       let fun rn regs = map (fn r => if r = from then to else r) regs
           fun upd(OP{s,t,i,e,p,b}) = OP{s=rn s,t=t,i=i,e=e,p=p,b=b}
             | upd(PHI{preds,s,t,t',b}) = PHI{preds=preds,s=rn s,t=t,t'=t',b=b}
             | upd(SINK{b,s,s'}) = SINK{b=b,s=rn s,s'=s'}
             | upd(SOURCE _) = error "replaceAllUses"
           fun renameUse(_,j,x) =
               let val ssa_op = #node_info ssa j
               in  #add_edge ssa (to,j,to);
                   #add_node ssa (j,upd ssa_op)
               end
       in  app renameUse (#out_edges ssa from);
           #set_out_edges ssa (from,[]);
           true
       end
   in  replace
   end

   (* 
    * Function that replaces all uses of one value by a copy of another.
    *)
   fun replaceByCopy (G.GRAPH ssa) = 
   let fun replace{from,to} =
       if from = to then false
       else
       let val INFO{cellClass,minPos,maxPos,...} = #graph_info ssa
           val (pos,block,ok) = 
              case #node_info ssa from of
                 OP{p,b,t=[_],...} => (p,b,true)
              |  PHI{b,...} => (!minPos,b,true) before minPos := !minPos - 1
              |  OP{p,b,...} => (p,b,false)
              |  SOURCE{b,...} => (0,b,false) 
              |  SINK{b,...} => error "replaceByCopy"
       in  ok andalso
           let val copy = hd(SP.copies[{class=DA.sub(cellClass,from),
                                       src=to,dst=from}])
               val ssa_op = OP{i=copy,e=E.COPY,s=[to],t=[from],b=block,p=pos}
           in  #add_node ssa (from,ssa_op); true
           end
       end
   in  replace
   end

   (*
    * Fold constant value
    *)
   fun foldConstant(SSA as G.GRAPH ssa) = 
   let val INFO{constTable,defSite,...} = #graph_info ssa
       val show_op = show_op SSA
       val {low,high} = SP.immedRange
       fun fold{value,const} = 
           case HA.sub(constTable,const) of
              SP.IMMED k => 
                let val i = DA.sub(defSite,value)
                in  case #node_info ssa i of
                      OP{e=E.LI,...} => false (* already a constant *)
                    | i' as OP{t=[_],p,b,...} =>
                         if low <= k andalso k <= high then 
                         let val i'' = OP{e=E.LI,t=[value],s=[const],p=p,b=b,
                                          i=SP.loadImmed{immed=k,t=value}
                                         } 
                         in  #set_in_edges ssa (value,[]);
                             #add_node ssa (value,i'');
                             true
                         end
                         else false
                    | _ => false
                end
           |  _ => false
   in  fold  
   end

   (*
    * Set conditional branch
    *)
   fun setBranch(SSA as G.GRAPH ssa) =
   let val label = label SSA 
       fun set{jmp=(i,OP{e=E.BRANCH _,b,p,...}),cond} = 
       let val CFG as G.GRAPH cfg = cfg SSA
           fun change([],es',target,elim) = (es',target,elim)
             | change((i,j,CFG.EDGE{k=CFG.BRANCH cond',w,a})::es,es',x,y) =
                 if cond' = cond then
                    change(es,(i,j,CFG.EDGE{k=CFG.JUMP,w=w,a=a})::es',j,y) 
                 else 
                    change(es,es',x,j)
             | change _ = error "change"
           val outEdges  = #out_edges cfg b
           val (outEdges',target,elim) = change(outEdges,[],~1,~1)
           val lab=CFG.defineLabel(#node_info cfg target)
           val jmp=OP{e=E.JMP(E.ID 0),i=P.jump lab,s=[label lab],t=[],b=b,p=p}
       in  if elim < 0 then error "setBranch: bad edges" else ();
           #add_node ssa (i,jmp);
           #set_in_edges ssa (b,[]);
           #set_out_edges ssa (b,[]);
           #set_out_edges cfg (b,outEdges');
           CFG.changed CFG
       end
         | set _ = error "setBranch"
   in  set 
   end

   (*
    * Move instruction from one block to another
    *)
   fun move(SSA as G.GRAPH ssa) =
   let val INFO{dom,cfg,minPos,maxPos,...} = #graph_info ssa
       val dom = dom cfg
       val dominates = #dominates(Dom.methods dom)
       fun mv{i=(id,OP{e,i,p,b,s,t}),block} = 
             if b = block then ()
             else let val pos = 
                          if dominates(block,b) then (* move upward? *)
                              (maxPos := !maxPos + 1; !maxPos)
                          else
                              (minPos := !minPos - 1; !minPos)
                      val i' = OP{e=e,i=i,p=pos,b=block,s=s,t=t}
                  in  #add_node ssa (id,i')
                  end
         | mv _ = error "move"
   in  mv
   end
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:43  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:43  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:14  pscheng
# *** empty log message ***
#
 *)
