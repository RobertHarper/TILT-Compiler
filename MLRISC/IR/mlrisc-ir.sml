functor MLRISC_IRFn
   (structure CFG         : CONTROL_FLOW_GRAPH
    structure CDG         : CONTROL_DEPENDENCE_GRAPH
    structure Loop        : LOOP_STRUCTURE
    structure GraphViewer : GRAPH_VIEWER
    structure Util        : CFG_UTIL
       sharing Loop.Dom = CDG.Dom
       sharing Util.CFG = CFG
   ) : MLRISC_IR =
struct

   structure I    = CFG.I
   structure CFG  = CFG
   structure Dom  = Loop.Dom
   structure CDG  = CDG
   structure Loop = Loop
   structure G    = Graph
   structure A    = Annotations
   structure Util = Util
   structure L    = GraphLayout
  
   type cfg  = CFG.cfg
   type IR   = CFG.cfg
   type dom  = (CFG.block,CFG.edge_info,CFG.info) Dom.dominator_tree
   type pdom = (CFG.block,CFG.edge_info,CFG.info) Dom.postdominator_tree
   type cdg  = (CFG.block,CFG.edge_info,CFG.info) CDG.cdg
   type loop = (CFG.block,CFG.edge_info,CFG.info) Loop.loop_structure

   val layouts = ref [] : (string * (IR -> L.layout)) list ref

   fun addLayout name layout =
   let fun f((x,y)::rest) = if x = name then (x,layout)::rest
                            else (x,y)::f rest
         | f [] = [(name,layout)]
   in  layouts := f(!layouts) end

   fun view name IR =
   let fun f [] = print ("[Can't find "^name^"]\n")
         | f((x,layout)::rest) =
           if x = name then GraphViewer.view (layout IR) else f rest
   in  f(!layouts) end

   fun viewSubgraph IR subgraph = 
         GraphViewer.view (CFG.subgraphLayout{cfg=IR,subgraph=subgraph})

   (*
    * This function defines how we compute a new view 
    *)

   fun memo compute = 
   let val {get,put,rmv,...} = A.new()
       fun getView
           (IR as G.GRAPH{graph_info=CFG.INFO{annotations,...},...} : IR) =
           case get (!annotations) of
              SOME info => info
           |  NONE => let val info = compute IR
                          fun kill() = annotations := rmv(!annotations)
                      in  annotations := 
                             CFG.CHANGEDONCE kill::put(info,!annotations);
                          info
                      end
   in  getView
   end

   (*
    *  Extract various views from an IR
    *) 

   val doms = memo Dom.dominator_trees
   fun dom IR  = #1 (doms IR)
   fun pdom IR = #2 (doms IR)
   val cdg  = memo (fn IR => CDG.control_dependence_graph CFG.cdgEdge (doms IR))
   val loop = memo (Loop.loop_structure o dom)
   val changed = CFG.changed 

   (*
    *  Methods to layout various graphs
    *)
   fun defaultEdge _  = [L.COLOR "red"]
   fun defaultGraph _ = []  
   fun layoutDom' IR G = 
   let val {node,...} = CFG.viewStyle IR
   in  L.makeLayout {edge = defaultEdge,
                     graph= defaultGraph,
                     node = fn (x,Dom.DOM{node=n,...}) => node(x,n)} G
   end
 
   fun layoutDom IR  = layoutDom' IR (dom IR)
   fun layoutPdom IR = layoutDom' IR (pdom IR)
   fun layoutDoms IR = layoutDom' IR
       let val (dom,pdom) = doms IR
       in  GraphCombinations.sum(dom,ReversedGraphView.rev_view pdom)
       end
   fun layoutCDG IR = CFG.viewLayout(cdg IR)
   fun layoutLoop (IR as G.GRAPH cfg) = 
       let val loop = loop IR
           val regmap = CFG.regmap IR
           fun mkNodes nodes =
              String.concat(map (fn i => Int.toString i^" ") nodes)
           fun mkEdges edges = 
              String.concat(map 
                (fn (i,j,_) => Int.toString i^"->"^Int.toString j^" ") edges)
           fun node(_,Loop.LOOP{nesting,header,loop_nodes,
                                backedges,exits,...}) =
               [L.LABEL("nesting: "^Int.toString nesting^"\n"^
                        CFG.show_block regmap (#node_info cfg header)^
                        "loop_nodes: "^mkNodes loop_nodes^"\n"^
                        "backedges: "^mkEdges backedges^"\n"^
                        "exits: "^mkEdges exits^"\n"
                       )]
       in  L.makeLayout {edge=defaultEdge,
                         graph=defaultGraph,
                         node=node} loop
       end
 
   (*
    *  Insert the layout methods here.
    *)
   val _ = addLayout "cfg" CFG.viewLayout
   val _ = addLayout "dom"  layoutDom
   val _ = addLayout "pdom" layoutPdom
   val _ = addLayout "doms" layoutDoms
   val _ = addLayout "cdg"  layoutCDG
   val _ = addLayout "loop" layoutLoop

end


