functor ControlFlowGraphFn
   (structure I : INSTRUCTIONS
    structure B : BLOCK_NAMES
    structure P : PSEUDO_OPS
    structure W : FIXED_POINT
    structure GraphImpl : GRAPH_IMPLEMENTATION
    structure Asm : EMITTER_NEW
       sharing Asm.I = I
       sharing Asm.P = P
   ) : CONTROL_FLOW_GRAPH =
struct

    structure I = I
    structure B = B
    structure P = P
    structure C = I.C
    structure W = W
    structure G = Graph
    structure L = GraphLayout
    structure A = Annotations
   
    type weight = W.fixed_point

    datatype block_kind = 
        START          (* entry node *)
      | STOP           (* exit node *)
      | FUNCTION_ENTRY (* for SSA transformations *)
      | NORMAL         (* normal node *)
      | HYPERBLOCK     (* hyperblock *)

    and data = LABEL  of Label.label
             | PSEUDO of P.pseudo_op
 
    and block = 
       BLOCK of
       {  id          : int,                        (* block id *)
          name        : B.name,                     (* block name *)
          kind        : block_kind,                 (* block kind *)
          freq        : weight ref,                 (* execution frequency *) 
          data        : data list ref,              (* data preceeding block *) 
          labels      : Label.label list ref,       (* labels on blocks *) 
          insns       : I.instruction list ref,     (* in rev order *)
          annotations : Annotations.annotations ref (* annotations *)
       }

    and edge_kind = ENTRY           (* entry edge *) 
                  | EXIT            (* exit edge *)
                  | JUMP            (* unconditional jump *)
                  | FALLSTHRU       (* falls through to next block *)  
                  | BRANCH of bool  (* branch *) 
                  | SWITCH of int   (* computed goto *)   
                  | SIDEEXIT of int (* side exit *)   
   
    and edge_info = EDGE of { k : edge_kind,                  (* edge kind *)
                              w : weight ref,                 (* edge freq *)
                              a : Annotations.annotations ref (* annotations *)
                            }

    type edge = edge_info Graph.edge
    type node = block Graph.node

    datatype info = 
        INFO of { regmap      : C.regmap,
                  annotations : Annotations.annotations ref,
                  firstBlock  : int ref,
                  reorder     : bool ref
                }

    type cfg = (block,edge_info,info) Graph.graph

   (*========================================================================
    *
    *  Various kinds of annotations 
    *
    *========================================================================*)
    exception LIVEOUT of C.cellset  (* escaping live out information *)
    exception CHANGED of unit -> unit
    exception CHANGEDONCE of unit -> unit

   (*========================================================================
    *
    *  Methods for manipulating basic blocks
    *
    *========================================================================*)
    fun defineLabel(BLOCK{labels=ref(l::_),...}) = l
      | defineLabel(BLOCK{labels,...}) = let val l = Label.newLabel ""
                                         in  labels := [l]; l end

    fun newBlock'(id,kind,name,insns) =
        BLOCK{ id          = id,
               kind        = kind,
               name        = name,
               freq        = ref W.zero,
               data        = ref [],
               labels      = ref [],
               insns       = ref insns,
               annotations = ref []
             }

    fun copyBlock(id,BLOCK{kind,name,freq,data,labels,insns,annotations,...}) =
        BLOCK{ id          = id,
               kind        = kind,
               name        = name,
               freq        = ref W.zero,
               data        = ref (!data),
               labels      = ref [],
               insns       = ref (!insns),
               annotations = ref (!annotations) 
             }

    fun newBlock(id,name) = newBlock'(id,NORMAL,name,[])
    fun newStart(id) = newBlock'(id,START,B.default,[])
    fun newStop(id) = newBlock'(id,STOP,B.default,[])
    fun newFunctionEntry(id) = newBlock'(id,FUNCTION_ENTRY,B.default,[])

   (*========================================================================
    *
    *  Emit a basic block
    *
    *========================================================================*)
    fun kindName START          = "START"
      | kindName STOP           = "STOP"
      | kindName HYPERBLOCK     = "Hyperblock"
      | kindName FUNCTION_ENTRY = "Entry"
      | kindName NORMAL         = "Block"

    fun nl() = TextIO.output(!AsmStream.asmOutStream,"\n")
    fun comment msg = (Asm.comment msg; nl())

    fun emitHeader (BLOCK{id,kind,freq,annotations,...}) = 
       (comment(kindName kind ^"["^Int.toString id^
                    "] ("^W.toString (!freq)^")");
        app (fn A.COMMENT msg => comment msg | _ => ()) (!annotations)
       ) 

    fun emitFooter (BLOCK{annotations,...}) = 
        (case A.get (fn LIVEOUT x => SOME x | _ => NONE) (!annotations) of
            SOME s => 
            let val regs = String.tokens Char.isSpace(C.cellset2string s)
                val K = 7
                fun f(_,[],s,l)    = s::l
                  | f(0,vs,s,l)    = f(K,vs,"   ",s::l)
                  | f(n,[v],s,l)   = v^s::l
                  | f(n,v::vs,s,l) = f(n-1,vs,s^" "^v,l)
                val text = rev(f(K,regs,"",[]))
            in  app comment text
            end
         |  NONE => ()
        ) handle Overflow => print("Bad footer\n")

    fun emit regmap (block as BLOCK{insns,data,labels,...}) =
       (emitHeader block;
        app (fn PSEUDO p => Asm.pseudoOp p
              | LABEL l  => Asm.defineLabel l) (!data);
        app Asm.defineLabel (!labels);
        app (fn i => Asm.emitInstr(i,regmap)) (rev (!insns));
        emitFooter block
       )
 
   (*========================================================================
    *
    *  Methods for manipulating CFG
    *
    *========================================================================*)
    fun cfg info = GraphImpl.graph("CFG",info,10)
    fun new regmap =
        let val info = INFO{ regmap = regmap,  
                             annotations = ref [],
                             firstBlock  = ref 0,
                             reorder     = ref false
                           }
        in  cfg info end

    fun subgraph(CFG as G.GRAPH{graph_info=INFO graph_info,...}) =
        let val info = INFO{ regmap      = #regmap graph_info,
                             annotations = ref [],
                             firstBlock  = #firstBlock graph_info,
                             reorder     = #reorder graph_info
                           }
        in  UpdateGraphInfo.update CFG info end

    fun init(G.GRAPH cfg) =
        if #order cfg () = 0 then 
           let val i     = #new_id cfg ()
               val start = newStart i
               val _     = #add_node cfg (i,start)
               val j     = #new_id cfg ()
               val stop  = newStop i
               val _     = #add_node cfg (j,stop) 
           in  #add_edge cfg (i,j,EDGE{k=ENTRY,w=ref W.zero,a=ref []});
               #set_entries cfg [i];
               #set_exits cfg [j]
           end
        else ()

    fun changed(G.GRAPH{graph_info=INFO{reorder,annotations,...},...}) = 
        let fun process((a as CHANGED f)::l) = (f(); a::process l)
              | process(CHANGEDONCE f::l) = (f(); process l)
              | process(a::l) = a::process l
              | process [] = []
        in  annotations := process(!annotations);
            reorder := true
        end 

    fun regmap(G.GRAPH{graph_info=INFO{regmap,...},...}) = regmap
    fun reglookup cfg =
        let val regmap = regmap cfg
            val look   = Intmap.map regmap
            fun lookup r = look r handle _ => r
        in  lookup end
    fun get f (BLOCK{annotations,...}) = A.get f (!annotations)
    fun liveOut b = 
         case get (fn LIVEOUT x => SOME x | _ => NONE) b of
            SOME s => s
         |  NONE => C.empty
    fun fallsThruFrom(G.GRAPH cfg,b) =
        let fun f [] = NONE
              | f((i,_,EDGE{k=BRANCH false,...})::_) = SOME i
              | f((i,_,EDGE{k=FALLSTHRU,...})::_) = SOME i
              | f(_::es) = f es
        in  f(#in_edges cfg b)
        end
    fun fallsThruTo(G.GRAPH cfg,b) =
        let fun f [] = NONE
              | f((_,j,EDGE{k=BRANCH false,...})::_) = SOME j
              | f((_,j,EDGE{k=FALLSTHRU,...})::_) = SOME j
              | f(_::es) = f es
        in  f(#out_edges cfg b)
        end
    fun removeEdge CFG (i,j,EDGE{a,...}) =
        Graph.remove_edge' CFG (i,j,fn EDGE{a=a',...} => a = a')

   (*========================================================================
    *
    *  Miscellaneous 
    *
    *========================================================================*)
   fun cdgEdge(EDGE{k, ...}) = 
        case k of
           (JUMP | FALLSTHRU) => false
        |  _ => true

   (*========================================================================
    *
    *  Pretty Printing and Viewing 
    *
    *========================================================================*)
   fun show_edge(EDGE{k,w,a,...}) = 
       let val kind = case k of
                         JUMP      => ""
                      |  FALLSTHRU => "fallsthru"
                      |  BRANCH b => Bool.toString b
                      |  SWITCH i => Int.toString i
                      |  ENTRY    => "entry"
                      |  EXIT     => "exit"
                      |  SIDEEXIT i => "sideexit("^Int.toString i^")"
           val weight = "(" ^ W.toString (!w) ^ ")"
       in  kind ^ weight 
       end 

   fun getString f x = 
   let val buffer = StringStream.mkStreamBuf()
       val S      = StringStream.openStringOut buffer
       val _      = AsmStream.withStream S f x 
   in  StringStream.getString buffer end

   fun show_block regmap block = 
   let val text = getString (emit regmap) block
   in  foldr (fn (x,"") => x | (x,y) => x^" "^y) ""
            (String.tokens (fn #" " => true | _ => false) text)
   end

   fun headerText block = getString emitHeader block
   fun footerText block = getString emitFooter block

   fun edgeStyle(i,j,e as EDGE{k,a,...}) = 
   let val a = L.LABEL(show_edge e) :: !a
   in  case k of 
         (ENTRY | EXIT) => L.COLOR "green" :: a
       | _ => L.COLOR "red" :: a
   end 

   fun viewStyle cfg =
   let val regmap = regmap cfg
       fun node (_,b as BLOCK{annotations,...}) = 
            L.LABEL(show_block regmap b) :: !annotations
   in  { graph = fn _ => [],
         edge  = edgeStyle,
         node  = node
       } 
   end

   fun viewLayout cfg = L.makeLayout (viewStyle cfg) cfg

   fun subgraphLayout {cfg,subgraph = G.GRAPH subgraph} =
   let val regmap = regmap cfg
       fun node(n,b as BLOCK{annotations,...}) = 
          if #has_node subgraph n then
             L.LABEL(show_block regmap b) :: !annotations
          else
             L.COLOR "lightblue"::L.LABEL(headerText b) :: !annotations
       fun edge(i,j,e) = 
            if #has_edge subgraph (i,j) then edgeStyle(i,j,e)
            else [L.EDGEPATTERN "dotted"]
   in  L.makeLayout {graph = fn _ => [],
                     edge  = edge,
                     node  = node} cfg
   end

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:32  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:05:59  pscheng
# *** empty log message ***
#
 *)
