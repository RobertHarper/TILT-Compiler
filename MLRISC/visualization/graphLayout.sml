structure GraphLayout =
struct

   exception LABEL of string
   exception COLOR of string
   exception NODE_COLOR of string
   exception EDGE_COLOR of string
   exception TEXT_COLOR of string
   exception ARROW_COLOR of string
   exception BACKARROW_COLOR of string
   exception BORDER_COLOR of string
   exception BORDERLESS 
   exception ALGORITHM of string
   exception EDGEPATTERN of string

   type annotations = Annotations.annotations

   type ('n,'e,'g) style = 
      { edge  : 'e Graph.edge -> annotations,
        node  : 'n Graph.node -> annotations,
        graph : 'g -> annotations
      }

   type layout = (annotations, annotations, annotations) Graph.graph

   fun makeLayout {node,edge,graph} G = 
       IsomorphicGraphView.map node edge graph G

end

