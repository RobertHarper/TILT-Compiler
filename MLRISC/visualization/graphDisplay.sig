signature GRAPH_DISPLAY =
sig

   val suffix    : unit -> string
   val program   : unit -> string
   val visualize : (string -> unit) -> GraphLayout.layout -> unit

end
