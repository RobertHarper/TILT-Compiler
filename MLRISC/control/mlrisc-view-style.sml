signature MLRISC_GRAPH_STYLE = sig
   val node_color : string ref
   val edge_color : string ref
end

signature MLRISC_VIEW_STYLE = sig
   val font       : string ref
   val background : string ref
   val foreground : string ref
   val viewer     : string ref
   val vcg        : string ref
   val daVinci    : string ref

   structure CPS      : MLRISC_GRAPH_STYLE
   structure CFG      : MLRISC_GRAPH_STYLE
   structure DOM      : MLRISC_GRAPH_STYLE
   structure CDG      : MLRISC_GRAPH_STYLE
   structure Interval : MLRISC_GRAPH_STYLE
   structure Region   : MLRISC_GRAPH_STYLE
   structure RA       : MLRISC_GRAPH_STYLE
end

functor NewStyle() = struct 
  val node_color = ref "white"
  val edge_color = ref "red"
end

structure MLRISC_ViewStyle : MLRISC_VIEW_STYLE =
struct
   val font       = ref ""
   val foreground = ref ""
   val background = ref ""
   val viewer     = ref "daVinci"
   val vcg        = ref "xvcg -font 10x20" 
   val daVinci    = ref "daVinci -font 10x20"

   structure CPS      = NewStyle()
   structure CFG      = NewStyle()
   structure DOM      = NewStyle()
   structure CDG      = NewStyle()
   structure Interval = NewStyle()
   structure Region   = NewStyle()
   structure RA       = NewStyle()
end

(*
 * $Log$
# Revision 1.1  99/02/17  22:33:27  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:02  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
