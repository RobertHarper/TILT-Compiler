functor GraphViewerFn(D : GRAPH_DISPLAY) : GRAPH_VIEWER =
struct

   structure L = GraphLayout
   structure G = Graph
   structure FileSys = OS.FileSys

   fun display exec (layout as G.GRAPH l) filename = 
      let val filename  = filename ^ D.suffix()
	  val _     = print("[ "^ #name l^": "^ 
                            D.program() ^ " " ^ filename ^ " ]\n")
          val file  = TextIO.openOut filename
          val out   = fn s => TextIO.output(file,s)
          val _     = D.visualize out layout
          val _     = TextIO.closeOut file
          val _     = exec filename
      in  
          ()
      end handle e => 
        (print("[Uncaught exception in "^exnName e^" graph viewer]\n"); raise e)

   fun system filename = (OS.Process.system 
			   ((D.program()) ^ " " ^ filename);
                          FileSys.remove filename)

   fun view layout = display system layout (FileSys.tmpName())
end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:57  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:10:11  pscheng
# *** empty log message ***
#
 *)
