functor AllDisplaysFn (val viewer : string ref) : 
sig
   include GRAPH_DISPLAY
   val viewer : string ref
end =
struct

   val viewer = viewer

   fun visualize print =
       (case !viewer of
          "daVinci" => daVinci.visualize print
        | "vcg"     => VCG.visualize print
        | _         => daVinci.visualize print
       )

   fun program() =
       (case !viewer of
          "daVinci" => daVinci.program()
        | "vcg"     => VCG.program()
        | _         => daVinci.program()
       )

   fun suffix() =
       (case !viewer of
          "daVinci" => daVinci.suffix()
        | "vcg"     => VCG.suffix()
        | _         => daVinci.suffix()
       )

end
