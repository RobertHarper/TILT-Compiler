signature BBSCHED = sig
  structure F : FLOWGRAPH

  val bbsched : F.cluster -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end
