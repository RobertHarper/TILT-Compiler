signature LILOPTIMIZE =
  sig
      val debug : bool ref
      (* Print debug information *)

      val chatlev  : int ref
      val debuglev : int ref

      val optimize :  {doCse : bool} -> Lil.module -> Lil.module
  end
