signature RTLTOASM =
sig
   val msgs        : bool ref
   val debug       : bool ref
   val show_labels : bool ref

   val allocateModule : Rtl.module -> unit

end

