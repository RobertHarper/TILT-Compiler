signature RTLTOASM =
sig

   structure Rtl : RTL

   val msgs        : bool ref
   val debug       : bool ref
   val knowns      : bool ref
   val show_labels : bool ref

   val allocateModule : Rtl.module -> unit
   val dumpEntryTables : Rtl.local_label list -> unit
end

