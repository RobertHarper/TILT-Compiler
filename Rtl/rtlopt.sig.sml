signature RTLOPT = 
  sig
    structure Rtl : RTL
    val GCmerge : Rtl.module -> Rtl.module
    val opt : Rtl.module -> Rtl.module
  end
