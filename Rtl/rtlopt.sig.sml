signature RTLOPT = 
  sig
    val GCmerge : Rtl.module -> Rtl.module
    val opt : Rtl.module -> Rtl.module
  end
