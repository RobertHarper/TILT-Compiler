(*$import *)

structure WNilContext =
  struct 
    fun empty () = ()
  end

structure WNilStatic = 
  struct
    fun module_valid (a,b) = ()
  end

structure NilToWizard = 
  struct
    fun nil_to_wizard m = m
  end