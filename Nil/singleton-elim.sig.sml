signature SINGLETONELIM =
  sig
    val R_module : Nil.module -> Nil.module
    val R_interface : Nil.interface -> Nil.interface
    val erasek : NilContext.context -> Nil.kind -> Nil.kind
  end
