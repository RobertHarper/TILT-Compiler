signature SINGLETONELIM =
  sig
    val R_module : Nil.module -> Nil.module
    val erasek : NilContext.context -> Nil.kind -> Nil.kind
  end
