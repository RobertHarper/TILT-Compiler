signature ORD_KEY =
  sig
  end

signature ORD_SET =
  sig

    structure Key : ORD_KEY

    type item

    val foldl : item -> 'b -> unit

  end

functor RedBlackSetFn (K : ORD_KEY) :> ORD_SET where Key = K =
  struct

    structure Key = K

    type item = int

    fun foldl f x = ()
  end;
