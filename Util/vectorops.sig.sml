signature VECTOROPS = 
  sig
    val maplist    : ('a -> 'b ) -> 'a vector -> 'b list
    val mapPartialList : ('a -> 'b option) -> 'a vector -> 'b list
    val mapilist    : (int * 'a -> 'b ) -> 'a vector -> 'b list
    val mapiPartialList : (int * 'a -> 'b option) -> 'a vector -> 'b list
  end