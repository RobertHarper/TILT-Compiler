(*$import Prelude Int *)

signature SIG =
sig
    type key
    type 'a table
    val empty : 'a table
    val insert : 'a table * key * 'a -> 'a table
end;

functor Fn (type key'
	    val compare : key' * key' -> order)
    :> SIG where type key = key' =
struct
    type key = key'
    datatype 'a table = EMPTY
    val empty = EMPTY
    val insert = fn _ => EMPTY
end;

structure Str = Fn (type key' = int
		    val compare = Int.compare);
