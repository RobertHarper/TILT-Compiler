(*$import Prelude *)

signature SEQUENCE =
  sig

    type ('a,'b) sequence = ('a * 'b) list

    val length     : ('a,'b) sequence -> int
    val foldl      : (('a*'b) * 'c -> 'c) -> 'c -> ('a,'b) sequence -> 'c (* like foldl *)
    val foldr      : (('a*'b) * 'c -> 'c) -> 'c -> ('a,'b) sequence -> 'c (* like foldl *)
    val foldl_acc  : (('a*'b) * 'c -> ('d*'e) * 'c) -> 'c -> ('a,'b) sequence -> ('d,'e) sequence * 'c
    val map    : (('a*'b) -> ('c*'d)) -> ('a,'b) sequence -> ('c,'d) sequence
    val map2    : (('a*'b) * ('c*'d) -> ('e*'f)) -> (('a,'b) sequence * ('c,'d) sequence) -> ('e,'f) sequence
    val map_second : ('b -> 'c) -> ('a,'b) sequence -> ('a,'c) sequence
    val mapcount : (int * ('a * 'b) -> ('c * 'd)) -> ('a,'b) sequence -> ('c,'d) sequence
    val maptolist : ('a * 'b -> 'c) -> ('a,'b) sequence -> 'c list
    val app    : (('a*'b) -> unit) -> ('a,'b) sequence -> unit
    val app_second : ('b -> unit) -> ('a,'b) sequence -> unit
    val lookup : ('a * 'a -> bool) -> ('a,'b) sequence -> 'a -> 'b option
    val find   : ('a -> bool) -> ('a,'b) sequence -> 'b option
    val all    : (('a *'b) -> bool) -> ('a,'b) sequence -> bool
    val all2    : (('a *'b) * ('c *'d) -> bool) -> (('a,'b) sequence * ('c,'d) sequence) -> bool
    val no_dups : (('a * 'b) * ('a * 'b) -> order) -> ('a,'b) sequence  -> bool

    (* these are slower than the above *)
    val toList   : ('a , 'b) sequence -> ('a * 'b) list
    val fromList : ('a * 'b) list -> ('a , 'b) sequence

  end

