(*$import TopLevel *)

signature SEQUENCE =
  sig

    type ('a,'b) sequence

    val length     : ('a,'b) sequence -> int
    val foldl      : (('a*'b) * 'c -> 'c) -> 'c -> ('a,'b) sequence -> 'c (* like foldl *)
    val foldl_acc  : (('a*'b) * 'c -> ('a*'b) * 'c) -> 'c -> ('a,'b) sequence -> ('a,'b) sequence * 'c
    val map    : (('a*'b) -> ('c*'d)) -> ('a,'b) sequence -> ('c,'d) sequence
    val map2    : (('a*'b) * ('c*'d) -> ('e*'f)) -> (('a,'b) sequence * ('c,'d) sequence) -> ('e,'f) sequence
    val app    : (('a*'b) -> unit) -> ('a,'b) sequence -> unit
    val lookup : ('a * 'a -> bool) -> ('a,'b) sequence -> 'a -> 'b option
    val all    : (('a *'b) -> bool) -> ('a,'b) sequence -> bool

    (* these are slower than the above *)
    val toList   : ('a * 'b) list -> ('a , 'b) sequence
    val fromList : ('a , 'b) sequence -> ('a * 'b) list

  end

