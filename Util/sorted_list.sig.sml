signature SLIST =
  sig
    type key

    val compare : key * key -> order

    type 'a slist

    val add : 'a slist -> (key * 'a) -> 'a slist

    val find : 'a slist -> key -> 'a option

    val map : ('a -> 'b) -> 'a slist -> 'b slist

    val empty : unit -> 'a slist

    val fromList : (key * 'a) list -> 'a slist

    val toList : 'a slist -> (key * 'a) list

    val null : 'a slist -> bool

    val merge : ('a slist * 'a slist) -> 'a slist

    val merge_left : ('a slist * 'a slist) -> 'a slist

    val merge_right : ('a slist * 'a slist) -> 'a slist
  end
