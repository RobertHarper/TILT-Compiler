(*$import Prelude *)

(*Non IL specific utility routines *)
signature LISTOPS = 
  sig

    (* different arities of zip and map *)
    val zip  : 'a list -> 'b list -> ('a * 'b) list
    val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
    val zip4 : 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list
    val zip5 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 
                            ('a * 'b * 'c * 'd * 'e) list
    val zip6 : 'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list -> 
                            ('a * 'b * 'c * 'd * 'e * 'f) list

    val unzip : ('a * 'b) list -> ('a list * 'b list)
    val unzip3 : ('a * 'b * 'c) list -> ('a list * 'b list * 'c list)
    val unzip4 : ('a * 'b * 'c * 'd) list -> ('a list * 'b list * 'c list * 'd list)
    val unzip5 : ('a * 'b * 'c * 'd * 'e) list -> ('a list * 'b list * 'c list * 'd list * 'e list)

    val all : ('a -> bool) -> 'a list -> bool
    val all2 : (('a * 'b) -> bool) -> ('a list * 'b list) -> bool
    val all3 : (('a * 'b *'c) -> bool) -> ('a list * 'b list * 'c list) -> bool

    val map : ('a -> 'b) -> 'a list -> 'b list
    val map2 : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val map3 : ('a * 'b * 'c -> 'd) -> 'a list * 'b list * 'c list -> 'd list
    val map4 : ('a * 'b * 'c * 'd -> 'e) -> 
                'a list * 'b list * 'c list * 'd list -> 'e list
    val map5 : ('a * 'b * 'c * 'd * 'e -> 'f) -> 
                'a list * 'b list * 'c list * 'd list * 'e list -> 'f list
    val map6 : ('a * 'b * 'c * 'd * 'e * 'f -> 'g) -> 
                'a list * 'b list * 'c list * 'd list * 'e list * 'f list -> 'g list
    val mapmap    : ('a -> 'b) -> 'a list list -> 'b list list
    val mapmapmap    : ('a -> 'b) -> 'a list list list -> 'b list list list
    val map0count : (int -> 'a) -> int -> 'a list
    val mapcount  : (int * 'a -> 'b) -> 'a list -> 'b list
    val map2count : (int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val map3count : (int * 'a * 'b * 'c -> 'd) -> 'a list * 'b list * 'c list -> 'd list
    val map4count : (int * 'a * 'b * 'c * 'd -> 'e) -> 
                           'a list * 'b list * 'c list * 'd list -> 'e list
    val map5count : (int * 'a * 'b * 'c * 'd * 'e -> 'f) -> 
                           'a list * 'b list * 'c list * 'd list * 'e list -> 'f list
    val map6count : (int * 'a * 'b * 'c * 'd * 'e * 'f -> 'g) -> 
                           'a list * 'b list * 'c list * 'd list * 'e list * 'f list -> 'g list

    val app2 : ('a * 'b -> 'c) -> ('a list * 'b list) -> unit
    val app3 : ('a * 'b * 'c -> 'd) -> ('a list * 'b list * 'c list) -> unit
    (* Misc list helpers *)
    val eq_list : (('a * 'b -> bool) * 'a list * 'b list) -> bool
    val eq_listlist : (('a * 'b -> bool) * 'a list list * 'b list list) -> bool
    (*[0...(n-1)]*)
    val count : int -> int list
    (*[1...n]*)
    val count1 : int -> int list
    val copy : int * 'a -> 'a list
    val member : ''a * ''a list -> bool
    val member_eq : ('a * 'b -> bool) * 'a * 'b list -> bool
    val assoc : (''a * (''a * 'b) list) -> 'b option
    val assoc_eq : (('a * 'a -> bool) * 'a * ('a * 'b) list) -> 'b option
    val list_diff  : ''a list * ''a list -> ''a list
    val list_diff_eq  : (('a * 'a -> bool) * 'a list * 'a list) -> 'a list
    val list_inter : ''a list * ''a list -> ''a list
    val list_inter_eq : ('a * 'a -> bool) * 'a list * 'a list -> 'a list
    val butlast : 'a list -> 'a list
    (* these are all left to right *)
    val andfold : ('a -> bool) -> 'a list -> bool
    val orfold : ('a -> bool) -> 'a list -> bool
    val andfold' : ('a * 'b -> (bool * 'b)) -> 'b -> 'a list -> bool
    val orfold' : ('a * 'b -> (bool * 'b)) -> 'b -> 'a list -> bool

    val flatten : 'a list list -> 'a list
    val transpose : 'a list list -> 'a list list

    val map_first : ('a -> 'b) -> ('a * 'c) list -> ('b * 'c) list
    val map_second : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list

    (* foldl_acc f s l => (l',s') where s' is equivalent to the result of
      * List.foldl (fn x => #2(f x)) s l
      * and l' is the accumulated list of the second result of f.
      * So foldl_acc (fn (x,y) => (x*x,y+x*x)) 0 [2,0,3,1] => ([4,0,9,1],14)
      *  that is, the squares of each element, and the sum of the squares
      *)
    val foldl_acc : ('a * 'b -> 'c * 'b) -> 'b -> 'a list -> 'c list * 'b

    val foldl_acc2 : ('e1 * 'e2 * 'state -> 'd1 * 'd2 * 'state) 
                         -> 'state -> 'e1 list * 'e2 list -> 'd1 list * 'd2 list * 'state

    val foldl2 : ('a * 'b * 'state -> 'state) -> 'state -> ('a list * 'b list) -> 'state
    val foldl3 : ('a * 'b * 'c * 'state -> 'state) -> 'state -> ('a list * 'b list * 'c list) -> 'state
    val foldl4 : ('a * 'b * 'c * 'd  * 'state -> 'state) -> 'state -> ('a list * 'b list * 'c list * 'd list) -> 'state

    val foldl_list : ('a * 'b -> 'c * 'b) -> 'b -> 'a list -> 'c list * 'b
    val eq_len : 'a list * 'b list -> bool
    val eq_len3 : 'a list *'b list * 'c list -> bool
    val split : 'a list -> 'a list * 'a
    val opt_cons : 'a -> ('a list option) -> 'a list 
    val find2 : ('a * 'b -> bool) -> ('a list * 'b list) -> ('a * 'b) option
    val insertion_sort : ('a * 'a -> order) -> 'a list -> 'a list
    val no_dups : ('a * 'a -> order) -> 'a list -> bool

   (* catenable lists *)
   datatype 'a catlist =
       LIST of 'a list
     | CONS of 'a * 'a catlist
     | APPEND of 'a catlist list
     | SNOC of 'a catlist * 'a
     | SINGLETON of 'a
     | NIL

   val flattenCatlist : 'a catlist -> 'a list

   (*Intersperse the elements of the list with a separator
    *)
   val join : 'a -> 'a list -> 'a list
  end
