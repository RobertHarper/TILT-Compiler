(* Top-level types and values. *)

(* See the Top-Level Environment chapter of the basis library spec. *)

signature TOP =
sig

    eqtype unit
    eqtype int
    eqtype word
    type real
    eqtype char
    eqtype string
    type substring
    type exn
    eqtype 'a array
    eqtype 'a vector
    eqtype 'a ref
    datatype bool = datatype bool
    datatype 'a option = NONE | SOME of 'a
    datatype order = LESS | EQUAL | GREATER
    datatype list = datatype list

    exception Bind
    exception Chr
    exception Div
    exception Domain
    exception Empty
    exception Fail of string
    exception Match
    exception Option
    exception Overflow
    exception Size
    exception Span
    exception Subscript

    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val before : 'a * unit -> 'a
    val ignore : 'a -> unit
    val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
    val exnName : exn -> string
    val exnMessage : exn -> string
    val getOpt : 'a option * 'a -> 'a
    val isSome : 'a option -> bool
    val valOf : 'a option -> 'a
    val not : bool -> bool
    val real : int -> real
    val trunc : real -> int
    val floor : real -> int
    val ceil : real -> int
    val round : real -> int
    val ord : char -> int
    val chr : int -> char
    val size : string -> int
    val str : char -> string
    val concat : string list -> string
    val implode : char list -> string
    val explode : string -> char list
    val substring : string * int * int -> string
    val ^ : string * string -> string
    val null : 'a list -> bool
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val length : 'a list -> int
    val rev : 'a list -> 'a list
    val @ : 'a list * 'a list -> 'a list
    val app : ('a -> unit) -> 'a list -> unit
    val map : ('a -> 'b) -> 'a list -> 'b list
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val print : string -> unit
    val vector : 'a list -> 'a vector
    val use : string -> unit
end
