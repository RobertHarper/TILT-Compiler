(*Non IL specific utility routines *)
signature UTIL =
  sig

    exception UNIMP
    exception BUG of string
    (* takes filename and then error msg *)
    val error : string -> string -> 'a


    (* Misc helpers *)
    val eq_opt : (('a * 'a -> bool) * 'a option * 'a option) -> bool
    val mapopt : ('a -> 'b) -> 'a option -> 'b option

(*
    (* Conversion: strings to/from int/word/char *)
    val IntStr2word : string -> Word32.word
    val WordStr2word : string -> Word32.word
    val IntStr2word64 : string -> Word64.word64
    val WordStr2word64 : string -> Word64.word64
*)
    val CharStr2char : string -> char

    (* oneshot is a ref that can be set only once *)
    type '1a oneshot
    val oneshot       : unit -> '1a oneshot
    val oneshot_init  : '1a   -> '1a oneshot
    val oneshot_set   : '1a oneshot * '1a -> unit
    val oneshot_deref : '1a oneshot -> '1a option
    val eq_oneshot    : '1a oneshot * '1a oneshot -> bool

    type ('a,'b) sequence
    type ('a,'b) set
    val list2sequence : ('a * 'b) list -> ('a , 'b) sequence
    val sequence2list : ('a , 'b) sequence -> ('a * 'b) list
    val set2list : ('a , 'b) set  -> ('a * 'b) list
    val list2set : ('a * 'b) list -> ('a , 'b) set
    val setconcat : ('a , 'b) set * ('a, 'b) set -> ('a, 'b) set
    val sequenceconcat : ('a , 'b) sequence * ('a, 'b) sequence -> ('a, 'b) sequence
    val sequence2set : ('a , 'b) sequence -> ('a , 'b) set
    val foldsequence : (('a*'b) * 'c -> 'c) -> 'c -> ('a,'b) sequence -> 'c (* like foldl *)
    val mapsequence : (('a*'b) -> ('c*'d)) -> ('a,'b) sequence -> ('c,'d) sequence
    val mapset : (('a*'b) -> ('c*'d)) -> ('a,'b) set -> ('c,'d) set
    val appsequence : (('a*'b) -> unit) -> ('a,'b) sequence -> unit
    val appset : (('a*'b) -> unit) -> ('a,'b) sequence -> unit
    val set_lookup : ('a * 'a -> bool) -> ('a,'b) set -> 'a -> 'b option
    val sequence_lookup : ('a * 'a -> bool) -> ('a,'b) sequence -> 'a -> 'b option
    val allsequence : (('a *'b) -> bool) -> ('a,'b) sequence -> bool
  end


