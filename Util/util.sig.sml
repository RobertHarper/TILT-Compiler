(*$import Prelude *)

(*Non IL specific utility routines *)
signature UTIL =
  sig

    exception UNIMP
    exception BUG of string
    (* takes filename and then error msg *)
    val error : string -> string -> 'a

    (* printl s => print (s^"\n") *)
    val printl : string -> unit
    (* lprint s => print ("\n"^s) *)
    val lprint : string -> unit
    (* lprintl s => print ("\n"^s^"\n") *)
    val lprintl : string -> unit

    (* Misc helpers *)
    val eq_opt : (('a * 'a -> bool) * 'a option * 'a option) -> bool
    val mapopt : ('a -> 'b) -> 'a option -> 'b option
    val split_opt : ('a * 'b) option -> 'a option * 'b option

    val substring : string * string -> int option (* (look for) pattern * (in) target -> present *)
(*
    (* Conversion: strings to/from int/word/char *)
    val IntStr2word : string -> Word32.word
    val WordStr2word : string -> Word32.word
    val IntStr2word64 : string -> Word64.word64
    val WordStr2word64 : string -> Word64.word64
*)
    val CharStr2char : string -> char

    (* oneshot is a ref that can be set only once *)
    type 'a oneshot
    val oneshot       : unit -> 'a oneshot
    val oneshot_init  : 'a   -> 'a oneshot
    val oneshot_set   : 'a oneshot * 'a -> unit
    val oneshot_deref : 'a oneshot -> 'a option
    val eq_oneshot    : 'a oneshot * 'a oneshot -> bool


    val curry2 : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
    val curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)

    val all_pairs : ('a * 'a -> bool) -> 'a list -> bool

    val memoize : (unit -> 'a) -> (unit -> 'a)

    (* system command *)
    (* If the system is UNIX (has a sys command), then execute system and return the success status.
       Otherwise, put the command in a file called worklist.  When the file disappears
          the command returns.  Presumably there is a process on a UNIX system monitoring
	  this file and executing commands placed there. *)
    val system : string -> bool

  end
