(*$import Prelude *)

(*Non IL specific utility routines *)
signature UTIL =
  sig

    exception UNIMP

    (* takes error msg *)
    val raise_error : string -> 'a
	
    (* takes filename and then error msg *)
    val error : string -> string -> 'a

    val spaces : int -> string     (* return a string of given length of spaces *)
    val printl : string -> unit    (* print newline after string *)
    val lprint : string -> unit    (* print newline before string *)
    val lprintl : string -> unit   (* print newline before and after string *)

    val printem : string list -> unit

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
    (* If we're running on UNIX, execute command via OS.Process.system.
     * Otherwise put the command in a file called worklist and wait for
     * the file to disappear.  Presumably there is a process on a non-UNIX
     * system monitoring this file and executing commands placed there.
     *)
    val system : string -> bool

  end
