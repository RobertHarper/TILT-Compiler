(*Non IL specific utility routines *)

signature UTIL =
sig

    exception UNIMP

    (* takes filename and then error msg *)
    val error : string -> string -> 'a

    val reject : string -> 'a

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

    val apply : ('a -> 'b) * 'a -> unit -> 'b
    val memoize : (unit -> 'a) -> unit -> 'a

    val run : string list -> unit
    val outputOf : string list -> string

    (*
	(background f) evaulates f() in a child process,
	returning a function isdone such that:

	isdone() = false if child is still running.
		 = true  if child finished successfully
		   (because f termianted or called exit(0)).
		 raises an exception if the child is killed or
		 if f raises an exception or calls exit(nonzero).
    *)
    val background : (unit -> 'a) -> unit -> bool

    (*
	(background' f) evaluates f() in a child process for
	side effects.  Returns a function that kills the process
	running f.
    *)
    val background' : (unit -> 'a) -> unit -> unit

    val min      : real vector -> real	(* raises Domain if |v| = 0 *)
    val max      : real vector -> real	(* raises Domain if |v| = 0 *)
    val mean     : real vector -> real	(* raises Domain if |v| = 0 *)
    val variance : real vector -> real	(* raises Domain if |v| <= 1 *)
    val stddev   : real vector -> real	(* raises Domain if |v| <= 1 *)
    val absdev   : real vector -> real	(* raises Domain if |v| = 0 *)

    structure StringMap : ORD_MAP
	where type Key.ord_key = string

    structure StringSet : ORD_SET
	where type Key.ord_key = string

end
