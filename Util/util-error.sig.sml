signature UTIL_ERROR =
sig

    val Interactive : bool ref
    exception BUG of {file : string, msg : string}
    exception Reject of {msg : string}
    exception BadMagicNumber of string	(* file *)

    (* takes filename and then error msg *)
    val error : string -> string -> 'a

    val reject : string -> 'a

    val errormsg : exn -> string
    val print : exn -> unit
    val print_and_exit : exn -> 'a
end
