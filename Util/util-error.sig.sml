signature UTIL_ERROR =
sig

    exception BUG of {file : string, msg : string}
    exception Reject of {msg : string}

    (* takes filename and then error msg *)
    val error : string -> string -> 'a

    val reject : string -> 'a

    val errormsg : {file : string, msg : string} -> string

end
