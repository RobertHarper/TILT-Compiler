signature UTIL_ERROR =
sig

    exception BUG of {file : string, msg : string}

    (* takes filename and then error msg *)
    val error : string -> string -> 'a

end
