(*$import *)

(*Pulled out of Util so that Platform can use Error while Util uses Platform.*)
signature UTIL_ERROR =
sig

    exception BUG of string

    val showErrors : bool ref

    (* takes error msg *)
    val raise_error : string -> 'a
	
    (* takes filename and then error msg *)
    val error : string -> string -> 'a

end
