structure UtilError :> UTIL_ERROR =
struct
    exception BUG of {file : string, msg : string}
    exception Reject of {msg : string}

    fun error (file : string) (msg : string) : 'a =
	raise BUG {file=file,msg=msg}

    fun reject (msg : string) : 'a =
	raise Reject {msg=msg}

    fun errormsg {file,msg} =  "BUG:" ^ file ^ ": " ^ msg

end
