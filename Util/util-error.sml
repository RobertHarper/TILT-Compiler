structure UtilError :> UTIL_ERROR =
struct
    exception BUG of {file : string, msg : string}

    fun error (file : string) (msg : string) : 'a =
	raise BUG {file=file,msg=msg}
end
