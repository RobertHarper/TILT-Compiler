(*$import TopLevel RUN *)

structure Run : RUN =
struct

    exception ArgErr
    
    (* exit : int -> 'a *)
    fun exit n = Posix.Process.exit (Word8.fromInt n)
	
    (* getenv : string -> string *)
    fun getenv name =
	(case OS.Process.getEnv name
	   of NONE => raise ArgErr
	    | SOME v => v)

    (* atoi : string -> int *)
    fun atoi s =
	(case Int.fromString s
	   of NONE => raise ArgErr
	    | SOME n => n)
	     
    (* cmd : unit -> string *)
    fun cmd () = getenv "TILT_CMD"
	
    (* args : unit -> string list *)
    fun args () =
	let val nargs = atoi (getenv "TILT_NARGS")
	    fun arg n = getenv ("TILT_ARG_" ^ Int.toString n)
	    fun get 0 acc = acc
	      | get n acc = let val n' = n-1
			    in  get n' (arg n' :: acc)
			    end
	in
	    get nargs nil
	end

    (* run : (string * string list -> OS.Process.status) -> unit *)
    fun run {main, main'} =
	let val status = ((main (cmd(), args())
			   handle ArgErr => main' ())
			  handle _ => OS.Process.failure)
	in
	    if status = OS.Process.success
		then ()
	    else exit 1
	end
end
