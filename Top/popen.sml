(*$import POPEN TextIO OS Util *)

structure Popen : POPEN =
struct
    val error = fn s => Util.error "popen.sml" s

    (* withFile : (TextIO.instream -> 'a) -> string -> 'a *)
    fun withFile f path =
	let val file = TextIO.openIn path
	    val result = f file handle e => (TextIO.closeIn file; raise e)
	    val _ = TextIO.closeIn file
	in
	    result
	end
	
    (* readFile : string -> string *)
    val readFile = withFile TextIO.inputAll


    (* withTmpName : (string -> 'a) -> 'a. *)
    fun withTmpName f =
	let val tmp = OS.FileSys.tmpName()
	    val result = f tmp handle e => (OS.FileSys.remove tmp; raise e)
	    val _ = OS.FileSys.remove tmp
	in
	    result
	end

    (* outputOf : string -> string *)
    fun outputOf command =
	let fun gather tmp = if Util.system ("exec >" ^ tmp ^ "; " ^ command)
				 then readFile tmp
			     else error ("non-zero exit status runnnig " ^ command)
	in
	    withTmpName gather
	end
end    
