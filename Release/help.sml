(*$import TextIO CommandLine OS String Posix *)

signature HELP =
sig
    val fail : string -> 'a

    (* (execute path args) executes program with arguments.  Path must
       be absolute. *)
    val execute : string -> string list -> unit

    (* (join dir file) is dir/file. *)
    val join : string -> string -> string

    (* (joinExt ext file) is file.ext *)
    val joinExt : string -> string -> string

    datatype ty = FILE | DIR
    type ent = ty * string

    (* (expand path) examines the filesystem to find a partially
       sorted list of files and directories reachable from path.  The
       partial order: A directory is listed after its contents.
       Parent and current arcs (eg, .. and . on Unix) are never
       listed.  If path does not exist, the list is empty.  If path
       exists and is a directory, the list will include the directory
       and all its contents.  If path exists and is not a directory,
       the list will be a singleton.  *)
    val expand : string -> ent list
end

structure Help : HELP =
struct
    fun fail (s : string) : 'a =
	let val print = fn s => TextIO.output (TextIO.stdErr, s)
	in
	    print (CommandLine.name() ^ ": ");
	    print s;
	    print "\n";
	    OS.Process.exit (OS.Process.failure)
	end

    fun execute' (path : string) (args : string list) : OS.Process.status =
	(case Posix.Process.fork()
	   of NONE => (Posix.Process.exec (path, OS.Path.file path :: args);
		       Posix.Process.exit 0w127)
	    | SOME pid =>
	       (case #2 (Posix.Process.waitpid (Posix.Process.W_CHILD pid, []))
		  of Posix.Process.W_EXITED => OS.Process.success
		   | _ => OS.Process.failure))

    fun execute (path : string) (args : string list) : unit =
	if execute' path args = OS.Process.success
	    then ()
	else 
	    let val cmd = foldr (fn (s,a) => " " :: s :: a) nil (path :: args)
		val msg = "command returned non-zero exit status:" :: cmd
	    in  fail (String.concat msg)
	    end
	 
    fun join (dir : string) (file : string) : string =
	OS.Path.joinDirFile {dir = dir, file = file}

    fun joinExt (ext : string) (file : string) : string =
	OS.Path.joinBaseExt {base=file, ext=SOME ext}

    datatype ty = FILE | DIR
    type ent = ty * string
    
    fun expand' (path : string, acc : ent list) : ent list =
	if not (OS.FileSys.access (path, [])) then []
	else
	    if OS.FileSys.isDir path
		then
		    let val join = join path
			val dirstream = OS.FileSys.openDir path
			fun loop acc = (case OS.FileSys.readDir dirstream
					  of "" => acc
					   | path => loop ((join path) :: acc))
			val contents = loop nil
			val _ = OS.FileSys.closeDir dirstream
		    in	foldl expand' ((DIR, path) :: acc) contents
		    end
	    else (FILE, path) :: acc
		
    fun expand (path : string) : ent list =
	expand' (path, nil)
end
