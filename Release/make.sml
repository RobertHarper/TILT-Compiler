(*$import Date Time TextIO CommandLine OS Unix String Posix *)

(* A source release is made via "cvs export".  Some files in the CVS
   tree do not need to be released and are deleted.  *)

fun fail (s : string) : 'a =
    let val print = fn s => TextIO.output (TextIO.stdErr, s)
    in
	print (CommandLine.name() ^ ": ");
	print s;
	print "\n";
	OS.Process.exit (OS.Process.failure)
    end

structure Config
    : sig
	  (* binary locations *)
	  val cvs : string
	  val tar : string
	      
	  (* Where to store generated files. *)
	  val destdir : string
	      
	  (* Temporary space. *)
	  val tmpdir : string
	      
	  (* CVS information *)
	  val cvsroot : string
	  val cvsmodule : string
	  val cvsversion : string
	      
	  (* Tag used to name the constructed files. *)
	  val release : string

	  (* Files and directories that don't need to be released. *)
	  val private : string list
      end =
struct
    val cvs = "/usr/local/bin/cvs"
    val tar = "/usr/local/bin/gtar"
	
    val tmpdir = "/usr/tmp"
    val destdir = tmpdir

    (* Get repository and version from command line. *)
    val (cvsroot, cvsversion) =
	(case CommandLine.arguments()
	   of [a,b] => (a,b)
	    | _ => fail "expected CVS root and module version (eg, -Dtoday or -rTag) as arguments")
    val cvsmodule = "ml96"

    (* Base release tag on today's date; eg, 20020514. *)
    val release = Date.fmt "%Y%m%d" (Date.fromTimeUniv (Time.now()))

    val private =
	[
	 "Apps","Bench","BenchData",
	 "Bin/makeRelease",
	 "Bin/run",
	 "Doc/buglist.txt",
	 "Doc/issue.txt",
	 "Doc/log.txt",
	 "Doc/overview.txt",		(* need updating *)
	 "Doc/tm.txt",
	 "MLRISC",
	 "Preludes",
	 "README.CMU",
	 "Release",
	 "RtlToMLRISC",
	 "Self",
	 "Wizard",
	 "icfp"
	 ]
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

val tempPath : string -> string
    = join Config.tmpdir

val releaseName : string = "tilt-" ^ Config.release
val exportDir   : string = tempPath releaseName

val _ = print "Performing CVS export\n"
val _ = if OS.FileSys.access (exportDir, [])
	    then print "(CVS export unnecessary)\n"
	else execute Config.cvs ["-qq", "-d", Config.cvsroot, "export",
				 Config.cvsversion, "-d", exportDir, Config.cvsmodule]

val _ = print "Removing unnecessary files\n"
fun removeEnt ((ty, path) : ent) : unit =
    (case ty
       of FILE => OS.FileSys.remove
	| DIR => OS.FileSys.rmDir) path

val remove : string -> unit =
    (app removeEnt) o expand

val _ = app (remove o (join exportDir)) Config.private

val _ = print "Creating tarball\n"
val tarFile = join Config.destdir releaseName
val tarFile = joinExt "tar" tarFile
val tarFile = joinExt "gz" tarFile
val _ = (OS.FileSys.chDir Config.tmpdir;
	 remove tarFile;
	 execute Config.tar ["czf",tarFile,releaseName])
