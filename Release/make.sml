(*$import Date Time TextIO CommandLine OS Unix String *)

(* A source release is made via "cvs export".  Some files in the CVS
   tree do not need to be released and are deleted.  *)

structure Config
    : sig
	  (* Where to store generated files. *)
	  val destdir : string
	      
	  (* Temporary space. *)
	  val tmpdir : string
	      
	  (* CVS information *)
	  val cvs : string		(* binary *)
	  val cvsroot : string
	  val cvsmodule : string
	  val cvsversion : string
	      
	  (* Tag used to name the constructed files. *)
	  val release : string

	  (* Files and directories that don't need to be released. *)
	  val private : string list
      end =
struct
    val destdir = "/usr0/swasey"
    val tmpdir = "/usr/tmp"

    val cvs = "cvs"
    val cvsroot = "/afs/cs.cmu.edu/project/fox-1/research/cvsroot"
    val cvsmodule = "ml96"
    val cvsversion = "-Dtoday"

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

fun fail (s : string) : 'a =
    let val print = fn s => TextIO.output (TextIO.stdErr, s)
    in
	print (CommandLine.name() ^ ": ");
	print s;
	print "\n";
	OS.Process.exit (OS.Process.failure)
    end

fun execute' (cmd : string list) : OS.Process.status =
    let val name = hd cmd
	val proc = Unix.execute (name, cmd)
	val status = Unix.reap proc
    in  status
    end

fun execute cmd : unit =
    if execute' cmd = OS.Process.success then ()
    else 
	let val cmd = foldr (fn (s,a) => " " :: s :: a) nil cmd
	    val msg = "command returned non-zero exit status:" :: cmd
	in  fail (String.concat msg)
	end

fun join (dir : string) (file : string) : string =
    OS.Path.joinDirFile {dir = dir, file = file}

fun joinExt (ext : string) (file : string) : string =
    OS.Path.joinBaseExt {base=file, ext=SOME ext}

fun remove (path : string) : unit =
    if OS.FileSys.isDir path
	then
	    let val dirstream = OS.FileSys.openDir path
		fun loop acc = (case OS.FileSys.readDir dirstream
				  of "" => acc
				   | path => loop (path :: acc))
		val contents = loop nil
		val _ = OS.FileSys.closeDir dirstream
		val contents = map (join path) contents
	    in  app remove contents;
		OS.FileSys.rmDir path
	    end
    else OS.FileSys.remove path
	
val tempPath : string -> string
    = join Config.tmpdir

val releaseName : string = "tilt-" ^ Config.release
val exportDir   : string = tempPath releaseName

val _ = print "Performing CVS export\n"
val _ = execute [Config.cvs, "-qq", "-d", Config.cvsroot, "export",
		 Config.cvsversion, "-d", exportDir, Config.cvsmodule]

val _ = print "Removing unnecessary files\n"
val _ = app (remove o (join exportDir)) Config.private

val _ = print "Creating tarball\n"
val tarFile = join Config.destdir releaseName
val tarFile = joinExt "tar" tarFile
val tarFile = joinExt "gz" tarFile
val _ = (OS.FileSys.chDir Config.tmpdir;
	 execute ["tar","czf",tarFile,releaseName])
