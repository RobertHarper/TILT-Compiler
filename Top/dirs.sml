structure Dirs :> DIRS =
struct

    val error = fn s => Util.error "dirs.sml" s

    type dirs =
	{libDir : string,
	 runtimeDir : string,
	 binDir : string,
	 commDir : string}

    fun inDirByName (parent : string, path : string) : bool =
	let val len = size parent
	in
	    len <= size path andalso
	    String.substring(path, 0, len) = parent
	end

    val cwd : unit -> string =
	Util.memoize OS.FileSys.getDir

    fun realPath (p : string) : string =
	OS.FileSys.realPath p handle _ => p

    fun inDir (parent : string, path : string) : bool =
	(inDirByName (parent, path) orelse
	 inDirByName (realPath parent, realPath path))

    fun dirAvailable (dir : string) : bool =
	(OS.FileSys.isDir dir andalso
	 OS.FileSys.access(dir, [OS.FileSys.A_READ, OS.FileSys.A_EXEC]))
	handle _ => false

    fun from (dir : string) : dirs =
	let val dir = realPath dir
	    val _ = if dirAvailable dir then ()
		    else error ("TILT_LIBDIR directory -- " ^ dir ^ " -- is inaccessible")
	    val curDir = cwd()
	    val commDir = Paths.commDir curDir
	    val _ = File.mkdirs commDir
	    fun join file = OS.Path.joinDirFile{dir=dir,file=file}
	in
	    {libDir=dir,
	     runtimeDir=join "Runtime", binDir=join "Bin",
	     commDir=commDir}
	end

    val getDirs : unit -> dirs =
	Util.memoize (fn () =>
		      case OS.Process.getEnv "TILT_LIBDIR"
		        of NONE => from "" (* or OS.FileSys.getDir() *)
			 | SOME dir => from dir)

    fun libDir () : string = #libDir (getDirs())
    fun runtimeDir () : string = #runtimeDir (getDirs())
    fun binDir () : string = #binDir (getDirs())
    fun commDir () : string = #commDir (getDirs())

end
