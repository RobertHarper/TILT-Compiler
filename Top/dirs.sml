(*$import TopLevel DIRS Delay Util *)

structure Dirs =
struct

    (* error : string -> 'a *)
    val error = fn s => Util.error "dirs.sml" s
	
    (* relative : string * string -> string *)
    (* could be smarter about symbolic links *)
    fun relative (dir, file) =
	let val file' = if OS.Path.isRelative file
			    then OS.Path.joinDirFile {dir=dir, file=file}
			else file
	in
	    OS.Path.mkCanonical file'
	end

    (* accessPath : string list * OS.FileSys.access_mode list -> string -> string option *)
    fun accessPath (dirs, perms) file =
	let
	    (* join : string -> string *)
	    fun join dir = relative (dir, file)
		    
	    (* search : string -> bool *)
	    fun search dir =
		let val file' = join dir
		in  (OS.FileSys.access (file', perms) handle _ => false)
		end
	in
	    Option.compose (join, List.find search) dirs
	end

    datatype dirs = DIRS of {system : string -> bool,
			     stripLib : string -> string option,
			     libDir : string,
			     runtimeDir : string,
			     binDir : string,
			     commDir : string}

    (* mkdir : string -> unit *)
    fun mkdir dir = if (OS.FileSys.isDir dir
			handle _ => false)
			then ()
		    else OS.FileSys.mkDir dir

    (* inDirByName : string * string -> bool. *)
    fun inDirByName (parent, path) =
	let val len = size parent
	in
	    len <= size path andalso
	    String.substring(path, 0, len) = parent
	end

    val cwd : string Delay.value =
	Delay.delay OS.FileSys.getDir

    (* realFullPath : string -> string *)
    fun realFullPath p =
	let val p' = relative (Delay.force cwd, p)
	in
	    OS.FileSys.realPath p'
	    handle _ => p'
	end
	
    (* inDir : string * string -> bool *)
    fun inDir (parent, path) = (inDirByName (parent, path) orelse
				inDirByName (realFullPath parent, realFullPath path))

    (* dirAvailable : string -> bool *)
    fun dirAvailable dir = ((OS.FileSys.isDir dir andalso
			     OS.FileSys.access(dir, [OS.FileSys.A_READ, OS.FileSys.A_EXEC]))
			    handle _ => false)

    (* chopSlash : string -> string *)
    fun chopSlash s = if size s > 0 andalso String.sub (s, 0) = #"/"
			  then String.extract (s, 1, NONE)
		      else s

    (* sysDir : string -> bool *)
    fun sysDir dir =
	let val sysDirs = ["/tmp", "/usr/tmp", "/usr0/tmp", (* For testing purposes *)
			   "/usr/lib", "/usr/local/lib"]
	in
	    List.exists (fn s => inDir (s, dir)) sysDirs
	end
    
    (* from : string -> dirs *)
    fun from dir =
	let val dir = realFullPath dir
	    val _ = if dirAvailable dir then ()
		    else error ("TILT_LIBDIR directory -- " ^ dir ^ " -- is inaccessible")
	    val sys = sysDir dir
	    fun system file = sys andalso inDir (dir, file)
	    val curDir = Delay.force cwd
	    val len = size dir
	    fun stripLib file = if inDirByName (dir, file)
				    then SOME (chopSlash (String.extract (file, len, NONE)))
				else if OS.Path.isRelative file
					 then stripLib (relative (curDir, file))
				     else NONE
	    val commDir = relative (curDir, "TempCommunication")
	    val _ = mkdir commDir
	    fun join file = relative (dir, file)		
	in
	    DIRS {system=system, stripLib=stripLib, libDir=dir,
		  runtimeDir=join "Runtime", binDir=join "Bin",
		  commDir=commDir}
	end

    val dirs : dirs Delay.value = (* Memoize - not strictly necessary *)
	Delay.delay (fn () =>
		     case OS.Process.getEnv "TILT_LIBDIR"
		       of NONE => from "" (* or OS.FileSys.getDir() *)
			| SOME dir => from dir)
	
    (* getDirs : unit -> dirs *)
    fun getDirs () = Delay.force dirs
	
    (* isSystemFile : dirs * string -> bool *)
    fun isSystemFile (DIRS {system,...}, file) = system file
	
    (* stripLibDir : dirs * string -> string option *)
    fun stripLibDir (DIRS {stripLib,...}, file) = stripLib file

    (* get*Dir : dirs -> string *)
    fun getLibDir     (DIRS {libDir,...})     = libDir
    fun getRuntimeDir (DIRS {runtimeDir,...}) = runtimeDir
    fun getBinDir     (DIRS {binDir,...})     = binDir
    fun getCommDir    (DIRS {commDir,...})    = commDir

    (* runtime, bin, lib : dirs * string -> string *)
    fun runtime (dirs, name) = relative (getRuntimeDir dirs, name)
    fun bin (dirs, name) = relative (getBinDir dirs, name)
    fun lib (dirs, name) = relative (getLibDir dirs, name)

end
