(*$import Prelude TopLevel OS DIRS Delay Util Platform String Option List SplaySetFn Paths *)

structure Dirs :> DIRS =
struct

    (* error : string -> 'a *)
    val error = fn s => Util.error "dirs.sml" s
	
    val fixslash =
        if (Platform.platform () = Platform.NT) then
          (String.implode o
           (map (fn c => if c = #"/" then #"\\" else c))
           o String.explode)
        else
           fn (s : string) => s

    (* dir : string -> string *)
    fun dir s =
	if (Platform.platform() = Platform.NT) then
           let
	      fun loop [] = ""
                | loop (#"/"::rest) = String.implode(rev rest)
                | loop (#"\\"::rest) = String.implode(rev rest)
                | loop (_::rest) = loop rest
           in
	      loop (List.rev (String.explode s))
           end 
        else
           OS.Path.dir s
	
 
    (* relative : string * string -> string *)
    (* could be smarter about symbolic links *)
    fun relative (dir, file) =
	let val file' = if OS.Path.isRelative file
			    then OS.Path.joinDirFile {dir=dir, file=file}
			else file
	in
            (* need fixslash.  Under Windows, a path with mixed slashes like
                  foo/bar\..\baz
               canonicalizes into baz rather than foo\baz because
               unlike the actual OS, OS.Path.mkCanonical doesn't realize
               that / is a path separator. *)
	    OS.Path.mkCanonical (fixslash file')
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
			     libDir : string,
			     runtimeDir : string,
			     binDir : string,
			     commDir : string Delay.value}

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
    fun dirAvailable dir = 
	let
	  val permissions = if (Platform.platform() = Platform.NT) then
	                       [OS.FileSys.A_READ]
                            else
	                       [OS.FileSys.A_READ, OS.FileSys.A_EXEC]
        in
	   (OS.FileSys.isDir dir andalso 
            OS.FileSys.access(dir, permissions))
           handle _ => false
        end

    (* chopSlash : string -> string *)
    val chopSlash = 
        if (Platform.platform() = Platform.NT) then
           fn s => if size s > 0 andalso 
                      ((String.sub (s, 0) = #"/") orelse
                       (String.sub (s, 0) = #"\\"))
			  then String.extract (s, 1, NONE)
		      else s
        else
           fn s => if size s > 0 andalso (String.sub (s, 0) = #"/")
			  then String.extract (s, 1, NONE)
		      else s


    (* strip : string * string -> string option *)
    fun strip (dir, file) = if inDirByName (dir, file)
				then SOME (chopSlash (String.extract (file, size dir, NONE)))
			    else if OS.Path.isRelative file
				     then strip (dir, relative (Delay.force cwd, file))
				 else NONE

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
(*	    Not necessary.  Use "tilt -v [stuff]" *)
(*	    val _ = (print "Expanded TILT_LIBDIR is "; print dir; print "\n")*)
	    val sys = sysDir dir
	    fun system file = sys andalso inDir (dir, file)
	    val curDir = Delay.force cwd
	    val commDirs = Paths.commDirs curDir
	    fun join file = relative (dir, file)		
	in
	    DIRS {system=system, libDir=dir,
		  runtimeDir=join "Runtime", binDir=join "Bin",
		  commDir=Delay.delay (fn () => (app mkdir commDirs; List.last commDirs))}
	end

    val dirs : dirs Delay.value = (* Memoize - not strictly necessary *)
	Delay.delay (fn () =>
		     case OS.Process.getEnv "TILT_LIBDIR"
		       of NONE => from "" (* or OS.FileSys.getDir() *)
			| SOME dir => from dir)
	
    (* getDirs : unit -> dirs *)
    fun getDirs () = Delay.force dirs
	
    (* encode/decode necessary because LibDir is not fixed *)
    (* decode should probably do a path search *)

    (* encode : dirs -> string -> string *)
    fun encode (DIRS {libDir, ...}) file =
	case strip (libDir, file)
	  of NONE => "U" ^ file
	   | SOME file' => "L" ^ file'
	      
    (* decode : dirs -> string -> string *)
    fun decode (DIRS {libDir, ...}) code =
	let val file = String.extract (code, 1, NONE)
	in
	    case String.sub (code, 0)
	      of #"L" => relative (libDir, file)
	       | #"U" => file
	end
	      
    (* isSystemFile : dirs * string -> bool *)
    fun isSystemFile (DIRS {system,...}, file) = system file
	
    (* get*Dir : dirs -> string *)
    fun getLibDir     (DIRS {libDir,...})     = libDir
    fun getRuntimeDir (DIRS {runtimeDir,...}) = runtimeDir
    fun getBinDir     (DIRS {binDir,...})     = binDir
    fun getCommDir    (DIRS {commDir,...})    = Delay.force commDir
	
    structure StringSet = SplaySetFn (type ord_key = string
				      val compare = String.compare)
	
    type dir_cache = StringSet.set

    val emptyCache = StringSet.empty

    (* createDir : string * dir_cache -> dir_cache *)
    fun createDir (dir, set) =
	if StringSet.member (set, dir) then set
	else (mkdir dir; StringSet.add (set, dir))

    (* createDirs : string list * dir_cache -> dir_cache *)
    fun createDirs (dirs, set) = foldl createDir set dirs
	
end
