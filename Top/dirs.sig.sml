(*$import TopLevel *)

signature DIRS =
sig
    (* Utilities *)
    
    (* relative (dir, path) is the canonical path p equivalent to dir/path.
     * Note if path is absolute then p = path, and if dir is absolute then so is p.
     *)
    val relative : string * string -> string

    (* accessPath (dirs, perms) path =
     * SOME path' if path' = relative(dir, path) where
     *            path' is accessible with perms,
     *            and dir is the first member of dirs that works.
     * NONE if path is not accessible via any of dirs.
     *)
    val accessPath : string list * OS.FileSys.access_mode list -> string -> string option
	
    (* Compiler directories *)
	
    type dirs

    val getDirs : unit -> dirs		(* Depends on process' environment.  Don't force too early. *)

    val isSystemFile  : dirs * string -> bool
    val stripLibDir   : dirs * string -> string option
	
    val getLibDir     : dirs -> string
    val getRuntimeDir : dirs -> string
    val getBinDir     : dirs -> string
    val getCommDir    : dirs -> string

end

