(*$import Prelude OS *)

signature DIRS =
sig
    (* Utilities *)

    (* dir (path) returns the directory part of the given path.
       if there is no such part, then the empty string is returned. *)
    val dir : string -> string
    
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

    (* Encode/decode path for use on another machine -- possibly with
     * different LIBDIRs, CWDs, etc.
     *)
    val encode : dirs -> string -> string
    val decode : dirs -> string -> string

    val isSystemFile  : dirs * string -> bool
	
    val getLibDir     : dirs -> string
    val getRuntimeDir : dirs -> string
    val getBinDir     : dirs -> string
    val getCommDir    : dirs -> string

    (* Create intermediate directories.  Dirs in the dir_cache are
     * assumed to exist.  Existing or created dirs are added to the
     * dir_cache.  *)
    
    type dir_cache

    val emptyCache : dir_cache
    val createDir : string * dir_cache -> dir_cache
    val createDirs : string list * dir_cache -> dir_cache

end

