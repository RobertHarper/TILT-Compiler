(*$import Help Config OS *)

(* A source release is made via "cvs export".  Some files in the CVS
   tree do not need to be released and are deleted.  *)

open Help
    
val tiltRelease : string
    = "tilt-" ^ Config.release
    
val exportDir : string
    = (join Config.tmpdir) tiltRelease

val _ = print "Performing CVS export.\n"
val _ = if OS.FileSys.access (exportDir, [])
	    then print "(CVS export unnecessary.)\n"
	else execute Config.cvs ["-qq", "-d", Config.cvsroot, "export",
				 Config.cvsversion, "-d", exportDir, Config.cvsmodule]

fun removeEnt ((ty, path) : ent) : unit =
    (case ty
       of FILE => OS.FileSys.remove
	| DIR => OS.FileSys.rmDir) path

val remove : string -> unit =
    (app removeEnt) o expand

val _ = print "Removing unnecessary files.\n"
val _ = app (remove o (join exportDir)) Config.private

val tarFile : string
    = ((joinExt "gz") o (joinExt "tar") o (join Config.destdir)) tiltRelease
    
val _ = print "Creating tarball.\n"
val _ = (OS.FileSys.chDir Config.tmpdir;
	 remove tarFile;
	 execute Config.gnuTar ["czf",tarFile,tiltRelease])
