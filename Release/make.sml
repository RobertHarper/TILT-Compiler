(* A source release is made via "cvs export".  Some files in the CVS
   tree do not need to be released and are deleted.  *)

fun makeRelease () =
    let
	open Help

	val _ = OS.FileSys.chDir Config.tmpdir

	val tiltRelease : string
	    = "tilt-" ^ Config.release

	val _ = print "Performing CVS export.\n"
	val _ = if OS.FileSys.access (tiltRelease, [])
		    then print "(CVS export unnecessary.)\n"
		else execute Config.cvs ["-Q", "-d", Config.cvsroot, "export",
					 Config.cvsversion, "-d", tiltRelease, Config.cvsmodule]

	fun removeEnt ((ty, path) : ent) : unit =
	    (case ty
	       of FILE => OS.FileSys.remove
		| DIR => OS.FileSys.rmDir) path

	val remove : string -> unit =
	    (app removeEnt) o expand

	val _ = print "Removing unnecessary files.\n"
	val _ = app (remove o (join tiltRelease)) Config.private

	val tarFile : string
	    = ((joinExt "gz") o (joinExt "tar") o (join Config.destdir)) tiltRelease

	val _ = print "Creating tarball.\n"
	val _ = (remove tarFile;
		 execute Config.gnuTar ["czf",tarFile,tiltRelease])
    in  ()
    end

val _ = makeRelease() handle e => Help.fail ("uncaught exception: " ^ exnMessage e)
