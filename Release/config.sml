(*$import Help Date Time CommandLine *)

signature CONFIG =
sig
    (* Absolute paths to the binaries we need. *)
    val cvs : string
    val gnuTar : string
	      
    (* Temporary space. *)
    val tmpdir : string
	      
    val destdir : string		(* Where to store files *)
    val release : string		(* How to name files *)
	      
    (* Where to find TILT in CVS. *)
    val cvsroot : string
    val cvsmodule : string
    val cvsversion : string

    (* Files and directories that may exist in CVS but do not need to
       be released. *)
    val private : string list
end

structure Config : CONFIG =
struct
    val cvs = "/usr/local/bin/cvs"
    val gnuTar = "/usr/local/bin/gtar"
	
    val tmpdir = "/usr/tmp"

    val destdir = tmpdir
	
    (* Base release tag on today's date; eg, 20020514. *)
    val release = Date.fmt "%Y%m%d" (Date.fromTimeUniv (Time.now()))

    (* Get repository and version from command line. *)
    val (cvsroot, cvsversion) =
	(case CommandLine.arguments()
	   of [a,b] => (a,b)
	    | _ => Help.fail "expected CVS root and module version (eg, -Ddate or -rtag) as arguments")
    val cvsmodule = "ml96"

    val private =
	[
	 "Apps","Bench","BenchData",
	 "Bin/makeRelease",
	 "Bin/run",
	 "Doc/buglist.txt",
	 "Doc/issue.txt",
	 "Doc/log.txt",
	 "Doc/overview.txt",		(* needs updating *)
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
