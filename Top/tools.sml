(* One handy source is the gcc "specs" file. *)

structure Tools :> TOOLS =
struct

    val error = fn s => Util.error "tools.sml" s

    val ToolsDiag = Stats.ff("ToolsDiag")
    fun msg str = if (!ToolsDiag) then print str else ()

    val ShowTools = Stats.ff "ShowTools"
    val DebugAsm = Stats.tt "DebugAsm"
    val DebugRuntime = Stats.ff "DebugRuntime"
    val Profile = Stats.ff "Profile"

    fun release () : string =
	if !DebugRuntime then "dbg" else "opt"

    type config = {assembler : string list,
		   linker : string list,
		   ldpre : string list,
		   ldpost : string list}

    fun runtimeFile (path : string) : string =
	let val runtimedir = IntSyn.F.runtimedir()
	in  OS.Path.joinDirFile{dir=runtimedir, file=path}
	end

    fun chop (s:string) : string =
	if size s = 0 then s
	else String.extract (s, 0, SOME (size s - 1))

    fun gccFile (file:string) : string =
	chop (Util.outputOf ["gcc","--print-file-name="^file])

    val sparcConfig : unit -> config =
	Util.memoize(fn () =>
		     let
			 val libc = if !Profile
					then "/usr/lib/libp/libc.a"
				    else "-lc"

			 val libm = "-lm" (* note: /usr/lib/libp/libm.a leads to ldd.so errors on cuff *)

			 val crt1 =
			     if !Profile then
				 (* Hack so we can profile on cuff. *)
				 (case gccFile "mcrt1.o"
				    of "mcrt1.o" => "/afs/cs/project/fox/member/swasey/ml96/Runtime/mcrt1.o"
				     | s => s)
			     else gccFile "crt1.o"

			 val libdl = if !Profile
					 then ["-ldl"]
				     else []
		     in
			 {assembler = ["/usr/ccs/bin/as", "-xarch=v8plus"],
			  linker    = ["/usr/ccs/bin/ld"],
			  ldpre     = [runtimeFile "sparc/"^release()^"/firstdata.o", crt1, gccFile "crti.o",
				       "/usr/ccs/lib/values-Xa.o", gccFile "crtbegin.o"],
			  ldpost    = (["-L/afs/cs/project/fox/member/pscheng/ml96/SparcPerfMon/lib",
					runtimeFile "runtime-sparc-"^release()^".a", "-lperfmon", "-lpthread","-lposix4", "-lgen",
					libm, libc, gccFile "libgcc.a", gccFile "crtend.o", gccFile "crtn.o"] @
				       libdl)}
		     end)


    val alphaConfig : unit -> config =
	Util.memoize(fn() =>
		     let
			 val debug = if (!DebugAsm) then ["-g"] else nil
			 val crt = if (!Profile)
				       then ["/usr/lib/cmplrs/cc/mcrt0.o",
					     "/usr/lib/cmplrs/cc/libprof1_r.a",
					     "/usr/lib/cmplrs/cc/libpdf.a"]
				   else ["/usr/lib/cmplrs/cc/crt0.o"]
		     in
			 {assembler = ["/usr/bin/as"] @ debug,
			  linker    = ["/usr/bin/ld", "-call_shared", "-D", "a000000", "-T", "8000000"] @ debug,
			  ldpre     = crt,
			  ldpost    = [runtimeFile "runtime-alpha-"^release()^".a", "-lpthread", "-lmach", "-lexc", "-lm",
				       "-lc", "-lrt"]}
		     end)

    (* I'm not sure about all the options yet.  *)
    val talx86Config : unit -> config =
	Util.memoize(fn () =>
		     let
		     in
			 {assembler = ["talc.exe", "-c", "--verify-link", "--verify-program"],
			  linker    = ["talc.exe", "--verify-link", "--verify-program"],
			  ldpre     = [],
			  ldpost    = []  (* Obviously, these are not yet complete *)
			  }
		     end)


    fun targetConfig () : config =
	(case Target.getTarget()
	   of Platform.ALPHA => alphaConfig()
	    | Platform.SPARC => sparcConfig()
	    | Platform.TALx86 => talx86Config())

    fun run' (command:string list) : unit =
	Util.run {command=command, stdin=NONE, stdout=NONE, wait=true}

    val run : string list list -> unit =
	run' o List.concat

    fun assemble (tal_includes : string list, asmFile : string, objFile : string) : unit =
	let val _ = msg "  Assembling\n"
	    val _ = Target.checkNative()
	    val {assembler, ...} = targetConfig()
	    fun writer tmp = run [assembler,tal_includes,["-o", tmp, asmFile]]
	in  Fs.write' writer objFile
	end

    (*
	We do not have a good temporary name because exeFile is not
	inside a TM directory.
    *)
    fun link (tal_includes : string list, objFiles : string list, exeFile : string) : unit =
	let val _ = msg "  Linking\n"
	    val _ = Target.checkNative()
	    val {linker, ldpre, ldpost, ...} = targetConfig()
	in  run [linker, tal_includes, ["-o", exeFile], ldpre, objFiles, ldpost]
	end

    fun compress {src : string, dest : string} : unit =
	let val _ = msg "  Compressing\n"
	    fun writer (tmp:string) : unit =
		Util.run
		    {command = ["gzip","-cq9"],
		     stdin = SOME src,
		     stdout = SOME tmp,
		     wait = true}
	in  Fs.write' writer dest
	end

    fun uncompress {src : string, dest : string} : unit =
	let val _ = msg "  Uncompressing\n"
	    fun writer (tmp:string) : unit =
		Util.run
		    {command = ["gunzip", "-cq"],
		     stdin = SOME src,
		     stdout = SOME tmp,
		     wait = true}
	in  Fs.write' writer dest
	end

    val assemble = Stats.timer("assembling",assemble)
    val link = Stats.timer("linking",link)
    val compress = Stats.timer("compressing",compress)
    val uncompress = Stats.timer("uncompressing",uncompress)
end
