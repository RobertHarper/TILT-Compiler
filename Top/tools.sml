(* One handy source is the gcc "specs" file. *)

structure Tools :> TOOLS =
struct

    val error = fn s => Util.error "tools.sml" s

    val ToolsDiag = Stats.ff("ToolsDiag")
    fun msg str = if (!ToolsDiag) then print str else ()

    val ShowTools = Stats.ff "ShowTools"
    val DebugAsm = Stats.tt "DebugAsm"
    val Profile = Stats.ff "Profile"

    type config = {assembler : string list,
		   linker : string list,
		   ldpre : string list,
		   ldpost : string list}

    fun runtimeFile (path : string) : string =
	OS.Path.joinDirFile{dir=Dirs.runtimeDir(), file=path}

    (* chop : string -> string *)
    fun chop "" = ""
      | chop s = String.extract (s, 0, SOME (size s - 1))

    (* gccFile : string -> string *)
    fun gccFile file = chop (Popen.outputOf ("gcc --print-file-name=" ^ file))

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
			  ldpre     = [runtimeFile "obj_solaris/firstdata.o", crt1, gccFile "crti.o",
				       "/usr/ccs/lib/values-Xa.o", gccFile "crtbegin.o"],
			  ldpost    = (["-L/afs/cs/project/fox/member/pscheng/ml96/SparcPerfMon/lib",
					runtimeFile "runtime.solaris.a", "-lperfmon", "-lpthread","-lposix4", "-lgen",
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
			  ldpost    = [runtimeFile "runtime.alpha_osf.a", "-lpthread", "-lmach", "-lexc", "-lm",
				       "-lc", "-lrt"]}
		     end)

    (* targetConfig : unit -> config *)
    fun targetConfig () =
	(case Target.getTargetPlatform()
	   of Target.TIL_ALPHA => alphaConfig()
	    | Target.TIL_SPARC => sparcConfig())

    (* run' : string list -> unit *)
    fun run' nil = ()
      | run' cmd =
	let val command = concat (Listops.join " " cmd)
	    val _ = if (!ShowTools) then (print "  "; print command; print "\n") else ()
	in
	    if Util.system command then ()
	    else error (hd cmd ^ " failed")
	end

    (* run : string list list -> unit *)
    val run = run' o List.concat

    fun assemble (asmFile : string, objFile : string) : unit =
	let val _ = msg "  Assembling\n"
	    val _ = Target.checkNative()
	    val {assembler, ...} = targetConfig()
	    val tmp = Paths.tmpFile objFile
	    val _ = (OS.FileSys.remove objFile) handle _ => ()
	in  (run [assembler, ["-o", tmp, asmFile]];
	     OS.FileSys.rename {old=tmp, new=objFile})
	end

    (*
	We do not have a good temporary name because exeFile is not
	inside a TM directory.
    *)
    fun link (objFiles : string list, exeFile : string) : unit =
	let val _ = msg "  Linking\n"
	    val _ = Target.checkNative()
	    val {linker, ldpre, ldpost, ...} = targetConfig()
	in  run [linker, ["-o", exeFile], ldpre, objFiles, ldpost]
	end

    fun compress {src : string, dest : string} : unit =
	let val _ = msg "  Compressing\n"
	    val tmp = Paths.tmpFile dest
	in  (run' ["gzip","-cq9","<" ^ src,">" ^ tmp];
	     OS.FileSys.rename {old=tmp, new=dest})
	end

    fun uncompress {src : string, dest : string} : unit =
	let val _ = msg "  Uncompressing\n"
	    val tmp = Paths.tmpFile dest
	in  (run' ["gunzip","-cq","<" ^ src,">" ^ tmp];
	     OS.FileSys.rename {old=tmp, new=dest})
	end

end
