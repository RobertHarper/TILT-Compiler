(*$import Prelude TopLevel Delay String List TOOLS TopHelp Util Stats OS Dirs Target Popen *)

(* One handy source is the gcc "specs" file. *)

structure Tools :> TOOLS =
struct

    val error = fn s => Util.error "tools.sml" s

    val showTools = Stats.ff "ShowTools"
    val debugAsm = Stats.tt "debug_asm"
    val profile = Stats.ff "prof"
	
    type config = {assembler : string list,
		   linker : string list,
		   ldpre : string list,
		   ldpost : string list}

    (* runtimeFile : string -> string *)
    fun runtimeFile path = Dirs.relative (Dirs.getRuntimeDir (Dirs.getDirs()), path)
	
    val gccPath : string Delay.value =
	Delay.delay (fn () =>
		     let val gcc = "gcc"
			 val path = ["/usr/local/bin", "/usr/bin"]
			 val which = Dirs.accessPath (path, [OS.FileSys.A_EXEC])
		     in
			 case which gcc
			   of NONE => gcc
			    | SOME gcc' => gcc'
		     end)

    (* chop : string -> string *)
    fun chop "" = ""
      | chop s = String.extract (s, 0, SOME (size s - 1))

    (* gccFile : string -> string *)
    fun gccFile file = chop (Popen.outputOf (Delay.force gccPath ^ " --print-file-name=" ^ file))

    val sparcConfig : config Delay.value =
	Delay.delay (fn () =>
		     let
			 val crt = if (!profile) then "mcrt1.o" else "crt1.o"
		     in
			 {assembler = ["/usr/ccs/bin/as", "-xarch=v8plus"],
			  linker    = ["/usr/ccs/bin/ld"],
			  ldpre     = [runtimeFile "obj_solaris/firstdata.o",
				       gccFile crt, gccFile "crti.o",
				       "/usr/ccs/lib/values-Xa.o", gccFile "crtbegin.o"],
			  ldpost    = ["-L/afs/cs/project/fox/member/pscheng/ml96/SparcPerfMon/lib", 
				       runtimeFile "runtime.solaris.a", "-lperfmon", "-lpthread","-lposix4", "-lgen",
				       "-lm", "-lc", gccFile "libgcc.a", gccFile "crtend.o", gccFile "crtn.o"]}
		     end)
    val alphaConfig : config Delay.value =
	Delay.delay (fn() =>
		     let
			 val debug = if (!debugAsm) then ["-g"] else nil
			 val crt = if (!profile)
				       then ["/usr/lib/cmplrs/cc/mcrt0.o",
					     "/usr/lib/cmplrs/cc/libprof1_r.a",
					     "/usr/lib/cmplrs/cc/libpdf.a"]
				   else ["/usr/lib/cmplrs/cc/crt0.o"]
		     in
			 {assembler = ["/usr/bin/as"] @ debug,
			  linker    = ["/usr/bin/ld", "-call_shared", "-D", "a000000", "-T", "8000000"] @ debug,
			  ldpre     = crt,
			  ldpost    = [runtimeFile "runtime.alpha_osf.a", "-lpthread", "-lmach", "-lexc", "-lm", "-lc", "-lrt"]}
		     end)

    (* targetConfig : unit -> config *)
    fun targetConfig () =
	Delay.force (case Target.getTargetPlatform()
		       of Target.TIL_ALPHA => alphaConfig
			| Target.TIL_SPARC => sparcConfig)
    
    (* run' : string list -> unit *)
    fun run' nil = ()
      | run' (cmd :: args) =
	let val command = List.foldr (fn (a,b) => a ^ " " ^ b) "" (cmd::args)
	    val _ = if (!showTools) then (print "Running: "; print command; print "\n") else ()
	in
	    if Util.system command then ()
	    else error (cmd ^ " failed")
	end
    
    (* run : string list list -> unit *)
    val run = run' o List.concat

    (* assemble : string * string -> unit *)
    fun assemble (asmFile, objFile) =
	let val _ = Target.checkNative()
	    val {assembler, ...} = targetConfig()
	in
	    run [assembler, ["-o", objFile, asmFile]]
	end
    
    (* link : string list * string -> unit *)
    fun link (objFiles, exeFile) =
	let val _ = Target.checkNative()
	    val {linker, ldpre, ldpost, ...} = targetConfig()
	in
	    run [linker, ["-o", exeFile], ldpre, objFiles, ldpost]
	end

end
