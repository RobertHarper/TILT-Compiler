(* One handy source is the gcc "specs" file. *)

structure Tools :> TOOLS =
struct

    val error = fn s => Util.error "tools.sml" s

    val ToolsDiag = Stats.ff("ToolsDiag")
    fun msg str = if (!ToolsDiag) then print str else ()

    val ShowTools = Stats.ff "ShowTools"
    val DebugRuntime = Stats.ff "DebugRuntime"
    val Profile = Stats.ff "Profile"

    fun binFile (path : string) : string =
	let val bindir = IntSyn.F.bindir()
	in  OS.Path.joinDirFile{dir=bindir, file=path}
	end

    fun run' (arg as {command:string list, stdin:string option, stdout:string option}) : unit =
	let val _ =
		if !ShowTools then
		    let val stdin =
			    (case stdin of
				NONE => nil
			    |	SOME file => ["<",file])
			val stdout =
			    (case stdout of
				NONE => nil
			    |	SOME file => [">",file])
			val msg = List.concat[command,stdin,stdout]
			val msg = Listops.concatWith " " msg
		    in	print "running: "; print msg; print "\n"
		    end
		else ()
	in  Util.run arg
	end

    fun run (command:string list list) : unit =
	let val command = List.concat command
	in  run'{command=command, stdin=NONE, stdout=NONE}
	end

    fun sparcas {obj:string, asm:string} : unit =
	let val _ = msg "  Assembling\n"
	    val cmd = binFile "sparcas"
	    fun writer tmp = run[[cmd,tmp,asm]]
	    val _ = Fs.write' writer obj
	in  ()
	end

    fun sparcld {exe:string, objs:string list} : unit =
	let val _ = msg "  Linking\n"
	    val ld = binFile "sparcld"
	    val _ = run[[ld,exe],objs]
	in  ()
	end

    fun inc_args (dirs:string list) : string list =
	let fun folder (dir, args) = "-I" :: dir :: args
	in  foldr folder nil dirs
	end

    fun verify_args (verify:bool) : string list =
	if verify then [] else ["-V"]

    fun talx86as {verify:bool, obj:string, tobj:string, incs:string list, asm:string} : unit =
	let val _ = msg "  Assembling\n"
	    val cmd = binFile "talx86as"
	    val verify = verify_args verify
	    val incs = inc_args incs
	    fun writer tmp1 tmp2 = run[[cmd],verify,incs,[tmp1,tmp2,asm]]
	    val _ = Fs.write2' writer obj tobj
	in  ()
	end

    fun talx86ld {verify:bool, exe:string, incs:string list, objs:string list} : unit =
	let val _ = msg "  Linking\n"
	    val ld = binFile "talx86ld"
	    val verify = verify_args verify
	    val incs = inc_args incs
	    val () = run[[ld],verify,incs,[exe],objs]
	in  ()
	end

    fun compress {src : string, dest : string} : unit =
	let val _ = msg "  Compressing\n"
	    val cmd = binFile "compress"
	    fun writer tmp =
		run'
		    {command=[cmd],
		     stdin=SOME src,
		     stdout=SOME tmp}
	    val _ = Fs.write' writer dest
	in  ()
	end

    fun uncompress {src : string, dest : string} : unit =
	let val _ = msg "  Uncompressing\n"
	    val cmd = binFile "uncompress"
	    fun writer (tmp:string) : unit =
		run'
		    {command = [cmd],
		     stdin = SOME src,
		     stdout = SOME tmp}
	    val _ = Fs.write' writer dest
	in  ()
	end

    val sparcas = Stats.timer("assembling",sparcas)
    val sparcld = Stats.timer("linking",sparcld)
    val talx86as = Stats.timer("assembling",talx86as)
    val talx86ld = Stats.timer("linking",talx86ld)
    val compress = Stats.timer("compressing",compress)
    val uncompress = Stats.timer("uncompressing",uncompress)
end
